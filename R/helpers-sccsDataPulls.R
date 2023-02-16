#' Get expsoure outcome pairs set for sccs module
getSccsExposuresOutcomes <- function(connectionHandler, resultDatabaseSettings, includeControls = FALSE) {
  # Note Query rew-written from dplyr because of data type/casting issues with null values in left joins
  sql <- "
  SELECT
    eos.*,
    e.era_id as exposure_id,
    e.true_effect_size,
    cd.cohort_name as outcome_name,
    CASE
      WHEN eras.era_name IS NULL THEN cd2.cohort_name
      ELSE eras.era_name
    END as exposure_name
  FROM @database_schema.@table_prefixsccs_exposures_outcome_set eos
  INNER JOIN @database_schema.@table_prefixsccs_exposure e ON e.exposures_outcome_set_id = eos.exposures_outcome_set_id
  INNER JOIN @database_schema.@cg_table_prefixcohort_definition cd ON cd.cohort_definition_id = eos.outcome_id
  LEFT JOIN (
    SELECT DISTINCT exposures_outcome_set_id, era_id, era_name
    FROM @database_schema.@table_prefixsccs_era
  ) eras ON e.era_id = eras.era_id AND eos.exposures_outcome_set_id = eras.exposures_outcome_set_id
  LEFT JOIN  @database_schema.@cg_table_prefixcohort_definition cd2 ON cd2.cohort_definition_id = e.era_id
  "
  exposureOutcomes <- connectionHandler$queryDb(sql,
                                                database_schema = resultDatabaseSettings$schema,
                                                cg_table_prefix = resultDatabaseSettings$cohortTablePrefix,
                                                table_prefix = resultDatabaseSettings$tablePrefix,
                                                snakeCaseToCamelCase = TRUE)

  if (!includeControls) {
    exposureOutcomes <- exposureOutcomes %>%
      dplyr::filter(is.na(.data$trueEffectSize))
  }
  exposureOutcomes
}

getSccsResults <- function(connectionHandler,
                           resultDatabaseSettings,
                           exposuresOutcomeSetId,
                           databaseIds,
                           analysisIds) {
  sql <- "
  SELECT
  sr.*,
  sds.*,
  sc.*
  FROM @database_schema.@table_prefixsccs_result sr
  INNER JOIN @database_schema.@table_prefixsccs_diagnostics_summary sds ON (
    sds.exposures_outcome_set_id = sr.exposures_outcome_set_id AND
    sds.database_id = sr.database_id AND
    sds.analysis_id = sr.analysis_id AND
    sds.covariate_id = sr.covariate_id
  )
  INNER JOIN @database_schema.@table_prefixsccs_covariate sc ON (
    sc.exposures_outcome_set_id = sr.exposures_outcome_set_id AND
    sc.database_id = sr.database_id AND
    sc.analysis_id = sr.analysis_id AND
    sc.covariate_id = sr.covariate_id
  )

  WHERE sr.analysis_id IN (@analysis_ids)
  AND sr.database_id IN (@database_ids)
  {@exposures_outcome_set_id != ''} ? {AND sr.exposures_outcome_set_id = @exposures_outcome_set_id}
  "

  results <- connectionHandler$queryDb(sql,
                                       database_schema = resultDatabaseSettings$schema,
                                       table_prefix = resultDatabaseSettings$tablePrefix,
                                       exposures_outcome_set_id = exposuresOutcomeSetId,
                                       database_ids = databaseIds,
                                       analysis_ids = analysisIds,
                                       snakeCaseToCamelCase = TRUE)

  return(results)
}

getModel <- function(connectionHandler,
                     resultsDatabaseSchema,
                     exposuresOutcomeSetId,
                     databaseId,
                     analysisId) {


  covariateResult <- connectionHandler$tbl(paste0(resultsDatabaseSchema, ".", "sccs_covariate_result"))
  covariate <- connectionHandler$tbl(paste0(resultsDatabaseSchema, ".", "sccs_covariate"))
  era <- connectionHandler$tbl(paste0(resultsDatabaseSchema, ".", "sccs_era"))
  cohortDefinition <- connectionHandler$tbl(paste0(resultsDatabaseSchema, ".", "cg_cohort_definition"))

  covariateResult %>%
    dplyr::inner_join(covariate, by = c("analysis_id", "exposures_outcome_set_id", "database_id", "covariate_id")) %>%
    dplyr::filter(
      .data$exposures_outcome_set_id == exposuresOutcomeSetId,
      .data$database_id == !!databaseId,
      .data$analysis_id == !!analysisId,
      !is.na(rr)
    ) %>%
    dplyr::left_join(era, by = c("exposures_outcome_set_id", "analysis_id", "era_id", "database_id")) %>%
    dplyr::left_join(cohortDefinition %>%
                       dplyr::select(era_id = "cohort_definition_id",
                                     era_name_2 = "cohort_name"),
                     by = "era_id") %>%
    dplyr::mutate(exposure_name = if_else(is.na(.data$era_name), .data$era_name_2, .data$era_name)) %>%
    dplyr::mutate(covariate_name = if_else(is.na(.data$era_name), .data$covariate_name, paste(.data$covariate_name, .data$era_name, sep = ": "))) %>%
    dplyr::select(
      "covariate_id",
      "covariate_name",
      "rr",
      "ci_95_lb",
      "ci_95_ub") %>%
    dplyr::collect() %>%
    SqlRender::snakeCaseToCamelCaseNames() %>%
    return()
}

getTimeTrend <- function(connectionHandler,
                         resultsDatabaseSchema,
                         exposuresOutcomeSetId,
                         databaseId,
                         analysisId) {
  timeTrend <- connectionHandler$tbl(paste0(resultsDatabaseSchema, ".", "sccs_time_trend"))
  timeTrend %>%
    dplyr::filter(
      .data$exposures_outcome_set_id == exposuresOutcomeSetId,
      .data$database_id == !!databaseId,
      .data$analysis_id == !!analysisId,
    ) %>%
    dplyr::collect() %>%
    SqlRender::snakeCaseToCamelCaseNames() %>%
    return()
}

getTimeToEvent <- function(connectionHandler,
                           resultsDatabaseSchema,
                           exposuresOutcomeSetId,
                           eraId,
                           covariateId,
                           databaseId,
                           analysisId) {
  diagnosticsSummary <- connectionHandler$tbl(paste0(resultsDatabaseSchema, ".", "sccs_diagnostics_summary"))
  p <- diagnosticsSummary %>%
    dplyr::filter(
      exposures_outcome_set_id == exposuresOutcomeSetId,
      covariate_id == !!covariateId,
      database_id == !!databaseId,
      analysis_id == !!analysisId,
    ) %>%
    dplyr::pull("pre_exposure_p")

  timeToEvent <- connectionHandler$tbl(paste0(resultsDatabaseSchema, ".", "sccs_time_to_event"))
  timeToEvent %>%
    dplyr::filter(
      .data$exposures_outcome_set_id == exposuresOutcomeSetId,
      .data$era_id == !!eraId,
      .data$database_id == !!databaseId,
      .data$analysis_id == !!analysisId,
    ) %>%
    dplyr::collect() %>%
    SqlRender::snakeCaseToCamelCaseNames() %>%
    dplyr::mutate(p = !!p) %>%
    return()
}

getAttrition <- function(connectionHandler,
                         resultsDatabaseSchema,
                         exposuresOutcomeSetId,
                         databaseId,
                         analysisId,
                         covariateId) {
  attrition <- connectionHandler$tbl(paste0(resultsDatabaseSchema, ".", "sccs_attrition"))
  attrition %>%
    dplyr::filter(
      .data$exposures_outcome_set_id == exposuresOutcomeSetId,
      .data$database_id == !!databaseId,
      .data$analysis_id == !!analysisId,
      .data$covariate_id == !!covariateId
    ) %>%
    dplyr::collect() %>%
    SqlRender::snakeCaseToCamelCaseNames() %>%
    return()
}

getEventDepObservation <- function(connectionHandler,
                                   resultsDatabaseSchema,
                                   exposuresOutcomeSetId,
                                   databaseId,
                                   analysisId) {
  eventDepObservation <- connectionHandler$tbl(paste0(resultsDatabaseSchema, ".", "sccs_event_dep_observation"))
  eventDepObservation %>%
    dplyr::filter(
      .data$exposures_outcome_set_id == exposuresOutcomeSetId,
      .data$database_id == !!databaseId,
      .data$analysis_id == !!analysisId
    ) %>%
    dplyr::collect() %>%
    SqlRender::snakeCaseToCamelCaseNames() %>%
    return()
}

getAgeSpanning <- function(connectionHandler,
                           resultsDatabaseSchema,
                           exposuresOutcomeSetId,
                           databaseId,
                           analysisId) {
  ageSpanning <- connectionHandler$tbl(paste0(resultsDatabaseSchema, ".", "sccs_age_spanning"))
  ageSpanning %>%
    dplyr::filter(
      .data$exposures_outcome_set_id == exposuresOutcomeSetId,
      .data$database_id == !!databaseId,
      .data$analysis_id == !!analysisId
    ) %>%
    dplyr::collect() %>%
    SqlRender::snakeCaseToCamelCaseNames() %>%
    return()
}

getCalendarTimeSpanning <- function(connectionHandler,
                                    resultsDatabaseSchema,
                                    exposuresOutcomeSetId,
                                    databaseId,
                                    analysisId) {
  calendarTimeSpanning <- connectionHandler$tbl(paste0(resultsDatabaseSchema, ".", "sccs_calendar_time_spanning"))
  calendarTimeSpanning %>%
    dplyr::filter(
      .data$exposures_outcome_set_id == exposuresOutcomeSetId,
      .data$database_id == !!databaseId,
      .data$analysis_id == !!analysisId
    ) %>%
    dplyr::collect() %>%
    SqlRender::snakeCaseToCamelCaseNames() %>%
    return()
}

getSpline <- function(connectionHandler,
                      resultsDatabaseSchema,
                      exposuresOutcomeSetId,
                      databaseId,
                      analysisId,
                      splineType = "age") {
  spline <- connectionHandler$tbl(paste0(resultsDatabaseSchema, ".", "sccs_spline"))
  spline %>%
    dplyr::filter(
      .data$exposures_outcome_set_id == exposuresOutcomeSetId,
      .data$database_id == !!databaseId,
      .data$analysis_id == !!analysisId,
      .data$spline_type == splineType
    ) %>%
    dplyr::collect() %>%
    SqlRender::snakeCaseToCamelCaseNames() %>%
    return()
}


getControlEstimates <- function(connectionHandler,
                                resultsDatabaseSchema,
                                exposuresOutcomeSetId,
                                databaseId,
                                analysisId,
                                covariateId) {


  sccsResult <- connectionHandler$tbl(paste0(resultsDatabaseSchema, ".", "sccs_result"))
  sccsExposure <- connectionHandler$tbl(paste0(resultsDatabaseSchema, ".", "sccs_exposure"))
  sccsCovariate <- connectionHandler$tbl(paste0(resultsDatabaseSchema, ".", "sccs_covariate"))

  sccsResult %>%
    dplyr::inner_join(sccsCovariate, by = c("analysis_id", "exposures_outcome_set_id", "covariate_id", "database_id")) %>%
    dplyr::inner_join(sccsExposure, by = c("exposures_outcome_set_id", "era_id")) %>%
    dplyr::filter(
      .data$database_id == !!databaseId,
      .data$analysis_id == !!analysisId,
      .data$covariate_id == !!covariateId,
      !is.na(.data$true_effect_size)
    ) %>%
    dplyr::select("ci_95_lb", "ci_95_ub", "log_rr", "se_log_rr", "calibrated_ci_95_lb", "calibrated_ci_95_ub", "calibrated_log_rr", "calibrated_se_log_rr", "true_effect_size") %>%
    dplyr::collect() %>%
    SqlRender::snakeCaseToCamelCaseNames() %>%
    return()
}

getDiagnosticsSummary <- function(connectionHandler,
                                  resultsDatabaseSchema,
                                  exposuresOutcomeSetId,
                                  databaseId,
                                  analysisId,
                                  covariateId) {
  diagnosticsSummary <- connectionHandler$tbl(paste0(resultsDatabaseSchema, ".", "sccs_diagnostics_summary"))
  diagnosticsSummary %>%
    dplyr::filter(
      .data$exposures_outcome_set_id == exposuresOutcomeSetId,
      .data$database_id == !!databaseId,
      .data$analysis_id == !!analysisId,
      .data$covariate_id == !!covariateId
    ) %>%
    dplyr::collect() %>%
    SqlRender::snakeCaseToCamelCaseNames() %>%
    return()
}