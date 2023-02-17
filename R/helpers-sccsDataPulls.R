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
  FROM @database_schema.@table_prefixexposures_outcome_set eos
  INNER JOIN @database_schema.@table_prefixexposure e ON e.exposures_outcome_set_id = eos.exposures_outcome_set_id
  INNER JOIN @database_schema.@cg_table_prefixcohort_definition cd ON cd.cohort_definition_id = eos.outcome_id
  LEFT JOIN (
    SELECT DISTINCT exposures_outcome_set_id, era_id, era_name
    FROM @database_schema.@table_prefixera
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
  ds.cdm_source_abbreviation as database_name,
  sds.mdrr,
  sds.ease,
  sds.time_trend_p,
  sds.pre_exposure_p,
  sds.mdrr_diagnostic,
  sds.ease_diagnostic,
  sds.time_trend_diagnostic,
  sds.pre_exposure_diagnostic,
  sds.unblind,
  sc.covariate_name,
  sc.era_id,
  sc.covariate_analysis_id
  FROM @database_schema.@table_prefixresult sr
  INNER JOIN @database_schema.@database_table_prefix@database_table ds ON sr.database_id = ds.database_id
  INNER JOIN @database_schema.@table_prefixdiagnostics_summary sds ON (
    sds.exposures_outcome_set_id = sr.exposures_outcome_set_id AND
    sds.database_id = sr.database_id AND
    sds.analysis_id = sr.analysis_id AND
    sds.covariate_id = sr.covariate_id
  )
  INNER JOIN @database_schema.@table_prefixcovariate sc ON (
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
                                       database_table_prefix = resultDatabaseSettings$databaseTablePrefix,
                                       database_table = resultDatabaseSettings$databaseTable,
                                       table_prefix = resultDatabaseSettings$tablePrefix,
                                       exposures_outcome_set_id = exposuresOutcomeSetId,
                                       database_ids = quoteLiterals(databaseIds),
                                       analysis_ids = analysisIds,
                                       snakeCaseToCamelCase = TRUE)

  return(results)
}

getSccsModel <- function(connectionHandler,
                         resultDatabaseSettings,
                         exposuresOutcomeSetId,
                         databaseId,
                         analysisId) {
  sql <- "
  SELECT
    CASE
       WHEN era.era_name IS NULL THEN sc.covariate_name
       ELSE CONCAT(sc.covariate_name, ' : ', era.era_name)
    END as covariate_name,
    scr.covariate_id, scr.rr, scr.ci_95_lb, scr.ci_95_ub
  FROM @database_schema.@table_prefixcovariate_result scr
  INNER JOIN @database_schema.@table_prefixcovariate sc ON (
    sc.exposures_outcome_set_id = scr.exposures_outcome_set_id AND
    sc.database_id = scr.database_id AND
    sc.analysis_id = scr.analysis_id AND
    sc.covariate_id = scr.covariate_id
  )
  LEFT JOIN @database_schema.@cg_table_prefixcohort_definition cd ON cd.cohort_definition_id = sc.era_id
  LEFT JOIN @database_schema.@table_prefixera era ON (
    era.exposures_outcome_set_id = scr.exposures_outcome_set_id AND
    era.database_id = scr.database_id AND
    era.analysis_id = scr.analysis_id AND
    era.era_id = sc.era_id
  )
  WHERE scr.database_id = '@database_id'
  AND scr.analysis_id = @analysis_id
  AND scr.exposures_outcome_set_id = @exposures_outcome_set_id
  AND scr.rr IS NOT NULL
  "

  connectionHandler$queryDb(sql,
                            database_schema = resultDatabaseSettings$schema,
                            table_prefix = resultDatabaseSettings$tablePrefix,
                            cg_table_prefix = resultDatabaseSettings$cgTablePrefix,
                            exposures_outcome_set_id = exposuresOutcomeSetId,
                            database_id = databaseId,
                            analysis_id = analysisId,
                            snakeCaseToCamelCase = TRUE)
}

getSccsTimeTrend <- function(connectionHandler,
                             resultDatabaseSettings,
                             exposuresOutcomeSetId,
                             databaseId,
                             analysisId) {
  sql <- "
  SELECT *
  FROM @database_schema.@table_prefixtime_trend
  WHERE database_id = '@database_id'
  AND analysis_id = @analysis_id
  AND exposures_outcome_set_id = @exposures_outcome_set_id
  "
  connectionHandler$queryDb(sql,
                            database_schema = resultDatabaseSettings$schema,
                            table_prefix = resultDatabaseSettings$tablePrefix,
                            database_id = databaseId,
                            analysis_id = analysisId,
                            exposures_outcome_set_id = exposuresOutcomeSetId,
                            snakeCaseToCamelCase = TRUE)
}

getSccsTimeToEvent <- function(connectionHandler,
                               resultsDatabaseSettings,
                               exposuresOutcomeSetId,
                               eraId,
                               covariateId,
                               databaseId,
                               analysisId) {
  diagnosticsSummary <- connectionHandler$tbl(paste0(resultsDatabaseSettings$schema,
                                                     ".", resultsDatabaseSettings$tablePrefix, "diagnostics_summary"))
  p <- diagnosticsSummary %>%
    dplyr::filter(
      exposures_outcome_set_id == exposuresOutcomeSetId,
      covariate_id == !!covariateId,
      database_id == !!databaseId,
      analysis_id == !!analysisId,
    ) %>%
    dplyr::pull("pre_exposure_p")

  timeToEvent <- connectionHandler$tbl(paste0(resultsDatabaseSettings$schema,
                                              ".", resultsDatabaseSettings$tablePrefix, "time_to_event"))
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

getSccsAttrition <- function(connectionHandler,
                             resultDatabaseSettings,
                             exposuresOutcomeSetId,
                             databaseId,
                             analysisId,
                             covariateId) {
  sql <- "
  SELECT *
  FROM @database_schema.@table_prefixattrition
  WHERE database_id = '@database_id'
  AND analysis_id = @analysis_id
  AND exposures_outcome_set_id = @exposures_outcome_set_id
  AND covariate_id = @covariate_id
  "
  connectionHandler$queryDb(sql,
                            database_schema = resultDatabaseSettings$schema,
                            table_prefix = resultDatabaseSettings$tablePrefix,
                            database_id = databaseId,
                            analysis_id = analysisId,
                            exposures_outcome_set_id = exposuresOutcomeSetId,
                            covariate_id = covariateId,
                            snakeCaseToCamelCase = TRUE)
}

getSccsEventDepObservation <- function(connectionHandler,
                                       resultDatabaseSettings,
                                       exposuresOutcomeSetId,
                                       databaseId,
                                       analysisId) {
  sql <- "
  SELECT *
  FROM @database_schema.@table_prefixevent_dep_observation
  WHERE database_id = '@database_id'
  AND analysis_id = @analysis_id
  AND exposures_outcome_set_id = @exposures_outcome_set_id
  "
  connectionHandler$queryDb(sql,
                            database_schema = resultDatabaseSettings$schema,
                            table_prefix = resultDatabaseSettings$tablePrefix,
                            database_id = databaseId,
                            analysis_id = analysisId,
                            exposures_outcome_set_id = exposuresOutcomeSetId,
                            snakeCaseToCamelCase = TRUE)
}

getSccsAgeSpanning <- function(connectionHandler,
                               resultDatabaseSettings,
                               exposuresOutcomeSetId,
                               databaseId,
                               analysisId) {
  sql <- "
  SELECT *
  FROM @database_schema.@table_prefixage_spanning
  WHERE database_id = '@database_id'
  AND analysis_id = @analysis_id
  AND exposures_outcome_set_id = @exposures_outcome_set_id
  "
  connectionHandler$queryDb(sql,
                            database_schema = resultDatabaseSettings$schema,
                            table_prefix = resultDatabaseSettings$tablePrefix,
                            database_id = databaseId,
                            analysis_id = analysisId,
                            exposures_outcome_set_id = exposuresOutcomeSetId,
                            snakeCaseToCamelCase = TRUE)
}

getSccsCalendarTimeSpanning <- function(connectionHandler,
                                        resultDatabaseSettings,
                                        exposuresOutcomeSetId,
                                        databaseId,
                                        analysisId) {
  sql <- "
  SELECT *
  FROM @database_schema.@table_prefixcalendar_time_spanning
  WHERE database_id = '@database_id'
  AND analysis_id = @analysis_id
  AND exposures_outcome_set_id = @exposures_outcome_set_id
  "
  connectionHandler$queryDb(sql,
                            database_schema = resultDatabaseSettings$schema,
                            table_prefix = resultDatabaseSettings$tablePrefix,
                            database_id = databaseId,
                            analysis_id = analysisId,
                            exposures_outcome_set_id = exposuresOutcomeSetId,
                            snakeCaseToCamelCase = TRUE)
}

getSccsSpline <- function(connectionHandler,
                          resultDatabaseSettings,
                          exposuresOutcomeSetId,
                          databaseId,
                          analysisId,
                          splineType = "age") {

  sql <- "
  SELECT *
  FROM @database_schema.@table_prefixspline
  WHERE database_id = '@database_id'
  AND analysis_id = @analysis_id
  AND exposures_outcome_set_id = @exposures_outcome_set_id
  AND spline_type = '@spline_type'
  "
  connectionHandler$queryDb(sql,
                            database_schema = resultDatabaseSettings$schema,
                            table_prefix = resultDatabaseSettings$tablePrefix,
                            database_id = databaseId,
                            spline_type = splineType,
                            analysis_id = analysisId,
                            exposures_outcome_set_id = exposuresOutcomeSetId,
                            snakeCaseToCamelCase = TRUE)
}


getSccsControlEstimates <- function(connectionHandler,
                                    resultDatabaseSettings,
                                    databaseId,
                                    analysisId,
                                    covariateId) {

  sql <- "
  SELECT ci_95_lb, ci_95_ub, log_rr, se_log_rr, calibrated_ci_95_lb, calibrated_ci_95_ub, calibrated_log_rr,
  calibrated_se_log_rr, true_effect_size
  FROM @database_schema.@table_prefixresult sr
  INNER JOIN @database_schema.@table_prefixcovariate sc ON (
    sc.exposures_outcome_set_id = sr.exposures_outcome_set_id AND
    sc.database_id = sr.database_id AND
    sc.analysis_id = sr.analysis_id AND
    sc.covariate_id = sr.covariate_id
  )
  INNER JOIN @database_schema.@table_prefixexposure se ON (
    se.exposures_outcome_set_id = sr.exposures_outcome_set_id AND
    se.era_id = sc.era_id
  )
  WHERE sr.database_id = '@database_id'
  AND sr.analysis_id = @analysis_id
  AND sr.covariate_id = @covariate_id
  "
  connectionHandler$queryDb(sql,
                            database_schema = resultDatabaseSettings$schema,
                            table_prefix = resultDatabaseSettings$tablePrefix,
                            database_id = databaseId,
                            covariate_id = covariateId,
                            analysis_id = analysisId,
                            snakeCaseToCamelCase = TRUE)
}

getSccsDiagnosticsSummary <- function(connectionHandler,
                                      resultDatabaseSettings,
                                      exposuresOutcomeSetId,
                                      databaseId,
                                      analysisId,
                                      covariateId) {
  sql <- "
  SELECT *
  FROM @database_schema.@table_prefixdiagnostics_summary
  WHERE database_id = '@database_id'
  AND analysis_id = @analysis_id
  AND covariate_id = @covariate_id
  AND exposures_outcome_set_id = @exposures_outcome_set_id
  "
  connectionHandler$queryDb(sql,
                            database_schema = resultDatabaseSettings$schema,
                            table_prefix = resultDatabaseSettings$tablePrefix,
                            database_id = databaseId,
                            covariate_id = covariateId,
                            analysis_id = analysisId,
                            exposures_outcome_set_id = exposuresOutcomeSetId,
                            snakeCaseToCamelCase = TRUE)

}