
getExposuresOutcomes <- function(connectionHandler, resultsDatabaseSchema, includeControls = FALSE) {
  # Note: probably a bad idea to use the connectionHandler directly to create dbplyr table objects that later will
  # need to be joined, so checking out a stable connection from the pool:
  
  sccsExposuresOutcomeSet <- connectionHandler$tbl(DatabaseConnector::inDatabaseSchema(resultsDatabaseSchema, "sccs_exposures_outcome_set"))
  sccsExposure <- connectionHandler$tbl(DatabaseConnector::inDatabaseSchema(resultsDatabaseSchema, "sccs_exposure"))
  cohortDefinition <- connectionHandler$tbl(DatabaseConnector::inDatabaseSchema(resultsDatabaseSchema, "cg_cohort_definition"))
  sccsEra <- connectionHandler$tbl(DatabaseConnector::inDatabaseSchema(resultsDatabaseSchema, "sccs_era"))

  exposureOutcomes <- sccsExposuresOutcomeSet %>%
    inner_join(sccsExposure %>%
                 rename(exposure_id = "era_id"),
               by = "exposures_outcome_set_id") %>%
    inner_join(cohortDefinition %>%
                 select(outcome_id = "cohort_definition_id",
                        outcome_name = "cohort_name"),
               by = "outcome_id") %>%
    left_join(
      sccsEra %>%
        rename(exposure_id = "era_id",
               exposure_name = "era_name") %>%
        distinct(exposures_outcome_set_id, exposure_id, exposure_name),
      by = c("exposures_outcome_set_id", "exposure_id")) %>%
    left_join(cohortDefinition %>%
                select(exposure_id = "cohort_definition_id",
                       exposure_name_2 = "cohort_name"),

              by = "exposure_id") %>%
    mutate(exposure_name = if_else(is.na(exposure_name), exposure_name_2, exposure_name)) %>%
    select(-"exposure_name_2")


  if (!includeControls) {
    exposureOutcomes <- exposureOutcomes %>%
      filter(is.na(.data$true_effect_size))
  }
  exposureOutcomes %>%
    collect() %>%
    SqlRender::snakeCaseToCamelCaseNames() %>%
    return()
}

getSccsResults <- function(connectionHandler,
                           resultsDatabaseSchema,
                           exposuresOutcomeSetId,
                           databaseIds,
                           analysisIds) {

  sccsResult <- connectionHandler$tbl(DatabaseConnector::inDatabaseSchema(resultsDatabaseSchema, "sccs_result"))
  diagnosticsSummary <- connectionHandler$tbl(DatabaseConnector::inDatabaseSchema(resultsDatabaseSchema, "sccs_diagnostics_summary"))
  covariate <- connectionHandler$tbl(DatabaseConnector::inDatabaseSchema(resultsDatabaseSchema, "sccs_covariate"))

  sccsResult %>%
    inner_join(diagnosticsSummary, by = c("exposures_outcome_set_id", "database_id", "analysis_id", "covariate_id")) %>%
    inner_join(covariate, by = c("exposures_outcome_set_id", "database_id", "analysis_id", "covariate_id")) %>%
    filter(
      exposures_outcome_set_id == exposuresOutcomeSetId,
      database_id %in% databaseIds,
      analysis_id %in% analysisIds
    ) %>%
    collect() %>%
    SqlRender::snakeCaseToCamelCaseNames() %>%
    return()
}

getModel <- function(connectionHandler,
                     resultsDatabaseSchema,
                     exposuresOutcomeSetId,
                     databaseId,
                     analysisId) {


  covariateResult <- connectionHandler$tbl(DatabaseConnector::inDatabaseSchema(resultsDatabaseSchema, "sccs_covariate_result"))
  covariate <- connectionHandler$tbl(DatabaseConnector::inDatabaseSchema(resultsDatabaseSchema, "sccs_covariate"))
  era <- connectionHandler$tbl(DatabaseConnector::inDatabaseSchema(resultsDatabaseSchema, "sccs_era"))
  cohortDefinition <- connectionHandler$tbl(DatabaseConnector::inDatabaseSchema(resultsDatabaseSchema, "cg_cohort_definition"))

  covariateResult %>%
    inner_join(covariate, by = c("analysis_id", "exposures_outcome_set_id", "database_id", "covariate_id")) %>%
    filter(
      exposures_outcome_set_id == exposuresOutcomeSetId,
      database_id == !!databaseId,
      analysis_id == !!analysisId,
      !is.na(rr)
    ) %>%
    left_join(era, by = c("exposures_outcome_set_id", "analysis_id", "era_id", "database_id")) %>%
    left_join(cohortDefinition %>%
                select(era_id = "cohort_definition_id",
                       era_name_2 = "cohort_name"),
              by = "era_id") %>%
    mutate(exposure_name = if_else(is.na(era_name), era_name_2, era_name)) %>%
    mutate(covariate_name = if_else(is.na(era_name), covariate_name, paste(covariate_name, era_name, sep = ": "))) %>%
    select(
      "covariate_id",
      "covariate_name",
      "rr",
      "ci_95_lb",
      "ci_95_ub") %>%
    collect() %>%
    SqlRender::snakeCaseToCamelCaseNames() %>%
    return()
}

getTimeTrend <- function(connectionHandler,
                         resultsDatabaseSchema,
                         exposuresOutcomeSetId,
                         databaseId,
                         analysisId) {
  timeTrend <- connectionHandler$tbl(DatabaseConnector::inDatabaseSchema(resultsDatabaseSchema, "sccs_time_trend"))
  timeTrend %>%
    filter(
      exposures_outcome_set_id == exposuresOutcomeSetId,
      database_id == !!databaseId,
      analysis_id == !!analysisId,
    ) %>%
    collect() %>%
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
  diagnosticsSummary <- connectionHandler$tbl(DatabaseConnector::inDatabaseSchema(resultsDatabaseSchema, "sccs_diagnostics_summary"))
  p <- diagnosticsSummary %>%
    filter(
      exposures_outcome_set_id == exposuresOutcomeSetId,
      covariate_id == !!covariateId,
      database_id == !!databaseId,
      analysis_id == !!analysisId,
    ) %>%
    pull("pre_exposure_p")

  timeToEvent <- connectionHandler$tbl(DatabaseConnector::inDatabaseSchema(resultsDatabaseSchema, "sccs_time_to_event"))
  timeToEvent %>%
    filter(
      exposures_outcome_set_id == exposuresOutcomeSetId,
      era_id == !!eraId,
      database_id == !!databaseId,
      analysis_id == !!analysisId,
    ) %>%
    collect() %>%
    SqlRender::snakeCaseToCamelCaseNames() %>%
    mutate(p = !!p) %>%
    return()
}

getAttrition <- function(connectionHandler,
                         resultsDatabaseSchema,
                         exposuresOutcomeSetId,
                         databaseId,
                         analysisId,
                         covariateId) {
  attrition <- connectionHandler$tbl(DatabaseConnector::inDatabaseSchema(resultsDatabaseSchema, "sccs_attrition"))
  attrition %>%
    filter(
      exposures_outcome_set_id == exposuresOutcomeSetId,
      database_id == !!databaseId,
      analysis_id == !!analysisId,
      covariateId == !!covariateId
    ) %>%
    collect() %>%
    SqlRender::snakeCaseToCamelCaseNames() %>%
    return()
}

getEventDepObservation <- function(connectionHandler,
                                   resultsDatabaseSchema,
                                   exposuresOutcomeSetId,
                                   databaseId,
                                   analysisId) {
  eventDepObservation <- connectionHandler$tbl(DatabaseConnector::inDatabaseSchema(resultsDatabaseSchema, "sccs_event_dep_observation"))
  eventDepObservation %>%
    filter(
      exposures_outcome_set_id == exposuresOutcomeSetId,
      database_id == !!databaseId,
      analysis_id == !!analysisId
    ) %>%
    collect() %>%
    SqlRender::snakeCaseToCamelCaseNames() %>%
    return()
}

getAgeSpanning <- function(connectionHandler,
                           resultsDatabaseSchema,
                           exposuresOutcomeSetId,
                           databaseId,
                           analysisId) {
  ageSpanning <- connectionHandler$tbl(DatabaseConnector::inDatabaseSchema(resultsDatabaseSchema, "sccs_age_spanning"))
  ageSpanning %>%
    filter(
      exposures_outcome_set_id == exposuresOutcomeSetId,
      database_id == !!databaseId,
      analysis_id == !!analysisId
    ) %>%
    collect() %>%
    SqlRender::snakeCaseToCamelCaseNames() %>%
    return()
}

getCalendarTimeSpanning <- function(connectionHandler,
                                    resultsDatabaseSchema,
                                    exposuresOutcomeSetId,
                                    databaseId,
                                    analysisId) {
  calendarTimeSpanning <- connectionHandler$tbl(DatabaseConnector::inDatabaseSchema(resultsDatabaseSchema, "sccs_calendar_time_spanning"))
  calendarTimeSpanning %>%
    filter(
      exposures_outcome_set_id == exposuresOutcomeSetId,
      database_id == !!databaseId,
      analysis_id == !!analysisId
    ) %>%
    collect() %>%
    SqlRender::snakeCaseToCamelCaseNames() %>%
    return()
}

getSpline <- function(connectionHandler,
                      resultsDatabaseSchema,
                      exposuresOutcomeSetId,
                      databaseId,
                      analysisId,
                      splineType = "age") {
  spline <- connectionHandler$tbl(DatabaseConnector::inDatabaseSchema(resultsDatabaseSchema, "sccs_spline"))
  spline %>%
    filter(
      exposures_outcome_set_id == exposuresOutcomeSetId,
      database_id == !!databaseId,
      analysis_id == !!analysisId,
      spline_type == splineType
    ) %>%
    collect() %>%
    SqlRender::snakeCaseToCamelCaseNames() %>%
    return()
}


getControlEstimates <- function(connectionHandler,
                                resultsDatabaseSchema,
                                exposuresOutcomeSetId,
                                databaseId,
                                analysisId,
                                covariateId) {


  sccsResult <- connectionHandler$tbl(DatabaseConnector::inDatabaseSchema(resultsDatabaseSchema, "sccs_result"))
  sccsExposure <- connectionHandler$tbl(DatabaseConnector::inDatabaseSchema(resultsDatabaseSchema, "sccs_exposure"))
  sccsCovariate <- connectionHandler$tbl(DatabaseConnector::inDatabaseSchema(resultsDatabaseSchema, "sccs_covariate"))

  sccsResult %>%
    inner_join(sccsCovariate, by = c("analysis_id", "exposures_outcome_set_id", "covariate_id", "database_id")) %>%
    inner_join(sccsExposure, by = c("exposures_outcome_set_id", "era_id")) %>%
    filter(
      database_id == !!databaseId,
      analysis_id == !!analysisId,
      covariate_id == !!covariateId,
      !is.na(true_effect_size)
    ) %>%
    select("ci_95_lb", "ci_95_ub", "log_rr", "se_log_rr", "calibrated_ci_95_lb", "calibrated_ci_95_ub", "calibrated_log_rr", "calibrated_se_log_rr", "true_effect_size") %>%
    collect() %>%
    SqlRender::snakeCaseToCamelCaseNames() %>%
    return()
}

getDiagnosticsSummary <- function(connectionHandler,
                                  resultsDatabaseSchema,
                                  exposuresOutcomeSetId,
                                  databaseId,
                                  analysisId,
                                  covariateId) {
  diagnosticsSummary <- connectionHandler$tbl(DatabaseConnector::inDatabaseSchema(resultsDatabaseSchema, "sccs_diagnostics_summary"))
  diagnosticsSummary %>%
    filter(
      exposures_outcome_set_id == exposuresOutcomeSetId,
      database_id == !!databaseId,
      analysis_id == !!analysisId,
      covariateId == !!covariateId
    ) %>%
    collect() %>%
    SqlRender::snakeCaseToCamelCaseNames() %>%
    return()
}