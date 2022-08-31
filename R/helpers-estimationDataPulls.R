
loadEstimationData <- function(dataFolder) {
  positiveControlOutcome <- NULL
  
  splittableTables <- c("covariate_balance", "preference_score_dist", "kaplan_meier_dist")
  
  files <- list.files(dataFolder, pattern = ".rds")
  
  # Find part to remove from all file names (usually databaseId):
  databaseFileName <- files[grepl("^database", files)]
  removeParts <- paste0(gsub("database", "", databaseFileName), "$")
  
  # Remove data already in global environment:
  for (removePart in removeParts) {
    tableNames <- gsub("_t[0-9]+_c[0-9]+$", "", gsub(removePart, "", files[grepl(removePart, files)]))
    camelCaseNames <- SqlRender::snakeCaseToCamelCase(tableNames)
    camelCaseNames <- unique(camelCaseNames)
    camelCaseNames <- camelCaseNames[!(camelCaseNames %in% SqlRender::snakeCaseToCamelCase(splittableTables))]
    suppressWarnings(
      rm(list = camelCaseNames)
    )
  }
  
  # Load data from data folder. R data objects will get names derived from the filename:
  loadFile <- function(file, removePart) {
    tableName <- gsub("_t[0-9]+_c[0-9]+$", "", gsub(removePart, "", file))
    camelCaseName <- SqlRender::snakeCaseToCamelCase(tableName)
    if (!(tableName %in% splittableTables)) {
      newData <- readRDS(file.path(dataFolder, file))
      colnames(newData) <- SqlRender::snakeCaseToCamelCase(colnames(newData))
      if (exists(camelCaseName, envir = .GlobalEnv)) {
        existingData <- get(camelCaseName, envir = .GlobalEnv)
        newData$tau <- NULL
        newData$traditionalLogRr <- NULL
        newData$traditionalSeLogRr <- NULL
        if (!all(colnames(newData) %in% colnames(existingData))) {
          stop(sprintf("Columns names do not match in %s. \nObserved:\n %s, \nExpecting:\n %s",
                       file,
                       paste(colnames(newData), collapse = ", "),
                       paste(colnames(existingData), collapse = ", ")))
          
        }
        newData <- dplyr::bind_rows(existingData, newData)
        newData <- unique(newData)
      }
      assign(camelCaseName, newData, envir = .GlobalEnv)
    }
    invisible(NULL)
  }
  # removePart <- removeParts[3]
  file <- files[grepl(removePart, files)][1]
  for (removePart in removeParts) {
    invisible(lapply(files[grepl(removePart, files)], loadFile, removePart))
  }
  
  # tcos <- unique(cohortMethodResult[, c("targetId", "comparatorId", "outcomeId")])
  # tcos <- tcos[tcos$outcomeId %in% outcomeOfInterest$outcomeId, ]
  # assign("tcos", tcos, envir = .GlobalEnv)
  metaAnalysisDbIds <- database$databaseId[database$isMetaAnalysis == 1]
  assign("metaAnalysisDbIds", metaAnalysisDbIds, envir = .GlobalEnv)
  
  
  
}


getEstimationTcoChoice <- function(connection, resultsSchema, tcoVar) {
  sql <- "
  SELECT
  DISTINCT
  cmtco.@tco_var,
  cd.cohort_name
FROM
  @results_schema.cm_target_comparator_outcome cmtco
  join @results_schema.cg_cohort_definition cd on cmtco.@tco_var = cd.cohort_definition_id;
  "
  return(
    DatabaseConnector::renderTranslateQuerySql(connection, sql,
                                               snakeCaseToCamelCase = TRUE,
                                               results_schema = resultsSchema,
                                               tco_var = tcoVar)
  )
}


getEstimationTargetChoices <- function(connection, resultsSchema) {
  return(
    getEstimationTcoChoice(connection, resultsSchema, "target_id")
  )
}


getEstimationComparatorChoices <- function(connection, resultsSchema) {
  return(
    getEstimationTcoChoice(connection, resultsSchema, "comparator_id")
  )
}


getEstimationOutcomeChoices <- function(connection, resultsSchema) {
  return(
    getEstimationTcoChoice(connection, resultsSchema, "outcome_id")
  )
}


getEstimationDatabaseChoices <- function(connection, resultsSchema) {
  sql <- "
SELECT
DISTINCT
dmd.database_id,
dmd.cdm_source_abbreviation
FROM
  @results_schema.cm_result cmr
  join @results_schema.database_meta_data dmd on dmd.database_id = cmr.database_id;

  "
  return(
    DatabaseConnector::renderTranslateQuerySql(connection, sql,
                                               snakeCaseToCamelCase = TRUE,
                                               results_schema = resultsSchema)
  )
}


getCmAnalysisOptions <- function(connection, resultsSchema) {
  sql <- "
SELECT
DISTINCT
cma.analysis_id,
cma.description
FROM
  @results_schema.cm_analysis cma;
  "
  return(
    DatabaseConnector::renderTranslateQuerySql(connection, sql,
                                               snakeCaseToCamelCase = TRUE,
                                               results_schema = resultsSchema)
  )
}

getAllEstimationResults <- function(connection, resultsSchema) {
  sql <- "
SELECT
  cma.analysis_id,
  cma.description description,
  dmd.database_id database_id, -- break?
  dmd.cdm_source_abbreviation cdm_source_abbreviation,
  cmr.rr rr,
  cmr.ci_95_lb ci_95_lb,
  cmr.ci_95_ub ci_95_ub,
  cmr.p p,
  cmr.log_rr,
  cmr.se_log_rr,
  cmr.target_subjects,
  cmr.comparator_subjects, 
  cmr.target_days,
  cmr.comparator_days,
  cmr.target_outcomes,
  cmr.comparator_outcomes,
  cmr.calibrated_rr calibrated_rr,
  cmr.calibrated_ci_95_lb calibrated_ci_95_lb,
  cmr.calibrated_ci_95_ub calibrated_ci_95_ub,
  cmr.calibrated_p calibrated_p,
  cmr.calibrated_log_rr,
  cmr.calibrated_se_log_rr,
  COALESCE(cmds.unblind, 0) unblind -- TODO: assume unblinded? (or always populated and moot)
FROM
  @results_schema.cm_analysis cma
  JOIN @results_schema.cm_result cmr on cmr.analysis_id = cma.analysis_id
  JOIN @results_schema.database_meta_data dmd on dmd.database_id = cmr.database_id
  LEFT JOIN @results_schema.cm_diagnostics_summary cmds on cmds.analysis_id = cmr.analysis_id
  "
  
  
  return(
    DatabaseConnector::renderTranslateQuerySql(connection, sql,
                                               snakeCaseToCamelCase = TRUE,
                                               warnOnMissingParameters = FALSE,
                                               results_schema = resultsSchema)
  )
}


getEstimationMainResults <- function(connection,
                                     resultsSchema,
                                     targetIds = c(),
                                     comparatorIds = c(),
                                     outcomeIds = c(),
                                     databaseIds = c(),
                                     analysisIds = c()) {
  
  sql <- "
SELECT
  cma.analysis_id,
  cma.description description,
  dmd.database_id database_id, -- break?
  dmd.cdm_source_abbreviation cdm_source_abbreviation,
  cmr.rr rr,
  cmr.ci_95_lb ci_95_lb,
  cmr.ci_95_ub ci_95_ub,
  cmr.p p,
  cmr.log_rr,
  cmr.se_log_rr,
  cmr.target_subjects,
  cmr.comparator_subjects, 
  cmr.target_days,
  cmr.comparator_days,
  cmr.target_outcomes,
  cmr.comparator_outcomes,
  cmr.calibrated_rr calibrated_rr,
  cmr.calibrated_ci_95_lb calibrated_ci_95_lb,
  cmr.calibrated_ci_95_ub calibrated_ci_95_ub,
  cmr.calibrated_p calibrated_p,
  cmr.calibrated_log_rr,
  cmr.calibrated_se_log_rr,
  COALESCE(cmds.unblind, 0) unblind -- TODO: assume unblinded? (or always populated and moot)
FROM
  @results_schema.cm_analysis cma
  JOIN @results_schema.cm_result cmr on cmr.analysis_id = cma.analysis_id
  JOIN @results_schema.database_meta_data dmd on dmd.database_id = cmr.database_id
  LEFT JOIN @results_schema.cm_diagnostics_summary cmds on cmds.analysis_id = cmr.analysis_id
	AND cmds.target_id = cmr.target_id
	AND cmds.comparator_id = cmr.comparator_id
	AND cmds.outcome_id = cmr.outcome_id
	AND cmds.database_id = cmr.database_id
  "
  if (length(targetIds) > 0 ||
      length(comparatorIds) > 0 ||
      length(outcomeIds) > 0 ||
      length(databaseIds) > 0 ||
      length(analysisIds) > 0) {
    sql <- paste0(sql, "\nWHERE\n\t")
  }
  
  clauses <- c()
  if (length(targetIds) > 0 ) {
    clauses <- c(clauses, "cmr.target_id IN (@target_ids)\n\t")
  }
  if (length(comparatorIds) > 0) {
    clauses <- c(clauses, "cmr.comparator_id IN (@comparator_ids)\n\t")
  }
  if (length(outcomeIds) > 0) {
    clauses <- c(clauses, "cmr.outcome_id IN (@outcome_ids)\n\t")
  }
  if (length(databaseIds) > 0) {
    clauses <- c(clauses, "cmr.database_id IN (@database_ids)\n\t")
  }
  if (length(analysisIds) > 0) {
    clauses <- c(clauses, "cmr.analysis_id IN (@analysis_ids)\n\t")
  }
  sql <- paste0(sql, paste(clauses, collapse = " AND "), ";")
  return(
    DatabaseConnector::renderTranslateQuerySql(connection, sql,
                                               snakeCaseToCamelCase = TRUE,
                                               warnOnMissingParameters = FALSE,
                                               results_schema = resultsSchema,
                                               target_ids = paste0("'", paste(targetIds, collapse = "', '"), "'"),
                                               comparator_ids = paste0("'", paste(comparatorIds, collapse = "', '"), "'"),
                                               outcome_ids = paste0("'", paste(outcomeIds, collapse = "', '"), "'"),
                                               database_ids = paste0("'", paste(databaseIds, collapse = "', '"), "'"),
                                               analysis_ids = paste0("'", paste(analysisIds, collapse = "', '"), "'"))
  )
  
}


getCohortMethodAnalyses <- function(connection, resultsSchema) {
  sql <- "
  SELECT
    cma.*
  FROM
    @results_schema.cm_analysis cma
  "
  return(
    DatabaseConnector::renderTranslateQuerySql(connection, sql,
                                               snakeCaseToCamelCase = TRUE,
                                               results_schema = resultsSchema)
  )
}


getEstimationSubgroupResults <- function(connection,
                                         targetIds = c(),
                                         comparatorIds = c(),
                                         outcomeIds = c(),
                                         databaseIds = c(),
                                         analysisIds = c(),
                                         subgroupIds = c(),
                                         estimatesOnly = FALSE) {
  idx <- rep(TRUE, nrow(cmInteractionResult))
  if (length(targetIds) != 0) {
    idx <- idx & cmInteractionResult$targetId %in% targetIds
  }
  if (length(comparatorIds) != 0) {
    idx <- idx & cmInteractionResult$comparatorId %in% comparatorIds
  }
  if (length(outcomeIds) != 0) {
    idx <- idx & cmInteractionResult$outcomeId %in% outcomeIds
  }
  if (length(databaseIds) != 0) {
    idx <- idx & cmInteractionResult$databaseId %in% databaseIds
  }
  if (length(analysisIds) != 0) {
    idx <- idx & cmInteractionResult$analysisId %in% analysisIds
  }
  if (length(subgroupIds) != 0) {
    idx <- idx & cmInteractionResult$interactionCovariateId %in% subgroupIds
  }
  result <- cmInteractionResult[idx, ]
  result <- merge(result, data.frame(interactionCovariateId = covariate$covariateId,
                                     databaseId = covariate$databaseId,
                                     covariateName = covariate$covariateName))
  result <- result[, c("covariateName",
                       "targetSubjects",
                       "comparatorSubjects",
                       "rrr",
                       "ci95Lb",
                       "ci95Ub",
                       "p",
                       "calibratedP")]
  colnames(result) <- c("interactionCovariateName",
                        "targetSubjects",
                        "comparatorSubjects",
                        "rrr",
                        "ci95Lb",
                        "ci95Ub",
                        "p",
                        "calibratedP")
  return(result)
}


getEstimationControlResults <- function(connection, resultsSchema, targetId,
                                        comparatorId, analysisId, databaseId = NULL,
                                        includePositiveControls = TRUE) {
  
  sql <- "
    SELECT
      cmr.*,
      cmtco.true_effect_size effect_size
    FROM
      @results_schema.cm_result cmr
      JOIN @results_schema.cm_target_comparator_outcome cmtco ON cmr.target_id = cmtco.target_id AND cmr.comparator_id = cmtco.comparator_id AND cmr.outcome_id = cmtco.outcome_id
    WHERE
      cmtco.outcome_of_interest != 1
      AND cmr.target_id = @target_id
      AND cmr.comparator_id = @comparator_id
      AND cmr.analysis_id = @analysis_id
  "
  
  
  if (!is.null(databaseId)) {
    # update sql
    sql <- paste(sql, paste("AND cmr.database_id = '@database_id'"), collapse = "\n")
  }
  
  if (!includePositiveControls) {
    # update sql
    sql <- paste(sql, paste("AND cmtco.true_effect_size = 1"), collapse = "\n")
  }
  
  return(
    DatabaseConnector::renderTranslateQuerySql(connection, sql,
                                               snakeCaseToCamelCase = TRUE,
                                               warnOnMissingParameters = FALSE,
                                               results_schema = resultsSchema,
                                               target_id = targetId,
                                               comparator_id = comparatorId,
                                               analysis_id = analysisId,
                                               database_id = databaseId)
  )
}


getCmFollowUpDist <- function(connection,
                              resultsSchema,
                              targetId,
                              comparatorId,
                              outcomeId,
                              databaseId = NULL,
                              analysisId) {
  
  sql <- "
  SELECT
    *
  FROM
    @results_schema.cm_follow_up_dist cmfud
  WHERE
    cmfud.target_id = @target_id
    AND cmfud.comparator_id = @comparator_id
    AND cmfud.outcome_id = @outcome_id
    AND cmfud.analysis_id = @analysis_id
  "
  if(!is.null(databaseId)) {
    sql <- paste(sql, paste("AND cmfud.database_id = '@database_id'"), collapse = "\n")
  }
  return(
    DatabaseConnector::renderTranslateQuerySql(connection, sql,
                                               snakeCaseToCamelCase = TRUE,
                                               warnOnMissingParameters = FALSE,
                                               results_schema = resultsSchema,
                                               target_id = targetId,
                                               comparator_id = comparatorId,
                                               outcome_id = outcomeId,
                                               analysis_id = analysisId,
                                               database_id = databaseId)
  )
}


getEstimationCovariateBalance <- function(connection,
                                          resultsSchema,
                                          targetId,
                                          comparatorId,
                                          analysisId,
                                          databaseId = NULL,
                                          outcomeId = NULL) {
  
  
  sql <- ""
  if (is.null(outcomeId)) {
    sql <- "
      SELECT
        cmscb.database_id,
        cmscb.covariate_id,
        cmc.covariate_name,
        -- cmc.covariate_analysis_id analysis_id, #TODO: once cm_analysis_id bug fixed
        cmscb.target_mean_before before_matching_mean_treated,
        cmscb.comparator_mean_before before_matching_mean_comparator,
        abs(cmscb.std_diff_before) abs_before_matching_std_diff, --absBeforeMatchingStdDiff
        cmscb.target_mean_after after_matching_mean_treated,
        cmscb.comparator_mean_after after_matching_mean_comparator,
        abs(cmscb.std_diff_after) abs_after_matching_std_diff
      FROM
        @results_schema.cm_shared_covariate_balance cmscb 
        JOIN @results_schema.cm_covariate cmc ON cmscb.covariate_id = cmc.covariate_id AND cmscb.analysis_id = cmc.analysis_id AND cmscb.database_id = cmc.database_id -- database_id optional
       -- JOIN poc.cm_covariate_analysis cmca ON cmca.analysis_id = cmc.analysis_id  -- question: shouldn't we have a covariate_analysis_id in cm_covariate table?
      WHERE
        cmscb.target_id = @target_id
        AND cmscb.comparator_id = @comparator_id
        AND cmscb.analysis_id = @analysis_id
        AND cmscb.database_id = '@database_id'
    "
  } else {
    sql <- "
      SELECT
        cmcb.database_id,
        cmcb.covariate_id,
        cmc.covariate_name,
        -- cmc.covariate_analysis_id analysis_id, #TODO: once cm_analysis_id bug fixed
        cmcb.target_mean_before before_matching_mean_treated,
        cmcb.comparator_mean_before before_matching_mean_comparator,
        abs(cmcb.std_diff_before) before_matching_std_diff,
        cmcb.target_mean_after after_matching_mean_treated,
        cmcb.comparator_mean_after after_matching_mean_comparator,
        abs(cmcb.std_diff_after) after_matching_std_diff
      FROM
        @results_schema.cm_covariate_balance cmcb
        JOIN @results_schema.cm_covariate cmc ON cmcb.covariate_id = cmcb.covariate_id AND cmcb.analysis_id = cmc.analysis_id AND cmcb.database_id = cmc.database_id -- database_id optional
       -- JOIN poc.cm_covariate_analysis cmca ON cmca.analysis_id = cmc.analysis_id  -- question: shouldn't we have a covariate_analysis_id in cm_covariate table?
      WHERE
        cmcb.target_id = @target_id
        AND cmcb.comparator_id = @comparator_id
        AND cmcb.outcome_id = @outcome_id
        AND cmcb.analysis_id = @analysis_id
        AND cmcb.database_id = '@database_id'
    "
  }
  
  return(
    DatabaseConnector::renderTranslateQuerySql(connection = connection,
                                               sql = sql,
                                               snakeCaseToCamelCase = TRUE,
                                               warnOnMissingParameters = FALSE,
                                               results_schema = resultsSchema,
                                               target_id = targetId,
                                               comparator_id = comparatorId,
                                               outcome_id = outcomeId,
                                               analysis_id = analysisId,
                                               database_id = databaseId)
  )
  
}


getEstimationPs <- function(connection, resultsSchema, targetId, comparatorId, analysisId, databaseId = NULL) {
  sql <- "
    SELECT
      *
    FROM
      @results_schema.cm_preference_score_dist cmpsd
    WHERE
      cmpsd.target_id = @target_id
      AND cmpsd.comparator_id = @comparator_id
      AND cmpsd.analysis_id = @analysis_id
  "
  if(!is.null(databaseId)) {
    sql <- paste(sql, paste("AND cmpsd.database_id = '@database_id'"), collapse = "\n")
  }
  
  
  ps <- DatabaseConnector::renderTranslateQuerySql(connection, sql,
                                                   snakeCaseToCamelCase = TRUE,
                                                   warnOnMissingParameters = FALSE,
                                                   results_schema = resultsSchema,
                                                   target_id = targetId,
                                                   comparator_id = comparatorId,
                                                   analysis_id = analysisId,
                                                   database_id = databaseId)
  
  
  if (!is.null(databaseId)) {
    ps$databaseId <- NULL
  }
  return(ps)
}


getEstimationKaplanMeier <- function(connection, resultsSchema, targetId, comparatorId, outcomeId, databaseId, analysisId) {
  sqlTmp <- "
  SELECT
    *
  FROM
    @results_schema.cm_kaplan_meier_dist cmkmd
    JOIN @results_schema.database_meta_data dmd on  dmd.database_id = cmkmd.database_id
  WHERE
    cmkmd.target_id = @target_id
    AND cmkmd.comparator_id = @comparator_id
    AND cmkmd.outcome_id = @outcome_id
    AND cmkmd.analysis_id = @analysis_id
    AND dmd.cdm_source_abbreviation = '@database_id';
  "
  sql <- "
  SELECT
    *
  FROM
    @results_schema.cm_kaplan_meier_dist cmkmd
  WHERE
    cmkmd.target_id = @target_id
    AND cmkmd.comparator_id = @comparator_id
    AND cmkmd.outcome_id = @outcome_id
    AND cmkmd.analysis_id = @analysis_id
    AND cmkmd.database_id = '@database_id';
  "
  
  return(
    DatabaseConnector::renderTranslateQuerySql(connection, sql,
                                               snakeCaseToCamelCase = TRUE,
                                               results_schema = resultsSchema,
                                               target_id = targetId,
                                               comparator_id = comparatorId,
                                               outcome_id = outcomeId,
                                               analysis_id = analysisId,
                                               database_id = databaseId)
  )
}


getEstimationAttrition <- function(connection, resultsSchema, targetId, comparatorId, outcomeId, analysisId, databaseId) {
  sqlTmp <- "
  SELECT
    cmat.*
  FROM
    @results_schema.cm_attrition cmat
    JOIN @results_schema.database_meta_data dmd on dmd.database_id = cmat.database_id
  WHERE
  cmat.target_id = @target_id
  AND cmat.comparator_id = @comparator_id
  AND cmat.outcome_id = @outcome_id
  AND cmat.analysis_id = @analysis_id
  AND dmd.cdm_source_abbreviation = '@database_id';
  "
  sql <- "
  SELECT
    cmat.*
  FROM
    @results_schema.cm_attrition cmat
  WHERE
  cmat.target_id = @target_id
  AND cmat.comparator_id = @comparator_id
  AND cmat.outcome_id = @outcome_id
  AND cmat.analysis_id = @analysis_id
  AND cmat.database_id = '@database_id';
  "
  result <- DatabaseConnector::renderTranslateQuerySql(connection, sql,
                                                       snakeCaseToCamelCase = TRUE,
                                                       results_schema = resultsSchema,
                                                       target_id = targetId,
                                                       comparator_id = comparatorId,
                                                       outcome_id = outcomeId,
                                                       analysis_id = analysisId,
                                                       database_id = databaseId)
  targetAttrition <- result[result$exposureId == targetId, ]
  comparatorAttrition <- result[result$exposureId == comparatorId, ]
  colnames(targetAttrition)[colnames(targetAttrition) == "subjects"] <- "targetPersons"
  targetAttrition$exposureId <- NULL
  colnames(comparatorAttrition)[colnames(comparatorAttrition) == "subjects"] <- "comparatorPersons"
  comparatorAttrition$exposureId <- NULL
  result <- merge(targetAttrition, comparatorAttrition)
  result <- result[order(result$sequenceNumber), ]
  return(result)
}


getEstimationStudyPeriod <- function(connection, targetId, comparatorId, databaseId) {
  sql <- "SELECT min_date,
  max_date
  FROM comparison_summary
  WHERE target_id = @target_id
  AND comparator_id = @comparator_id
  AND database_id = '@database_id'"
  sql <- SqlRender::renderSql(sql,
                              target_id = targetId,
                              comparator_id = comparatorId,
                              database_id = databaseId)$sql
  sql <- SqlRender::translateSql(sql, targetDialect = connection@dbms)$sql
  studyPeriod <- querySql(connection, sql)
  colnames(studyPeriod) <- SqlRender::snakeCaseToCamelCase(colnames(studyPeriod))
  return(studyPeriod)
}


getEstimationPropensityModel <- function(connection, resultsSchema, targetId, comparatorId, analysisId, databaseId) {
  sqlTmp <- "
  SELECT
    cmpm.coefficient,
    cmc.covariate_id,
    cmc.covariate_name
  FROM
    @results_schema.cm_covariate cmc
    JOIN @results_schema.cm_propensity_model cmpm ON cmc.covariate_id = cmpm.covariate_id AND cmc.database_id = cmpm.database_id
  WHERE
    cmpm.target_id = @target_id
    AND cmpm.comparator_id = @comparator_id
    AND cmpm.analysis_id = @analysis_id
    AND cmpm.database_id = '@database_id'
  "
  
  sql <- "
    SELECT
    cmc.covariate_id,
    cmc.covariate_name,
    cmpm.coefficient
  FROM
    (
      SELECT
        covariate_id,
        covariate_name
      FROM
        @results_schema.cm_covariate
      WHERE
        analysis_id = @analysis_id
        AND database_id = '@database_id'
      UNION
      SELECT
      0 as covariate_id,
      'intercept' as covariate_name) cmc
    JOIN @results_schema.cm_propensity_model cmpm ON cmc.covariate_id = cmpm.covariate_id
  WHERE
    cmpm.target_id = @target_id
    AND cmpm.comparator_id = @comparator_id
    AND cmpm.analysis_id = @analysis_id
    AND cmpm.database_id = '@database_id'
  "
  
  model <- DatabaseConnector::renderTranslateQuerySql(connection, sql,
                                                      snakeCaseToCamelCase = TRUE,
                                                      results_schema = resultsSchema,
                                                      target_id = targetId,
                                                      comparator_id = comparatorId,
                                                      analysis_id = analysisId,
                                                      database_id = databaseId)
  return(model)
}


getEstimationCovariateBalanceSummary <- function(connection, targetId, comparatorId, analysisId,
                                                 beforeLabel = "Before matching",
                                                 afterLabel = "After matching") {
  
  balance <- getEstimationCovariateBalance(connection = connection,
                                           targetId = targetId,
                                           comparatorId = comparatorId,
                                           analysisId = analysisId,
                                           outcomeId = NULL)
  balanceBefore <- balance %>%
    dplyr::group_by(.data$databaseId) %>%
    dplyr::summarise(covariateCount = dplyr::n(),
                     qs = quantile(.data$beforeMatchingStdDiff, c(0, 0.25, 0.5, 0.75, 1)), prob = c("ymin", "lower", "median", "upper", "ymax")) %>%
    tidyr::spread(key = "prob", value = "qs")
  balanceBefore[, "type"] <- beforeLabel
  balanceAfter <-  balance %>%
    dplyr::group_by(.data$databaseId) %>%
    dplyr::summarise(covariateCount = dplyr::n(),
                     qs = quantile(.data$afterMatchingStdDiff, c(0, 0.25, 0.5, 0.75, 1)), prob = c("ymin", "lower", "median", "upper", "ymax")) %>%
    tidyr::spread(key = "prob", value = "qs")
  balanceAfter[, "type"] <- afterLabel
  
  balanceSummary <- rbind(balanceBefore, balanceAfter) %>%
    dplyr::ungroup()
  
  return(balanceSummary)
  
}

getEstimationNegativeControlEstimates <- function(cohortMethodResult, connection, targetId, comparatorId, analysisId) {
  subset <- getEstimationControlResults(cohortMethodResult, connection, targetId, comparatorId, analysisId, includePositiveControls = FALSE)
  subset <- subset[, c("databaseId", "logRr", "seLogRr")]
  if(nrow(subset) == 0)
    return(NULL)
  return(subset)
}



getDiagnosticsData <- function(connection, resultsSchema) {
  sql <- "
    SELECT
      dmd.cdm_source_abbreviation database_name,
      cma.description analysis_desc,
      cgcd1.cohort_name target,
      cgcd2.cohort_name comparator,
      cgcd3.cohort_name outcome,
      cmds.max_sdm,
      cmds.shared_max_sdm,
      cmds.equipoise,
      cmds.mdrr,
      cmds.attrition_fraction,
      cmds.ease,
      cmds.balance_diagnostic,
      cmds.shared_balance_diagnostic,
      cmds.equipoise_diagnostic,
      cmds.mdrr_diagnostic,
      cmds.attrition_diagnostic,
      cmds.ease_diagnostic,
      cmds.unblind
    FROM
      @results_schema.cm_diagnostics_summary cmds
      JOIN @results_schema.cm_analysis cma ON cmds.analysis_id = cma.analysis_id
      JOIN @results_schema.database_meta_data dmd ON dmd.database_id = cmds.database_id
      JOIN @results_schema.cg_cohort_definition cgcd1 ON cmds.target_id = cgcd1.cohort_definition_id
      JOIN @results_schema.cg_cohort_definition cgcd2 ON cmds.comparator_id = cgcd2.cohort_definition_id
      JOIN @results_schema.cg_cohort_definition cgcd3 ON cmds.outcome_id = cgcd3.cohort_definition_id
  "
  
  return(
    DatabaseConnector::renderTranslateQuerySql(connection, sql,
                                               snakeCaseToCamelCase = TRUE,
                                               results_schema = resultsSchema)
  )
}
