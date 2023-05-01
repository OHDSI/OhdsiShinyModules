sccsGetOutcomes <- function(
    connectionHandler, 
    resultDatabaseSettings
){
  # Note Query rew-written from dplyr because of data type/casting issues with null values in left joins
  sql <- "
  select distinct
  c.cohort_name as name,
  eos.outcome_id 
  
  from
   @my_schema.@sccs_table_prefixexposures_outcome_set as eos
   inner join
   @my_schema.@cg_table_prefixcohort_definition as c
   on c.cohort_definition_id = eos.outcome_id
   inner join
   @my_schema.@sccs_table_prefixexposure as e
   on e.exposures_outcome_set_id = eos.exposures_outcome_set_id
   
   --where e.true_effect_size != 1
   ;
  "
  outcomes <- connectionHandler$queryDb(
    sql,
    my_schema = resultDatabaseSettings$schema,
    cg_table_prefix = resultDatabaseSettings$cohortTablePrefix,
    sccs_table_prefix = resultDatabaseSettings$tablePrefix,
    snakeCaseToCamelCase = TRUE
  )
  
  result <- outcomes$outcomeId
  names(result) <- outcomes$name
  
  return(result)
}


sccsGetExposures <- function(
    connectionHandler, 
    resultDatabaseSettings
){
  # Note Query rew-written from dplyr because of data type/casting issues with null values in left joins
  sql <- "
  select distinct
  c.cohort_name as name,
  e.era_id as exposure_id
  
  from
   @my_schema.@cg_table_prefixcohort_definition as c
   inner join
   @my_schema.@sccs_table_prefixexposure as e
   on e.era_id = c.cohort_definition_id;
  "
  exposures <- connectionHandler$queryDb(
    sql,
    my_schema = resultDatabaseSettings$schema,
    cg_table_prefix = resultDatabaseSettings$cohortTablePrefix,
    sccs_table_prefix = resultDatabaseSettings$tablePrefix,
    snakeCaseToCamelCase = TRUE
  )
  
  result <- exposures$exposureId
  names(result) <- exposures$name
  
  return(result)
}


sccsGetDatabases <- function(
    connectionHandler, 
    resultDatabaseSettings
){
  # Note Query rew-written from dplyr because of data type/casting issues with null values in left joins
  sql <- "
  select distinct
  ds.cdm_source_abbreviation as database_name,
  r.database_id
  
  from
   @my_schema.@database_table_prefix@database_table ds 
   inner join
   @my_schema.@sccs_table_prefixresult as r
   on r.database_id = ds.database_id;
  "
  dbs <- connectionHandler$queryDb(
    sql,
    my_schema = resultDatabaseSettings$schema,
    database_table_prefix = resultDatabaseSettings$databaseTablePrefix,
    database_table = resultDatabaseSettings$databaseTable,
    sccs_table_prefix = resultDatabaseSettings$tablePrefix,
    snakeCaseToCamelCase = TRUE
  )
  
  result <- dbs$databaseId
  names(result) <- dbs$databaseName
  
  return(result)
}

sccsGetAnalyses <- function(
    connectionHandler, 
    resultDatabaseSettings
){
  # Note Query rew-written from dplyr because of data type/casting issues with null values in left joins
  sql <- "
  select distinct
  a.description as name,
  r.analysis_id
  
  from
   @my_schema.@sccs_table_prefixresult as r
   inner join
   @my_schema.@sccs_table_prefixanalysis as a
   on r.analysis_id = a.analysis_id
   ;
  "
  analyses <- connectionHandler$queryDb(
    sql,
    my_schema = resultDatabaseSettings$schema,
    sccs_table_prefix = resultDatabaseSettings$tablePrefix,
    snakeCaseToCamelCase = TRUE
  )
  
  result <- analyses$analysisId
  names(result) <- analyses$name
  
  result2 <- unlist(lapply(unique(names(result)), function(x){paste(result[names(result) == x], collapse=',')}))
  names(result2) <- unique(names(result))
  
  return(result2)
}

getSccsResults <- function(connectionHandler,
                           resultDatabaseSettings,
                           exposureIds,
                           outcomeIds,
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
  sc.covariate_analysis_id,
  a.description,
  eos.outcome_id
  FROM @database_schema.@table_prefixresult sr
  INNER JOIN 
  @database_schema.@database_table_prefix@database_table ds 
  ON sr.database_id = ds.database_id
  INNER JOIN 
  @database_schema.@table_prefixdiagnostics_summary sds ON (
    sds.exposures_outcome_set_id = sr.exposures_outcome_set_id AND
    sds.database_id = sr.database_id AND
    sds.analysis_id = sr.analysis_id AND
    sds.covariate_id = sr.covariate_id
  )
  INNER JOIN 
  @database_schema.@table_prefixcovariate sc ON (
    sc.exposures_outcome_set_id = sr.exposures_outcome_set_id AND
    sc.database_id = sr.database_id AND
    sc.analysis_id = sr.analysis_id AND
    sc.covariate_id = sr.covariate_id
  )
  INNER JOIN @database_schema.@table_prefixexposures_outcome_set eos
  ON 
  eos.exposures_outcome_set_id = sr.exposures_outcome_set_id
  INNER JOIN
  @database_schema.@table_prefixanalysis a
  on a.analysis_id = sr.analysis_id
  
  WHERE sr.analysis_id IN (@analysis_ids)
  AND sr.database_id IN (@database_ids)
  AND eos.outcome_id IN (@outcome_ids)
  AND sc.era_id IN (@exposure_ids)
  "

  results <- connectionHandler$queryDb(
    sql,
    database_schema = resultDatabaseSettings$schema,
    database_table_prefix = resultDatabaseSettings$databaseTablePrefix,
    database_table = resultDatabaseSettings$databaseTable,
    table_prefix = resultDatabaseSettings$tablePrefix,
    database_ids = paste(quoteLiterals(databaseIds), collapse = ','),
    analysis_ids = analysisIds,
    outcome_ids = paste(outcomeIds, collapse = ','),
    exposure_ids = paste(exposureIds, collapse = ','),
    snakeCaseToCamelCase = TRUE
  )

  return(results)
}

getSccsModel <- function(connectionHandler,
                         resultDatabaseSettings,
                         exposureId,
                         outcomeId,
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
  LEFT JOIN @database_schema.@cg_table_prefixcohort_definition cd 
  ON cd.cohort_definition_id = sc.era_id
  LEFT JOIN @database_schema.@table_prefixera era ON (
    era.exposures_outcome_set_id = scr.exposures_outcome_set_id AND
    era.database_id = scr.database_id AND
    era.analysis_id = scr.analysis_id AND
    era.era_id = sc.era_id
  )
  INNER JOIN @database_schema.@table_prefixexposures_outcome_set eos
  ON 
  eos.exposures_outcome_set_id = scr.exposures_outcome_set_id
  
  WHERE scr.database_id = '@database_id'
  AND scr.analysis_id = @analysis_id
  AND eos.outcome_id = @outcome_id
  AND  sc.era_id = @exposure_id
  AND scr.rr IS NOT NULL
  "

  connectionHandler$queryDb(sql,
                            database_schema = resultDatabaseSettings$schema,
                            table_prefix = resultDatabaseSettings$tablePrefix,
                            cg_table_prefix = resultDatabaseSettings$cohortTablePrefix,
                            database_id = databaseId,
                            analysis_id = analysisId,
                            outcome_id = outcomeId,
                            exposure_id = exposureId,
                            snakeCaseToCamelCase = TRUE)
}

getSccsTimeTrend <- function(connectionHandler,
                             resultDatabaseSettings,
                             exposureId,
                             outcomeId,
                             databaseId,
                             analysisId) {
  sql <- "
  SELECT tt.*
  FROM @database_schema.@table_prefixtime_trend tt
  inner join
  @database_schema.@table_prefixexposures_outcome_set eos
  on tt.exposures_outcome_set_id = eos.exposures_outcome_set_id
  WHERE database_id = '@database_id'
  AND analysis_id = @analysis_id
  AND outcome_id = @outcome_id
  "
  connectionHandler$queryDb(
    sql,
    database_schema = resultDatabaseSettings$schema,
    table_prefix = resultDatabaseSettings$tablePrefix,
    database_id = databaseId,
    analysis_id = analysisId,
    outcome_id = outcomeId,
    #exposure_id = exposureId,
    snakeCaseToCamelCase = TRUE
  )
}

getSccsTimeToEvent <- function(connectionHandler,
                               resultDatabaseSettings,
                               exposureId,
                               outcomeId,
                               covariateId,
                               databaseId,
                               analysisId) {
  
  sql <- "
  SELECT ds.pre_exposure_p
  FROM @database_schema.@table_prefixdiagnostics_summary ds
  inner join
  @database_schema.@table_prefixexposures_outcome_set eos
  on ds.exposures_outcome_set_id = eos.exposures_outcome_set_id
  WHERE ds.database_id = '@database_id'
  AND ds.covariate_id  = @covariate_id 
  AND ds.analysis_id = @analysis_id
  AND eos.outcome_id = @outcome_id
  "
  
  p <- connectionHandler$queryDb(
    sql,
    database_schema = resultDatabaseSettings$schema,
    table_prefix = resultDatabaseSettings$tablePrefix,
    database_id = databaseId,
    analysis_id = analysisId,
    outcome_id = outcomeId,
    covariate_id = covariateId,
    snakeCaseToCamelCase = TRUE
  )
  
  sql <- "
  SELECT tte.*, @p as p
  FROM @database_schema.@table_prefixtime_to_event tte
  inner join
  @database_schema.@table_prefixexposures_outcome_set eos
  on tte.exposures_outcome_set_id = eos.exposures_outcome_set_id
  WHERE tte.database_id = '@database_id'
  AND tte.era_id  = @exposure_id 
  AND tte.analysis_id = @analysis_id
  AND eos.outcome_id = @outcome_id
  "

  timeToEvent <- connectionHandler$queryDb(
    sql,
    database_schema = resultDatabaseSettings$schema,
    table_prefix = resultDatabaseSettings$tablePrefix,
    database_id = databaseId,
    analysis_id = analysisId,
    outcome_id = outcomeId,
    exposure_id = exposureId,
    p = ifelse(is.null(p$preExposureP), -1, p$preExposureP),
    snakeCaseToCamelCase = TRUE
  )
    
    return(timeToEvent)
}



getSccsAttrition <- function(connectionHandler,
                             resultDatabaseSettings,
                             #exposuresId,
                             outcomeId,
                             databaseId,
                             analysisId,
                             covariateId) {
  sql <- "
  SELECT a.*
  FROM @database_schema.@table_prefixattrition a
  inner join
  @database_schema.@table_prefixexposures_outcome_set eos
  on a.exposures_outcome_set_id = eos.exposures_outcome_set_id
  
  WHERE a.database_id = '@database_id'
  AND a.analysis_id = @analysis_id
  AND eos.outcome_id = @outcome_id
  AND a.covariate_id = @covariate_id
  "
  connectionHandler$queryDb(sql,
                            database_schema = resultDatabaseSettings$schema,
                            table_prefix = resultDatabaseSettings$tablePrefix,
                            database_id = databaseId,
                            analysis_id = analysisId,
                            outcome_id = outcomeId,
                            covariate_id = covariateId,
                            snakeCaseToCamelCase = TRUE)
}

getSccsEventDepObservation <- function(connectionHandler,
                                       resultDatabaseSettings,
                                       outcomeId,
                                       databaseId,
                                       analysisId) {
  sql <- "
  SELECT o.*
  FROM @database_schema.@table_prefixevent_dep_observation o
    inner join
  @database_schema.@table_prefixexposures_outcome_set eos
  on o.exposures_outcome_set_id = eos.exposures_outcome_set_id
  
  WHERE o.database_id = '@database_id'
  AND o.analysis_id = @analysis_id
  AND eos.outcome_id = @outcome_id
  "
  connectionHandler$queryDb(sql,
                            database_schema = resultDatabaseSettings$schema,
                            table_prefix = resultDatabaseSettings$tablePrefix,
                            database_id = databaseId,
                            analysis_id = analysisId,
                            outcome_id = outcomeId,
                            snakeCaseToCamelCase = TRUE)
}

getSccsAgeSpanning <- function(connectionHandler,
                               resultDatabaseSettings,
                               outcomeId,
                               databaseId,
                               analysisId) {
  sql <- "
  SELECT asp.*
  FROM @database_schema.@table_prefixage_spanning asp
      inner join
  @database_schema.@table_prefixexposures_outcome_set eos
  on asp.exposures_outcome_set_id = eos.exposures_outcome_set_id
  
  WHERE asp.database_id = '@database_id'
  AND asp.analysis_id = @analysis_id
  AND eos.outcome_id = @outcome_id
  "
  connectionHandler$queryDb(sql,
                            database_schema = resultDatabaseSettings$schema,
                            table_prefix = resultDatabaseSettings$tablePrefix,
                            database_id = databaseId,
                            analysis_id = analysisId,
                            outcome_id = outcomeId,
                            snakeCaseToCamelCase = TRUE)
}

getSccsCalendarTimeSpanning <- function(connectionHandler,
                                        resultDatabaseSettings,
                                        outcomeId,
                                        databaseId,
                                        analysisId) {
  sql <- "
  SELECT ts.*
  FROM @database_schema.@table_prefixcalendar_time_spanning ts
        inner join
  @database_schema.@table_prefixexposures_outcome_set eos
  on ts.exposures_outcome_set_id = eos.exposures_outcome_set_id
  
  WHERE ts.database_id = '@database_id'
  AND ts.analysis_id = @analysis_id
  AND eos.outcome_id = @outcome_id
  "
  connectionHandler$queryDb(sql,
                            database_schema = resultDatabaseSettings$schema,
                            table_prefix = resultDatabaseSettings$tablePrefix,
                            database_id = databaseId,
                            analysis_id = analysisId,
                            outcome_id = outcomeId,
                            snakeCaseToCamelCase = TRUE)
}

getSccsSpline <- function(connectionHandler,
                          resultDatabaseSettings,
                          outcomeId,
                          databaseId,
                          analysisId,
                          splineType = "age") {

  sql <- "
  SELECT s.*
  FROM @database_schema.@table_prefixspline s
          inner join
  @database_schema.@table_prefixexposures_outcome_set eos
  on s.exposures_outcome_set_id = eos.exposures_outcome_set_id
  
  WHERE s.database_id = '@database_id'
  AND s.analysis_id = @analysis_id
  AND eos.outcome_id = @outcome_id
  AND s.spline_type = '@spline_type';
  "
  connectionHandler$queryDb(sql,
                            database_schema = resultDatabaseSettings$schema,
                            table_prefix = resultDatabaseSettings$tablePrefix,
                            database_id = databaseId,
                            spline_type = splineType,
                            analysis_id = analysisId,
                            outcome_id = outcomeId,
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
                                      outcomeId,
                                      databaseId,
                                      analysisId,
                                      covariateId) {
  sql <- "
  SELECT ds.*
  FROM @database_schema.@table_prefixdiagnostics_summary ds
            inner join
  @database_schema.@table_prefixexposures_outcome_set eos
  on ds.exposures_outcome_set_id = eos.exposures_outcome_set_id
  WHERE ds.database_id = '@database_id'
  AND ds.analysis_id = @analysis_id
  AND ds.covariate_id = @covariate_id
  AND eos.outcome_id = @outcome_id
  "
  connectionHandler$queryDb(sql,
                            database_schema = resultDatabaseSettings$schema,
                            table_prefix = resultDatabaseSettings$tablePrefix,
                            database_id = databaseId,
                            covariate_id = covariateId,
                            analysis_id = analysisId,
                            outcome_id = outcomeId,
                            snakeCaseToCamelCase = TRUE)

}



getSccsAllDiagnosticsSummary <- function(
    connectionHandler,
    resultDatabaseSettings
) {
  sql <- "
  SELECT 
  d.cdm_source_abbreviation as database_name,
  c.cohort_name as outcome, 
  c2.cohort_name as exposure,
  a.description as analysis,
  ds.*
  FROM @database_schema.@table_prefixdiagnostics_summary ds
            inner join
  @database_schema.@table_prefixexposures_outcome_set eos
  on ds.exposures_outcome_set_id = eos.exposures_outcome_set_id
     inner join
   @database_schema.@cg_table_prefixcohort_definition as c
   on c.cohort_definition_id = eos.outcome_id
   
   INNER JOIN
  @database_schema.@database_table_prefix@database_table d
  on d.database_id = ds.database_id
  
  INNER JOIN
  @database_schema.@table_prefixanalysis a
  on a.analysis_id = ds.analysis_id
  
  INNER JOIN
  @database_schema.@table_prefixcovariate cov
  on cov.covariate_id = ds.covariate_id and 
  cov.exposures_outcome_set_id = ds.exposures_outcome_set_id and
  cov.analysis_id = ds.analysis_id and
  cov.database_id = ds.database_id
  
   inner join
   @database_schema.@cg_table_prefixcohort_definition as c2
   on cov.era_id = c2.cohort_definition_id
  ;
  "
  result <- connectionHandler$queryDb(sql,
                            database_schema = resultDatabaseSettings$schema,
                            cg_table_prefix = resultDatabaseSettings$cohortTablePrefix,
                            table_prefix = resultDatabaseSettings$tablePrefix,
                            database_table_prefix = resultDatabaseSettings$databaseTablePrefix,
                            database_table = resultDatabaseSettings$databaseTable,
                            snakeCaseToCamelCase = TRUE)
  
  result <- result %>% 
    dplyr::select(-c("analysisId","exposuresOutcomeSetId","databaseId","covariateId"))
  
  return(result)
  
}

