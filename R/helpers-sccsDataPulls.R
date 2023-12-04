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
   @schema.@sccs_table_prefixexposures_outcome_set as eos
   inner join
   @schema.@cg_table_prefixcohort_definition as c
   on c.cohort_definition_id = eos.outcome_id
   inner join
   @schema.@sccs_table_prefixexposure as e
   on e.exposures_outcome_set_id = eos.exposures_outcome_set_id
   
   --where e.true_effect_size != 1
   ;
  "
  outcomes <- connectionHandler$queryDb(
    sql,
    schema = resultDatabaseSettings$schema,
    cg_table_prefix = resultDatabaseSettings$cgTablePrefix,
    sccs_table_prefix = resultDatabaseSettings$sccsTablePrefix,
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
  c1.cohort_name as name,
  e.era_id as exposure_id,
  eos.*
  
  from @schema.@sccs_table_prefixresult r

  INNER JOIN @schema.@sccs_table_prefixexposures_outcome_set eos
      ON r.exposures_outcome_set_id = eos.exposures_outcome_set_id

  INNER JOIN @schema.@sccs_table_prefixcovariate cov
        ON r.exposures_outcome_set_id = cov.exposures_outcome_set_id
        AND r.analysis_id = cov.analysis_id
        AND r.covariate_id = cov.covariate_id

  INNER JOIN @schema.@sccs_table_prefixexposure e
                ON r.exposures_outcome_set_id = e.exposures_outcome_set_id
                                AND cov.era_id = e.era_id

  INNER JOIN @schema.@cg_table_prefixcohort_definition as c1 on c.cohort_definition_id = r.era_id
  WHERE e.true_effect_size IS NULL
   ;
  "
  exposures <- connectionHandler$queryDb(
    sql,
    schema = resultDatabaseSettings$schema,
    cg_table_prefix = resultDatabaseSettings$cgTablePrefix,
    sccs_table_prefix = resultDatabaseSettings$sccsTablePrefix,
    snakeCaseToCamelCase = TRUE
  )

  # Requires migration from version 5.1.0
  if ("nestingCohortId" %in% colnames(exposures)) {

    getNestedNames  <- function(nestIds) {
      exposures %>% dplyr::filter(.data$exposureId == !!nestId) %>% dplyr::pull("name")
    }

    exposures <-
      exposures %>% dplyr::mutate(
        name = ifelse(
          is.null(.data$nestingCohortId),
          .data$name,
          paste(.data$name, "-", getNestedName(.data$nestingChortId))
        )
      )
  }

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
   @schema.@database_table_prefix@database_table ds 
   inner join
   @schema.@sccs_table_prefixresult as r
   on r.database_id = ds.database_id;
  "
  dbs <- connectionHandler$queryDb(
    sql,
    schema = resultDatabaseSettings$schema,
    database_table_prefix = resultDatabaseSettings$databaseTablePrefix,
    database_table = resultDatabaseSettings$databaseTable,
    sccs_table_prefix = resultDatabaseSettings$sccsTablePrefix,
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
   @schema.@sccs_table_prefixresult as r
   inner join
   @schema.@sccs_table_prefixanalysis as a
   on r.analysis_id = a.analysis_id
   ;
  "
  analyses <- connectionHandler$queryDb(
    sql,
    schema = resultDatabaseSettings$schema,
    sccs_table_prefix = resultDatabaseSettings$sccsTablePrefix,
    snakeCaseToCamelCase = TRUE
  )
  
  result <- analyses$analysisId
  names(result) <- analyses$name
  
  result2 <- unlist(lapply(unique(names(result)), function(x){paste(result[names(result) == x], collapse=',')}))
  names(result2) <- unique(names(result))
  
  return(result2)
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
  FROM @schema.@sccs_table_prefixcovariate_result scr
  INNER JOIN @schema.@sccs_table_prefixcovariate sc ON (
    sc.exposures_outcome_set_id = scr.exposures_outcome_set_id AND
    sc.database_id = scr.database_id AND
    sc.analysis_id = scr.analysis_id AND
    sc.covariate_id = scr.covariate_id
  )
  LEFT JOIN @schema.@cg_table_prefixcohort_definition cd 
  ON cd.cohort_definition_id = sc.era_id
  LEFT JOIN @schema.@sccs_table_prefixera era ON (
    era.exposures_outcome_set_id = scr.exposures_outcome_set_id AND
    era.database_id = scr.database_id AND
    era.analysis_id = scr.analysis_id AND
    era.era_id = sc.era_id
  )
  INNER JOIN @schema.@sccs_table_prefixexposures_outcome_set eos
  ON 
  eos.exposures_outcome_set_id = scr.exposures_outcome_set_id
  
  WHERE scr.database_id = '@database_id'
  AND scr.analysis_id = @analysis_id
  AND eos.outcome_id = @outcome_id
  AND  sc.era_id = @exposure_id
  AND scr.rr IS NOT NULL
  "

  connectionHandler$queryDb(sql,
                            schema = resultDatabaseSettings$schema,
                            sccs_table_prefix = resultDatabaseSettings$sccsTablePrefix,
                            cg_table_prefix = resultDatabaseSettings$cgTablePrefix,
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
  FROM @schema.@sccs_table_prefixtime_trend tt
  inner join
  @schema.@sccs_table_prefixexposures_outcome_set eos
  on tt.exposures_outcome_set_id = eos.exposures_outcome_set_id
  WHERE database_id = '@database_id'
  AND analysis_id = @analysis_id
  AND outcome_id = @outcome_id
  "
  connectionHandler$queryDb(
    sql,
    schema = resultDatabaseSettings$schema,
    sccs_table_prefix = resultDatabaseSettings$sccsTablePrefix,
    database_id = databaseId,
    analysis_id = analysisId,
    outcome_id = outcomeId,
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
  FROM @schema.@sccs_table_prefixdiagnostics_summary ds
  inner join
  @schema.@sccs_table_prefixexposures_outcome_set eos
  on ds.exposures_outcome_set_id = eos.exposures_outcome_set_id
  WHERE ds.database_id = '@database_id'
  AND ds.covariate_id  = @covariate_id 
  AND ds.analysis_id = @analysis_id
  AND eos.outcome_id = @outcome_id
  "
  
  p <- connectionHandler$queryDb(
    sql,
    schema = resultDatabaseSettings$schema,
    sccs_table_prefix = resultDatabaseSettings$sccsTablePrefix,
    database_id = databaseId,
    analysis_id = analysisId,
    outcome_id = outcomeId,
    covariate_id = covariateId,
    snakeCaseToCamelCase = TRUE
  )
  
  sql <- "
  SELECT tte.*, @p as p
  FROM @schema.@sccs_table_prefixtime_to_event tte
  inner join
  @schema.@sccs_table_prefixexposures_outcome_set eos
  on tte.exposures_outcome_set_id = eos.exposures_outcome_set_id
  WHERE tte.database_id = '@database_id'
  AND tte.era_id  = @exposure_id 
  AND tte.analysis_id = @analysis_id
  AND eos.outcome_id = @outcome_id
  "

  timeToEvent <- connectionHandler$queryDb(
    sql,
    schema = resultDatabaseSettings$schema,
    sccs_table_prefix = resultDatabaseSettings$sccsTablePrefix,
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
  FROM @schema.@sccs_table_prefixattrition a
  inner join
  @schema.@sccs_table_prefixexposures_outcome_set eos
  on a.exposures_outcome_set_id = eos.exposures_outcome_set_id
  
  WHERE a.database_id = '@database_id'
  AND a.analysis_id = @analysis_id
  AND eos.outcome_id = @outcome_id
  AND a.covariate_id = @covariate_id
  "
  connectionHandler$queryDb(sql,
                            schema = resultDatabaseSettings$schema,
                            sccs_table_prefix = resultDatabaseSettings$sccsTablePrefix,
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
  FROM @schema.@sccs_table_prefixevent_dep_observation o
    inner join
  @schema.@sccs_table_prefixexposures_outcome_set eos
  on o.exposures_outcome_set_id = eos.exposures_outcome_set_id
  
  WHERE o.database_id = '@database_id'
  AND o.analysis_id = @analysis_id
  AND eos.outcome_id = @outcome_id
  "
  connectionHandler$queryDb(sql,
                            schema = resultDatabaseSettings$schema,
                            sccs_table_prefix = resultDatabaseSettings$sccsTablePrefix,
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
  FROM @schema.@sccs_table_prefixage_spanning asp
      inner join
  @schema.@sccs_table_prefixexposures_outcome_set eos
  on asp.exposures_outcome_set_id = eos.exposures_outcome_set_id
  
  WHERE asp.database_id = '@database_id'
  AND asp.analysis_id = @analysis_id
  AND eos.outcome_id = @outcome_id
  "
  connectionHandler$queryDb(sql,
                            schema = resultDatabaseSettings$schema,
                            sccs_table_prefix = resultDatabaseSettings$sccsTablePrefix,
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
  FROM @schema.@sccs_table_prefixcalendar_time_spanning ts
        inner join
  @schema.@sccs_table_prefixexposures_outcome_set eos
  on ts.exposures_outcome_set_id = eos.exposures_outcome_set_id
  
  WHERE ts.database_id = '@database_id'
  AND ts.analysis_id = @analysis_id
  AND eos.outcome_id = @outcome_id
  "
  connectionHandler$queryDb(sql,
                            schema = resultDatabaseSettings$schema,
                            sccs_table_prefix = resultDatabaseSettings$sccsTablePrefix,
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
  FROM @schema.@sccs_table_prefixspline s
          inner join
  @schema.@sccs_table_prefixexposures_outcome_set eos
  on s.exposures_outcome_set_id = eos.exposures_outcome_set_id
  
  WHERE s.database_id = '@database_id'
  AND s.analysis_id = @analysis_id
  AND eos.outcome_id = @outcome_id
  AND s.spline_type = '@spline_type';
  "
  connectionHandler$queryDb(sql,
                            schema = resultDatabaseSettings$schema,
                            sccs_table_prefix = resultDatabaseSettings$sccsTablePrefix,
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
  FROM @schema.@sccs_table_prefixresult sr
  INNER JOIN @schema.@sccs_table_prefixcovariate sc ON (
    sc.exposures_outcome_set_id = sr.exposures_outcome_set_id AND
    sc.database_id = sr.database_id AND
    sc.analysis_id = sr.analysis_id AND
    sc.covariate_id = sr.covariate_id
  )
  INNER JOIN @schema.@sccs_table_prefixexposure se ON (
    se.exposures_outcome_set_id = sr.exposures_outcome_set_id AND
    se.era_id = sc.era_id
  )
  WHERE sr.database_id = '@database_id'
  AND sr.analysis_id = @analysis_id
  AND sr.covariate_id = @covariate_id
  "
  connectionHandler$queryDb(sql,
                            schema = resultDatabaseSettings$schema,
                            sccs_table_prefix = resultDatabaseSettings$sccsTablePrefix,
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
                                      covariateId,
                                      exposureId) {
  sql <- "
  SELECT ds.*
  FROM @schema.@sccs_table_prefixdiagnostics_summary ds
            inner join
  @schema.@sccs_table_prefixexposures_outcome_set eos
  on ds.exposures_outcome_set_id = eos.exposures_outcome_set_id
  
  inner join
  @schema.@sccs_table_prefixcovariate cov
  on cov.covariate_id = ds.covariate_id and 
  cov.exposures_outcome_set_id = ds.exposures_outcome_set_id and
  cov.analysis_id = ds.analysis_id and
  cov.database_id = ds.database_id
  
  WHERE ds.database_id = '@database_id'
  AND ds.analysis_id = @analysis_id
  AND ds.covariate_id = @covariate_id
  AND eos.outcome_id = @outcome_id
  AND cov.era_id = @exposure_id
  "
  connectionHandler$queryDb(sql,
                            schema = resultDatabaseSettings$schema,
                            sccs_table_prefix = resultDatabaseSettings$sccsTablePrefix,
                            database_id = databaseId,
                            covariate_id = covariateId,
                            analysis_id = analysisId,
                            outcome_id = outcomeId,
                            exposure_id = exposureId,
                            snakeCaseToCamelCase = TRUE)
  
}





