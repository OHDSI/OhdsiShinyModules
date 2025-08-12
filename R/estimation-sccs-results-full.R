estimationSccsFullResultViewer <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::div(
    
    # add selection module
    inputSelectionDfViewer(
      id = ns("input-selection-df"),
      title = 'Result Selected'
      ),

    shiny::tabsetPanel(
      id = ns("fullTabsetPanel"), 
      type = 'pills',

        shiny::tabPanel(
          "Power",
          shiny::div(shiny::strong("Table 1."), "For each variable of interest: the number of cases (people with at least one outcome), the number of years those people were observed, the number of outcomes, the number of subjects with at least one exposure, the number of patient-years exposed, the number of outcomes while exposed, and the minimum detectable relative risk (MDRR)."),
          shinyWidgets::addSpinner(
            resultTableViewer(ns('powerTable'))
          )
        ),
        shiny::tabPanel(
          "Attrition",
          shinyWidgets::addSpinner(
            shiny::plotOutput(ns("attritionPlot"), width = 600, height = 500)
            ),
          shiny::div(
            shiny::strong("Figure 1."),
            "Attrition, showing the number of cases (number of subjects with at least one outcome), and number of outcomes (number of ocurrences of the outcome) after each step in the study.")
        ),
        shiny::tabPanel(
          "Model",
          shiny::tabsetPanel(
            id = ns("modelTabsetPanel"),
            shiny::tabPanel(
              "Model coefficients",
              shiny::div(
                shiny::strong("Table 2."),
                "The fitted non-zero coefficent (incidence rate ratio) and 95 percent confidence interval for all variables in the model."
              ),
              shiny::tableOutput(ns("modelTable"))
            ),
            shiny::tabPanel(
              "Age spline",
              shinyWidgets::addSpinner(
                shiny::plotOutput(ns("ageSplinePlot"))
                ),
              shiny::div(shiny::strong("Figure 2a."), "Spline fitted for age.")
            ),
            shiny::tabPanel(
              "Season spline",
              shinyWidgets::addSpinner(
                shiny::plotOutput(ns("seasonSplinePlot"))
                ),
              shiny::div(shiny::strong("Figure 2b."), "Spline fitted for season")
            ),
            shiny::tabPanel(
              "Calendar time spline",
              shinyWidgets::addSpinner(
                shiny::plotOutput(ns("calendarTimeSplinePlot"))
                ),
              shiny::div(shiny::strong("Figure 2c."), "Spline fitted for calendar time")
            )
          )
        ),
        shiny::tabPanel(
          "Spanning",
          shiny::radioButtons(ns("spanningType"), label = "Type:", choices = c("Age", "Calendar time")),
          shinyWidgets::addSpinner(
            shiny::plotOutput(ns("spanningPlot"))
            ),
          shiny::div(shiny::strong("Figure 3."), "Number of subjects observed for 3 consecutive months, centered on the indicated month.")
        ),
        shiny::tabPanel(
          "Time trend",
          shinyWidgets::addSpinner(
            shiny::plotOutput(ns("timeTrendPlot"), height = 600)
            ),
          shiny::div(
            shiny::strong("Figure 4."),
            "The ratio of observed to expected outcomes per month. The expected count is computing either assuming a constant rate (bottom plot) or adjusting for calendar time, seasonality, and / or age, as specified in the model (top plot)."
          )
        ),
        shiny::tabPanel(
          "Time to event",
          shinyWidgets::addSpinner(
            shiny::plotOutput(ns("timeToEventPlot"))
            ),
          shiny::div(
            shiny::strong("Figure 5."),
            "The number of events and subjects observed per week relative to the start of the first exposure (indicated by the thick vertical line)."
          )
        ),
        shiny::tabPanel(
          "Event dep. observation",
          shinyWidgets::addSpinner(
            shiny::plotOutput(ns("eventDepObservationPlot"))
            ),
          shiny::div(shiny::strong("Figure 6."), "Histograms for the number of months between the first occurrence of the outcome and the end of observation, stratified by whether the end of observation was censored (inferred as not being equal to the end of database time), or uncensored (inferred as having the subject still be observed at the end of database time)."
          )
        ),
        shiny::tabPanel(
          "Systematic error",
          shinyWidgets::addSpinner(
            shiny::plotOutput(ns("controlEstimatesPlot"))
            ),
          shiny::div(shiny::strong("Figure 7."), "Systematic error. Effect size estimates for the negative controls (true incidence rate ratio = 1)
                                                                                    and positive controls (true incidence rate ratio > 1), before and after calibration. Estimates below the diagonal dashed
                                                                                    lines are statistically significant (alpha = 0.05) different from the true effect size. A well-calibrated
                                                                                    estimator should have the true effect size within the 95 percent confidence interval 95 percent of times.")
        )
  )
  
  )
  
}


estimationSccsFullResultServer <- function(
    id,
    connectionHandler,
    resultDatabaseSettings,
    selectedRow,
    actionCount
) {
  
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      # reset the tab when a new result is selected
      shiny::observeEvent(actionCount(), {
      shiny::updateTabsetPanel(session, "fullTabsetPanel", selected = "Power")
    })
      
      # show what was selected
      modifiedRow <- shiny::reactive({
        selectedRow() %>%
          dplyr::select(
            "covariateName",
            'indication',
            "outcome",
            "description",
            "databaseName"
          ) %>%
          dplyr::rename(
            Indication = "indication",
            Outcome = "outcome",
            Analysis = "description",
            Database = "databaseName"
          )
      })
      
      inputSelectionDfServer(
        id = "input-selection-df", 
        dataFrameRow = modifiedRow,
        ncol = 2
        )
      
      powerTable <- shiny::reactive({
        row <- selectedRow()
        if (is.null(row)) {
          return(NULL)
        } else {
          resTargetTable <- row %>%
            dplyr::mutate(outcomeEvents = ifelse(.data$unblind == 1, .data$outcomeEvents, NA)) %>%
            dplyr::select(
              "covariateName",
              "outcomeSubjects",
              "observedDays",
              "outcomeEvents",
              "covariateSubjects",
              "covariateDays",
              "covariateOutcomes",
              "mdrr"
            ) %>%
            dplyr::mutate(observedDays = .data$observedDays / 365.25,
                          covariateDays = .data$covariateDays / 365.25)
          
          return(resTargetTable)
        }
      })
      
      colDefsInput <- list(
        covariateName = reactable::colDef( 
          name = "Variable", 
          header = withTooltip(
            "Variable", 
            "The covariate"
          )),
        outcomeSubjects = reactable::colDef( 
          name = "Cases", 
          header = withTooltip(
            "Cases", 
            "The number of cases"
          )),
        observedDays = reactable::colDef( 
          name = "Years observed", 
          format = reactable::colFormat(digits = 2),
          header = withTooltip(
            "Years observed", 
            "The total years observed"
          )),
        outcomeEvents = reactable::colDef( 
          name = "Outcomes", 
          header = withTooltip(
            "Outcomes", 
            "The total number of outcomes"
          )),
        covariateSubjects = reactable::colDef( 
          name = "Persons exposed", 
          header = withTooltip(
            "Persons exposed", 
            "The total number of people exposed"
          )),
        covariateDays = reactable::colDef( 
          name = "Years exposed",
          format = reactable::colFormat(digits = 2),
          header = withTooltip(
            "Years exposed", 
            "The total number of years exposed"
          )),
        covariateOutcomes = reactable::colDef( 
          name = "Outcomes while exposed", 
          header = withTooltip(
            "Outcomes while exposed", 
            "The total number of outcomes while exposed"
          )),
        mdrr = reactable::colDef( 
          name = "MDRR", 
          format = reactable::colFormat(digits = 4),
          header = withTooltip(
            "MDRR", 
            "The minimal detectable relative risk"
          ))
      )
  
      # move these to a different submodule?
      resultTableServer(
        id = "powerTable", # how is this working without session$ns
        df = powerTable,
        colDefsInput = colDefsInput,
        elementId = session$ns('powerTable')
      )
        
      output$attritionPlot <- shiny::renderPlot({
        
        row <- selectedRow()
        if (is.null(row)) {
          return(NULL)
        } else {
          attrition <- estimationGetSccsAttrition(
            connectionHandler = connectionHandler,
            resultDatabaseSettings = resultDatabaseSettings,
            databaseId = row$databaseId,
            analysisId = row$analysisId,
            covariateId = row$covariateId,
            exposuresOutcomeSetId = row$exposuresOutcomeSetId
          )
          drawAttritionDiagram(attrition)
        }
      })
      
      output$modelTable <- shiny::renderTable({
        row <- selectedRow()
        if (is.null(row)) {
          return(NULL)
        } else {
          resTargetTable <- estimationGetSccsModel(
            connectionHandler = connectionHandler,
            resultDatabaseSettings = resultDatabaseSettings,
            exposureId = row$eraId,
            exposuresOutcomeSetId = row$exposuresOutcomeSetId,
            databaseId = row$databaseId,
            analysisId = row$analysisId
          )
          
          resTargetTable <- resTargetTable %>%
            dplyr::arrange(.data$covariateId) %>%
            dplyr::select(-"covariateId")
          
          colnames(resTargetTable) <- c("Variable",
                                        "IRR",
                                        "LB",
                                        "UB")
          return(resTargetTable)
        }
      })
      
      output$timeTrendPlot <- shiny::renderPlot({
        row <- selectedRow()
        if (is.null(row)) {
          return(NULL)
        } else {
          timeTrend <- estimationGetSccsTimeTrend(
            connectionHandler = connectionHandler,
            resultDatabaseSettings = resultDatabaseSettings,
            exposureId = row$eraId,
            exposuresOutcomeSetId = row$exposuresOutcomeSetId,
            databaseId = row$databaseId,
            analysisId = row$analysisId
          )

          if (all(c(hasData(timeTrend$ratio), hasData(timeTrend$adjustedRatio)))) {
            plotTimeTrend(timeTrend)
          } else {
            plotTimeTrendStability(timeTrend)
          }
        }
      })
      
      output$timeToEventPlot <- shiny::renderPlot({
        row <- selectedRow()
        if (is.null(row)) {
          return(NULL)
        } else {
          timeToEvent <- estimationGetSccsTimeToEvent(
            connectionHandler = connectionHandler,
            resultDatabaseSettings = resultDatabaseSettings,
            exposuresOutcomeSetId = row$exposuresOutcomeSetId,
            exposureId = row$eraId,
            covariateId = row$covariateId,
            databaseId = row$databaseId,
            analysisId = row$analysisId
          )
          plotTimeToEventSccs(timeToEvent)
        }
      })
      
      output$eventDepObservationPlot <- shiny::renderPlot({
        row <- selectedRow()
        if (is.null(row)) {
          return(NULL)
        } else {
          eventDepObservation <- estimationGetSccsEventDepObservation(
            connectionHandler = connectionHandler,
            resultDatabaseSettings = resultDatabaseSettings,
            exposuresOutcomeSetId = row$exposuresOutcomeSetId,
            databaseId = row$databaseId,
            analysisId = row$analysisId
          )
          plotEventDepObservation(eventDepObservation)
        }
      })
      
      output$spanningPlot <- shiny::renderPlot({
        row <- selectedRow()
        if (is.null(row)) {
          return(NULL)
        } else {
          if (input$spanningType == "Age") {
            ageSpanning <- estimationGetSccsAgeSpanning(
              connectionHandler = connectionHandler,
              resultDatabaseSettings = resultDatabaseSettings,
              exposuresOutcomeSetId = row$exposuresOutcomeSetId,
              databaseId = row$databaseId,
              analysisId = row$analysisId
            )
            plotSpanning(ageSpanning, type = "age")
          } else {
            calendarTimeSpanning <- estimationGetSccsCalendarTimeSpanning(
              connectionHandler = connectionHandler,
              resultDatabaseSettings = resultDatabaseSettings,
              exposuresOutcomeSetId = row$exposuresOutcomeSetId,
              databaseId = row$databaseId,
              analysisId = row$analysisId
            )
            plotSpanning(calendarTimeSpanning, type = "calendar time")
          }
        }
      })
      
      output$ageSplinePlot <- shiny::renderPlot({
        
        row <- selectedRow()
        if (is.null(row)) {
          return(NULL)
        } else {
          ageSpline <- estimationGetSccsSpline(
            connectionHandler = connectionHandler,
            resultDatabaseSettings = resultDatabaseSettings,
            exposuresOutcomeSetId = row$exposuresOutcomeSetId,
            databaseId = row$databaseId,
            analysisId = row$analysisId,
            splineType = "age"
          )
          if (nrow(ageSpline) == 0) {
            return(NULL)
          }
          plotAgeSpline(ageSpline)
        }
      })
      
      output$seasonSplinePlot <- shiny::renderPlot({
        row <- selectedRow()
        if (is.null(row)) {
          return(NULL)
        } else {
          seasonSpline <- estimationGetSccsSpline(
            connectionHandler = connectionHandler,
            resultDatabaseSettings = resultDatabaseSettings,
            exposuresOutcomeSetId = row$exposuresOutcomeSetId,
            databaseId = row$databaseId,
            analysisId = row$analysisId,
            splineType = "season"
          )
          if (nrow(seasonSpline) == 0) {
            return(NULL)
          }
          plotSeasonSpline(seasonSpline)
        }
      })
      
      output$calendarTimeSplinePlot <- shiny::renderPlot({
        row <- selectedRow()
        if (is.null(row)) {
          return(NULL)
        } else {
          calendarTimeSpline <- estimationGetSccsSpline(
            connectionHandler = connectionHandler,
            resultDatabaseSettings = resultDatabaseSettings,
            exposuresOutcomeSetId = row$exposuresOutcomeSetId,
            databaseId = row$databaseId,
            analysisId = row$analysisId,
            splineType = "calendar time"
          )
          if (nrow(calendarTimeSpline) == 0) {
            return(NULL)
          }
          plotCalendarTimeSpline(calendarTimeSpline)
        }
      })
      
      output$controlEstimatesPlot <- shiny::renderPlot({
        row <- selectedRow()
        if (is.null(row)) {
          return(NULL)
        } else {
          controlEstimates <- estimationGetSccsControlEstimates(
            connectionHandler = connectionHandler,
            resultDatabaseSettings = resultDatabaseSettings,
            covariateId = row$covariateId,
            databaseId = row$databaseId,
            analysisId = row$analysisId,
            indicationId = row$indicationId,
            eraId = row$eraId,
            covariateAnalysisId = row$covariateAnalysisId,
            exposuresOutcomeSetId = row$exposuresOutcomeSetId
          )
          plotControlEstimates(
            controlEstimates = controlEstimates$plotResult,
            ease = controlEstimates$ease
            )
        }
      })
      
    }
  )
}


estimationGetSccsAttrition <- function(
    connectionHandler,
    resultDatabaseSettings,
    databaseId,
    analysisId,
    covariateId,
    exposuresOutcomeSetId
) {
  sql <- "
  SELECT *
  FROM @schema.@sccs_table_prefixattrition

  WHERE database_id = '@database_id'
  AND analysis_id = @analysis_id
  AND exposures_outcome_set_id = @exposures_outcome_set_id
  AND covariate_id = @covariate_id
  "
  connectionHandler$queryDb(
    sql,
    schema = resultDatabaseSettings$schema,
    sccs_table_prefix = resultDatabaseSettings$sccsTablePrefix,
    database_id = databaseId,
    analysis_id = analysisId,
    covariate_id = covariateId,
    exposures_outcome_set_id = exposuresOutcomeSetId,
    snakeCaseToCamelCase = TRUE
  )
}


estimationGetSccsModel <- function(
    connectionHandler,
    resultDatabaseSettings,
    exposuresOutcomeSetId,
    databaseId,
    analysisId,
    exposureId
) {
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

  WHERE scr.database_id = '@database_id'
  AND scr.analysis_id = @analysis_id
  --AND  sc.era_id = @exposure_id
  --AND scr.rr IS NOT NULL
  AND scr.exposures_outcome_set_id = @exposures_outcome_set_id
  "
  
  connectionHandler$queryDb(sql,
                            schema = resultDatabaseSettings$schema,
                            sccs_table_prefix = resultDatabaseSettings$sccsTablePrefix,
                            cg_table_prefix = resultDatabaseSettings$cgTablePrefix,
                            database_id = databaseId,
                            analysis_id = analysisId,
                            exposure_id = exposureId,
                            exposures_outcome_set_id = exposuresOutcomeSetId,
                            snakeCaseToCamelCase = TRUE)
}


estimationGetSccsTimeTrend <- function(
    connectionHandler,
    resultDatabaseSettings,
    exposureId,
    exposuresOutcomeSetId,
    databaseId,
    analysisId
) {
  sql <- "
  SELECT *
  FROM @schema.@sccs_table_prefixtime_trend
  WHERE database_id = '@database_id'
  AND analysis_id = @analysis_id
  AND exposures_outcome_set_id = @exposures_outcome_set_id
  "
  connectionHandler$queryDb(
    sql,
    schema = resultDatabaseSettings$schema,
    sccs_table_prefix = resultDatabaseSettings$sccsTablePrefix,
    database_id = databaseId,
    analysis_id = analysisId,
    exposures_outcome_set_id = exposuresOutcomeSetId,
    snakeCaseToCamelCase = TRUE
  )
}

estimationGetSccsTimeToEvent <- function(
    connectionHandler,
    resultDatabaseSettings,
    exposureId,
    exposuresOutcomeSetId,
    covariateId,
    databaseId,
    analysisId
) {
  
  sql <- "
  SELECT pre_exposure_p
  FROM @schema.@sccs_table_prefixdiagnostics_summary
  
  WHERE database_id = '@database_id'
  AND covariate_id  = @covariate_id 
  AND analysis_id = @analysis_id
  AND exposures_outcome_set_id = @exposures_outcome_set_id
  "
  
  p <- connectionHandler$queryDb(
    sql,
    schema = resultDatabaseSettings$schema,
    sccs_table_prefix = resultDatabaseSettings$sccsTablePrefix,
    database_id = databaseId,
    analysis_id = analysisId,
    exposures_outcome_set_id = exposuresOutcomeSetId,
    covariate_id = covariateId,
    snakeCaseToCamelCase = TRUE
  )
  
  # if NULL set to NA so code below works
  if(is.null(p$preExposureP)){
    p$preExposureP <- NA
  }
  
  sql <- "
  SELECT * , @p as p
  FROM @schema.@sccs_table_prefixtime_to_event

  WHERE database_id = '@database_id'
  AND era_id  = @exposure_id 
  AND analysis_id = @analysis_id
  AND exposures_outcome_set_id = @exposures_outcome_set_id;
  "
  
  timeToEvent <- connectionHandler$queryDb(
    sql,
    schema = resultDatabaseSettings$schema,
    sccs_table_prefix = resultDatabaseSettings$sccsTablePrefix,
    database_id = databaseId,
    analysis_id = analysisId,
    exposures_outcome_set_id = exposuresOutcomeSetId,
    exposure_id = exposureId,
    p = ifelse(is.na(p$preExposureP), -1, p$preExposureP),
    snakeCaseToCamelCase = TRUE
  )
  
  
  return(timeToEvent)
}


estimationGetSccsEventDepObservation <- function(
    connectionHandler,
    resultDatabaseSettings,
    exposuresOutcomeSetId,
    databaseId,
    analysisId
) {
  sql <- "
  SELECT *
  FROM @schema.@sccs_table_prefixevent_dep_observation
  
  WHERE database_id = '@database_id'
  AND analysis_id = @analysis_id
  AND exposures_outcome_set_id = @exposures_outcome_set_id;
  "
  connectionHandler$queryDb(
    sql,
    schema = resultDatabaseSettings$schema,
    sccs_table_prefix = resultDatabaseSettings$sccsTablePrefix,
    database_id = databaseId,
    analysis_id = analysisId,
    exposures_outcome_set_id = exposuresOutcomeSetId,
    snakeCaseToCamelCase = TRUE
  )
}


estimationGetSccsAgeSpanning <- function(
    connectionHandler,
    resultDatabaseSettings,
    exposuresOutcomeSetId,
    databaseId,
    analysisId
) {
  sql <- "
  SELECT *
  FROM @schema.@sccs_table_prefixage_spanning
  
  WHERE database_id = '@database_id'
  AND analysis_id = @analysis_id
  AND exposures_outcome_set_id = @exposures_outcome_set_id
  "
  connectionHandler$queryDb(
    sql,
    schema = resultDatabaseSettings$schema,
    sccs_table_prefix = resultDatabaseSettings$sccsTablePrefix,
    database_id = databaseId,
    analysis_id = analysisId,
    exposures_outcome_set_id = exposuresOutcomeSetId,
    snakeCaseToCamelCase = TRUE
  )
}

estimationGetSccsCalendarTimeSpanning <- function(
    connectionHandler,
    resultDatabaseSettings,
    exposuresOutcomeSetId,
    databaseId,
    analysisId
) {
  sql <- "
  SELECT *
  FROM @schema.@sccs_table_prefixcalendar_time_spanning
  
  WHERE database_id = '@database_id'
  AND analysis_id = @analysis_id
  AND exposures_outcome_set_id = @exposures_outcome_set_id
  "
  connectionHandler$queryDb(
    sql,
    schema = resultDatabaseSettings$schema,
    sccs_table_prefix = resultDatabaseSettings$sccsTablePrefix,
    database_id = databaseId,
    analysis_id = analysisId,
    exposures_outcome_set_id = exposuresOutcomeSetId,
    snakeCaseToCamelCase = TRUE
  )
}

estimationGetSccsSpline <- function(
    connectionHandler,
    resultDatabaseSettings,
    exposuresOutcomeSetId,
    databaseId,
    analysisId,
    splineType = "age"
) {
  
  sql <- "
  SELECT *
  FROM @schema.@sccs_table_prefixspline 

  WHERE database_id = '@database_id'
  AND analysis_id = @analysis_id
  AND exposures_outcome_set_id = @exposures_outcome_set_id
  AND spline_type = '@spline_type';
  "
  connectionHandler$queryDb(
    sql,
    schema = resultDatabaseSettings$schema,
    sccs_table_prefix = resultDatabaseSettings$sccsTablePrefix,
    database_id = databaseId,
    spline_type = splineType,
    analysis_id = analysisId,
    exposures_outcome_set_id = exposuresOutcomeSetId,
    snakeCaseToCamelCase = TRUE
  )
}



estimationGetSccsControlEstimates <- function(
    connectionHandler,
    resultDatabaseSettings,
    databaseId,
    analysisId,
    covariateId,
    eraId,
    indicationId = NULL,
    covariateAnalysisId,
    exposuresOutcomeSetId = NULL
) {
  
  # convert any NA values to NULL
  if(!is.null(indicationId)){
    if(is.na(indicationId)){
      indicationId <- NULL
    }
  }
  
  sql <- "
  SELECT r.ci_95_lb, r.ci_95_ub, r.log_rr, r.se_log_rr, 
  r.calibrated_ci_95_lb, r.calibrated_ci_95_ub, r.calibrated_log_rr,
  r.calibrated_se_log_rr, r.exposures_outcome_set_id,
  e.true_effect_size, c.exposures_outcome_set_id,
  eos.nesting_cohort_id as indication_id,
  e.era_id
  
  FROM 
  @schema.@sccs_table_prefixresult r
  INNER JOIN
   @schema.@sccs_table_prefixexposure e
   on r.exposures_outcome_set_id = e.exposures_outcome_set_id

   INNER JOIN 
   @schema.@sccs_table_prefixcovariate c
   on e.era_id = c.era_id 
   and e.exposures_outcome_set_id = c.exposures_outcome_set_id
   and c.database_id = r.database_id
   and c.analysis_id = r.analysis_id
   and c.covariate_id = r.covariate_id
   
   INNER JOIN  
   @schema.@sccs_table_prefixexposures_outcome_set eos
   on eos.exposures_outcome_set_id = r.exposures_outcome_set_id
   
   WHERE r.database_id = '@database_id'
   AND r.analysis_id = @analysis_id
   AND r.covariate_id = @covariate_id
   AND e.true_effect_size is not NULL
  {@use_indication_id}?{AND eos.nesting_cohort_id = @indication_id}
   AND c.covariate_analysis_id = @covariate_analysis_id
   
   AND e.era_id = @era_id
  ;
  "
  
  res <- connectionHandler$queryDb(
    sql,
    schema = resultDatabaseSettings$schema,
    sccs_table_prefix = resultDatabaseSettings$sccsTablePrefix,
    database_id = databaseId,
    covariate_id = covariateId,
    analysis_id = analysisId,
    era_id = eraId,
    use_indication_id = !is.null(indicationId),
    indication_id = indicationId,
    covariate_analysis_id = covariateAnalysisId,
    snakeCaseToCamelCase = TRUE
  )
  

  # get ease for the plot
  sql <- "SELECT top 1 ds.ease
  FROM @schema.@sccs_table_prefixdiagnostics_summary ds

  WHERE ds.database_id = '@database_id'
  AND ds.analysis_id = @analysis_id
  AND ds.covariate_id = @covariate_id
  
  {@use_exposures_outcome_set_id} ? {AND ds.exposures_outcome_set_id = '@exposures_outcome_set_id'}
  ;"
  
  ease <- connectionHandler$queryDb(
    sql,
    schema = resultDatabaseSettings$schema,
    sccs_table_prefix = resultDatabaseSettings$sccsTablePrefix,
    database_id = databaseId,
    covariate_id = covariateId,
    analysis_id = analysisId,
    snakeCaseToCamelCase = TRUE,
    use_exposures_outcome_set_id = !is.null(exposuresOutcomeSetId),
    exposures_outcome_set_id = exposuresOutcomeSetId
  )

  return(list(
    plotResult = res,
    ease = ease$ease
    )
  )
}
