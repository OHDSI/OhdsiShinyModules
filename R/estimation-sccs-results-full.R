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
            'indicationName',
            "outcomeName",
            "description",
            "databaseName"
          ) %>%
          dplyr::rename(
            Indication = "indicationName",
            Outcome = "outcomeName",
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
            exposureId = NULL,
            exposuresOutcomeSetId = row$exposuresOutcomeSetId,
            databaseId = row$databaseId,
            analysisId = row$analysisId
          )

          resTargetTable <- resTargetTable %>%
            dplyr::arrange(.data$covariateId) %>%
            dplyr::select("covariateName", "rr", "ci95Lb", "ci95Ub") %>%
            dplyr::rename(
              Variable = "covariateName",
              IRR = "rr",
              LB = "ci95Lb",
              UB = "ci95Ub"
            )
          
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
            exposureId = row$targetId, #row$eraId,
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
            exposureId = row$targetId, #row$eraId,
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
            eraId = row$targetId, #row$eraId,
            covariateAnalysisId = row$covariateAnalysisId,
            exposuresOutcomeSetId = NULL #row$exposuresOutcomeSetId
          )
          plotControlEstimates(
            controlEstimates = controlEstimates,
            ease = controlEstimates$ease[1] # should be single val
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
  
  result <- OhdsiReportGenerator::getSccsTable(
    connectionHandler = connectionHandler,
    table = 'attrition',
    schema = resultDatabaseSettings$schema,
    sccsTablePrefix = resultDatabaseSettings$sccsTablePrefix,
    cgTablePrefix = resultDatabaseSettings$cgTablePrefix,
    databaseTable = resultDatabaseSettings$databaseTable,
    databaseIds = databaseId,
    analysisIds = analysisId,
    covariateIds = covariateId,
    exposureOutcomeIds = exposuresOutcomeSetId
  )

  return(result)
}


estimationGetSccsModel <- function(
    connectionHandler,
    resultDatabaseSettings,
    exposuresOutcomeSetId,
    databaseId,
    analysisId,
    exposureId
) {
  
  result <- OhdsiReportGenerator::getSccsModel(
    connectionHandler = connectionHandler,
    schema = resultDatabaseSettings$schema,
    sccsTablePrefix = resultDatabaseSettings$sccsTablePrefix,
    cgTablePrefix = resultDatabaseSettings$cgTablePrefix, 
    databaseTable = resultDatabaseSettings$databaseTable, 
    databaseIds = databaseId,
    targetIds = exposureId,
    analysisIds = analysisId,
    exposureOutcomeSetIds = exposuresOutcomeSetId
  )
  
  return(result)
}


estimationGetSccsTimeTrend <- function(
    connectionHandler,
    resultDatabaseSettings,
    exposureId,
    exposuresOutcomeSetId,
    databaseId,
    analysisId
) {
  
  result <- OhdsiReportGenerator::getSccsTable(
    connectionHandler = connectionHandler,
    table = 'time_trend',
    schema = resultDatabaseSettings$schema,
    sccsTablePrefix = resultDatabaseSettings$sccsTablePrefix,
    cgTablePrefix = resultDatabaseSettings$cgTablePrefix,
    databaseTable = resultDatabaseSettings$databaseTable,
    databaseIds = databaseId,
    analysisIds = analysisId,
    exposureOutcomeIds = exposuresOutcomeSetId
  )

  return(result)
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
  
  result <- OhdsiReportGenerator::getSccsTimeToEvent(
    connectionHandler = connectionHandler,
    schema = resultDatabaseSettings$schema,
    sccsTablePrefix = resultDatabaseSettings$sccsTablePrefix,
    cgTablePrefix = resultDatabaseSettings$cgTablePrefix,
    databaseTable = resultDatabaseSettings$databaseTable,
    databaseIds = databaseId,
    analysisIds = analysisId,
    exposuresOutcomeSetIds = exposuresOutcomeSetId
  )
  
  return(result)
}


estimationGetSccsEventDepObservation <- function(
    connectionHandler,
    resultDatabaseSettings,
    exposuresOutcomeSetId,
    databaseId,
    analysisId
) {
  
  result <- OhdsiReportGenerator::getSccsTable(
    connectionHandler = connectionHandler,
    table = 'event_dep_observation',
    schema = resultDatabaseSettings$schema,
    sccsTablePrefix = resultDatabaseSettings$sccsTablePrefix,
    cgTablePrefix = resultDatabaseSettings$cgTablePrefix,
    databaseTable = resultDatabaseSettings$databaseTable,
    databaseIds = databaseId,
    analysisIds = analysisId,
    exposureOutcomeIds = exposuresOutcomeSetId
  )
  
  return(result)
  
}


estimationGetSccsAgeSpanning <- function(
    connectionHandler,
    resultDatabaseSettings,
    exposuresOutcomeSetId,
    databaseId,
    analysisId
) {
  
  result <- OhdsiReportGenerator::getSccsTable(
    connectionHandler = connectionHandler,
    table = 'age_spanning',
    schema = resultDatabaseSettings$schema,
    sccsTablePrefix = resultDatabaseSettings$sccsTablePrefix,
    cgTablePrefix = resultDatabaseSettings$cgTablePrefix,
    databaseTable = resultDatabaseSettings$databaseTable,
    databaseIds = databaseId,
    analysisIds = analysisId,
    exposureOutcomeIds = exposuresOutcomeSetId
  )
  
  return(result)
  
}

estimationGetSccsCalendarTimeSpanning <- function(
    connectionHandler,
    resultDatabaseSettings,
    exposuresOutcomeSetId,
    databaseId,
    analysisId
) {
  
  result <- OhdsiReportGenerator::getSccsTable(
    connectionHandler = connectionHandler,
    table = 'calendar_time_spanning',
    schema = resultDatabaseSettings$schema,
    sccsTablePrefix = resultDatabaseSettings$sccsTablePrefix,
    cgTablePrefix = resultDatabaseSettings$cgTablePrefix,
    databaseTable = resultDatabaseSettings$databaseTable,
    databaseIds = databaseId,
    analysisIds = analysisId,
    exposureOutcomeIds = exposuresOutcomeSetId
  )
  
  return(result)

}

estimationGetSccsSpline <- function(
    connectionHandler,
    resultDatabaseSettings,
    exposuresOutcomeSetId,
    databaseId,
    analysisId,
    splineType = "age"
) {
  
  result <- OhdsiReportGenerator::getSccsTable(
    connectionHandler = connectionHandler,
    table = 'spline',
    schema = resultDatabaseSettings$schema,
    sccsTablePrefix = resultDatabaseSettings$sccsTablePrefix,
    cgTablePrefix = resultDatabaseSettings$cgTablePrefix,
    databaseTable = resultDatabaseSettings$databaseTable,
    databaseIds = databaseId,
    analysisIds = analysisId,
    exposureOutcomeIds = exposuresOutcomeSetId
  )
  
  if(nrow(result) > 0){
    # filter to splineType
    result <- result %>% 
      dplyr::filter(.data$splineType == !!splineType)
  }
  
  return(result)

}



estimationGetSccsControlEstimates <- function(
    connectionHandler,
    resultDatabaseSettings,
    databaseId = NULL,
    analysisId = NULL,
    covariateId = NULL,
    eraId = NULL,
    indicationId = NULL,
    covariateAnalysisId = NULL,
    exposuresOutcomeSetId = NULL
) {
  
  # convert any NA values to NULL
  if(!is.null(indicationId)){
    if(is.na(indicationId)){
      indicationId <- NULL
    }
  }
  
  result <- OhdsiReportGenerator::getSccsNegativeControlEstimates(
    connectionHandler = connectionHandler,
    schema = resultDatabaseSettings$schema,
    sccsTablePrefix = resultDatabaseSettings$sccsTablePrefix,
    cgTablePrefix = resultDatabaseSettings$cgTablePrefix, 
    databaseTable = resultDatabaseSettings$databaseTable, 
    databaseIds = databaseId,
    analysisIds = analysisId,
    covariateIds = covariateId,
    covariateAnalysisIds = covariateAnalysisId,
    exposuresOutcomeSetIds = exposuresOutcomeSetId, 
    targetIds = eraId,
    indicationIds = indicationId
  )
  
  return(result)
}
