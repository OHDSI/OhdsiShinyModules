sccsFullResultViewer <- function(id) {
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
          resultTableViewer(ns('powerTable'))
        ),
        shiny::tabPanel(
          "Attrition",
          shiny::plotOutput(ns("attritionPlot"), width = 600, height = 500),
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
              shiny::plotOutput(ns("ageSplinePlot")),
              shiny::div(shiny::strong("Figure 2a."), "Spline fitted for age.")
            ),
            shiny::tabPanel(
              "Season spline",
              shiny::plotOutput(ns("seasonSplinePlot")),
              shiny::div(shiny::strong("Figure 2b."), "Spline fitted for season")
            ),
            shiny::tabPanel(
              "Calendar time spline",
              shiny::plotOutput(ns("calendarTimeSplinePlot")),
              shiny::div(shiny::strong("Figure 2c."), "Spline fitted for calendar time")
            )
          )
        ),
        shiny::tabPanel(
          "Spanning",
          shiny::radioButtons(ns("spanningType"), label = "Type:", choices = c("Age", "Calendar time")),
          shiny::plotOutput(ns("spanningPlot")),
          shiny::div(shiny::strong("Figure 3."), "Number of subjects observed for 3 consecutive months, centered on the indicated month.")
        ),
        shiny::tabPanel(
          "Time trend",
          shiny::plotOutput(ns("timeTrendPlot"), height = 600),
          shiny::div(
            shiny::strong("Figure 4."),
            "The ratio of observed to expected outcomes per month. The expected count is computing either assuming a constant rate (bottom plot) or adjusting for calendar time, seasonality, and / or age, as specified in the model (top plot)."
          )
        ),
        shiny::tabPanel(
          "Time to event",
          shiny::plotOutput(ns("timeToEventPlot")),
          shiny::div(
            shiny::strong("Figure 5."),
            "The number of events and subjects observed per week relative to the start of the first exposure (indicated by the thick vertical line)."
          )
        ),
        shiny::tabPanel(
          "Event dep. observation",
          shiny::plotOutput(ns("eventDepObservationPlot")),
          shiny::div(shiny::strong("Figure 6."), "Histograms for the number of months between the first occurrence of the outcome and the end of observation, stratified by whether the end of observation was censored (inferred as not being equal to the end of database time), or uncensored (inferred as having the subject still be observed at the end of database time)."
          )
        ),
        shiny::tabPanel(
          "Systematic error",
          shiny::plotOutput(ns("controlEstimatesPlot")),
          shiny::div(shiny::strong("Figure 7."), "Systematic error. Effect size estimates for the negative controls (true incidence rate ratio = 1)
                                                                                    and positive controls (true incidence rate ratio > 1), before and after calibration. Estimates below the diagonal dashed
                                                                                    lines are statistically significant (alpha = 0.05) different from the true effect size. A well-calibrated
                                                                                    estimator should have the true effect size within the 95 percent confidence interval 95 percent of times.")
        )
  )
  
  )
  
}


sccsFullResultServer <- function(
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
      
      modifiedRow <- shiny::reactive({
        selectedRow() %>%
          dplyr::select(
            "covariateName",
            "outcome",
            "description",
            "databaseName"
          ) %>%
          dplyr::rename(
            'Outcome' = .data$outcome,
            'Analysis' = .data$description,
            'Database' = .data$databaseName
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
          header = withTooltip(
            "Variable", 
            "The covariate"
          )),
        outcomeSubjects = reactable::colDef( 
          header = withTooltip(
            "Cases", 
            "The number of cases"
          )),
        observedDays = reactable::colDef( 
          format = reactable::colFormat(digits = 2),
          header = withTooltip(
            "Years observed", 
            "The total years observed"
          )),
        outcomeEvents = reactable::colDef( 
          header = withTooltip(
            "Outcomes", 
            "The total number of outcomes"
          )),
        covariateSubjects = reactable::colDef( 
          header = withTooltip(
            "Persons exposed", 
            "The total number of people exposed"
          )),
        covariateDays = reactable::colDef( 
          format = reactable::colFormat(digits = 2),
          header = withTooltip(
            "Years exposed", 
            "The total number of years exposed"
          )),
        covariateOutcomes = reactable::colDef( 
          header = withTooltip(
            "Outcomes while exposed", 
            "The total number of outcomes while exposed"
          )),
        mdrr = reactable::colDef( 
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
        colDefsInput = colDefsInput
      )
        
      output$attritionPlot <- shiny::renderPlot({
        
        row <- selectedRow()
        if (is.null(row)) {
          return(NULL)
        } else {
          attrition <- getSccsAttrition(
            connectionHandler = connectionHandler,
            resultDatabaseSettings = resultDatabaseSettings,
            outcomeId = row$outcomeId,
            databaseId = row$databaseId,
            analysisId = row$analysisId,
            covariateId = row$covariateId
          )
          drawAttritionDiagram(attrition)
        }
      })
      
      output$modelTable <- shiny::renderTable({
        row <- selectedRow()
        if (is.null(row)) {
          return(NULL)
        } else {
          resTargetTable <- getSccsModel(
            connectionHandler = connectionHandler,
            resultDatabaseSettings = resultDatabaseSettings,
            exposureId = row$eraId,
            outcomeId = row$outcomeId,
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
          timeTrend <- getSccsTimeTrend(
            connectionHandler = connectionHandler,
            resultDatabaseSettings = resultDatabaseSettings,
            exposureId = row$eraId,
            outcomeId = row$outcomeId,
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
          timeToEvent <- getSccsTimeToEvent(
            connectionHandler = connectionHandler,
            resultDatabaseSettings = resultDatabaseSettings,
            outcomeId = row$outcomeId,
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
          eventDepObservation <- getSccsEventDepObservation(
            connectionHandler = connectionHandler,
            resultDatabaseSettings = resultDatabaseSettings,
            outcomeId = row$outcomeId,
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
            ageSpanning <- getSccsAgeSpanning(
              connectionHandler = connectionHandler,
              resultDatabaseSettings = resultDatabaseSettings,
              outcomeId = row$outcomeId,
              databaseId = row$databaseId,
              analysisId = row$analysisId
            )
            plotSpanning(ageSpanning, type = "age")
          } else {
            calendarTimeSpanning <- getSccsCalendarTimeSpanning(
              connectionHandler = connectionHandler,
              resultDatabaseSettings = resultDatabaseSettings,
              outcomeId = row$outcomeId,
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
          ageSpline <- getSccsSpline(
            connectionHandler = connectionHandler,
            resultDatabaseSettings = resultDatabaseSettings,
            outcomeId = row$outcomeId,
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
          seasonSpline <- getSccsSpline(
            connectionHandler = connectionHandler,
            resultDatabaseSettings = resultDatabaseSettings,
            outcomeId = row$outcomeId,
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
          calendarTimeSpline <- getSccsSpline(
            connectionHandler = connectionHandler,
            resultDatabaseSettings = resultDatabaseSettings,
            outcomeId = row$outcomeId,
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
          controlEstimates <- getSccsControlEstimates(
            connectionHandler = connectionHandler,
            resultDatabaseSettings = resultDatabaseSettings,
            covariateId = row$covariateId,
            databaseId = row$databaseId,
            analysisId = row$analysisId
          )
          plotControlEstimates(controlEstimates)
        }
      })
      
    }
  )
}






