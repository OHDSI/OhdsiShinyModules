#' The module server for exploring SCCS
#'
#' @details
#' The user specifies the id for the module
#'
#' @param id  the unique reference id for the module
#' @param connectionHandler a connection to the database with the results
#' @param resultDatabaseSettings a list containing the prediction result schema and connection details
#'
#' @return
#' The server for the PatientLevelPrediction module
#'
#' @export
sccsServer <- function(
  id,
  connectionHandler,
  resultDatabaseSettings = list(port = 1)
) {
  ns <- shiny::NS(id)

  withTooltip <- function(value, tooltip, ...) {
    shiny::div(style = "text-decoration: underline; text-decoration-style: dotted; cursor: help",
               tippy::tippy(value, tooltip, ...))
  }
  
  exposuresOutcomeSets <- getSccsExposuresOutcomes(connectionHandler, resultDatabaseSettings)

  exposuresOutcomeNames <- exposuresOutcomeSets %>%
    dplyr::group_by(.data$exposuresOutcomeSetId, .data$outcomeName) %>%
    dplyr::summarise(exposures = paste(.data$exposureName, collapse = ", "), .groups = "drop") %>%
    dplyr::mutate(name = sprintf("%s - %s", .data$exposures, .data$outcomeName))

  sccsAnalyses <- connectionHandler$tbl(paste0(resultDatabaseSettings$tablePrefix,"analysis"), databaseSchema = resultDatabaseSettings$schema) %>%
    dplyr::collect() %>%
    SqlRender::snakeCaseToCamelCaseNames()

  databases <- connectionHandler$tbl(resultDatabaseSettings$databaseTable, databaseSchema = resultDatabaseSettings$schema) %>%
    dplyr::collect() %>%
    SqlRender::snakeCaseToCamelCaseNames()

  shiny::moduleServer(id, function(input, output, session) {

    shiny::observe({
      # Dynamic loading of user selections

      databaseIds <- databases$databaseId
      names(databaseIds) <- databases$cdmSourceAbbreviation
      shinyWidgets::updatePickerInput(
        session = session, 
        inputId = "exposuresOutcome", 
        choices = exposuresOutcomeNames$name, 
        selected = exposuresOutcomeNames$name[1],
        options = shinyWidgets::pickerOptions(
          actionsBox = TRUE,
          liveSearch = TRUE,
          size = 10,
          liveSearchStyle = "contains",
          liveSearchPlaceholder = "Type here to search",
          virtualScroll = 50
        )
        )
      
      shinyWidgets::updatePickerInput(
        session = session, 
        inputId = "database", 
        choices = databaseIds, 
        selected = databaseIds,
        options = shinyWidgets::pickerOptions(
          actionsBox = TRUE,
          liveSearch = TRUE,
          size = 10,
          liveSearchStyle = "contains",
          liveSearchPlaceholder = "Type here to search",
          virtualScroll = 50
        )
      )
      
      shinyWidgets::updatePickerInput(
        session = session, 
        inputId = "analysis", 
        choices = sccsAnalyses$description, 
        selected = sccsAnalyses$description,
        options = shinyWidgets::pickerOptions(
          actionsBox = TRUE,
          liveSearch = TRUE,
          size = 10,
          liveSearchStyle = "contains",
          liveSearchPlaceholder = "Type here to search",
          virtualScroll = 50
        )
      )
      
    })

    resultSubset <- shiny::reactive({
      exposuresOutcomeSetId <- exposuresOutcomeNames$exposuresOutcomeSetId[exposuresOutcomeNames$name == input$exposuresOutcome]
      analysisIds <- sccsAnalyses$analysisId[sccsAnalyses$description %in% input$analysis]
      databaseIds <- input$database

      if (length(analysisIds) == 0) {
        analysisIds <- -1
      }
      if (length(databaseIds) == 0) {
        databaseIds <- "none"
      }
      results <- getSccsResults(connectionHandler = connectionHandler,
                                resultDatabaseSettings = resultDatabaseSettings,
                                exposuresOutcomeSetId = exposuresOutcomeSetId,
                                databaseIds = databaseIds,
                                analysisIds = analysisIds)
                                
      results$description <- sccsAnalyses$description[match(results$analysisId, sccsAnalyses$analysisId)]
      
      results <- results[order(results$analysisId),]

      idx <- (results$unblind == 0)
      if (any(idx)) {
        results$rr[idx] <- NA
        results$ci95Ub[idx] <- NA
        results$ci95Lb[idx] <- NA
        results$logRr[idx] <- NA
        results$seLogRr[idx] <- NA
        results$p[idx] <- NA
        results$calibratedRr[idx] <- NA
        results$calibratedCi95Ub[idx] <- NA
        results$calibratedCi95Lb[idx] <- NA
        results$calibratedLogRr[idx] <- NA
        results$calibratedSeLogRr[idx] <- NA
        results$calibratedP[idx] <- NA
      }
      
      results$rr <- prettyHr(results$rr)
      results$ci95Lb <- prettyHr(results$ci95Lb)
      results$ci95Ub <- prettyHr(results$ci95Ub)
      results$p <- prettyHr(results$p)
      results$calibratedRr <- prettyHr(results$calibratedRr)
      results$calibratedCi95Lb <- prettyHr(results$calibratedCi95Lb)
      results$calibratedCi95Ub <- prettyHr(results$calibratedCi95Ub)
      results$calibratedP <- prettyHr(results$calibratedP)
      
      return(results)
    })

    output$mainTable <- reactable::renderReactable({
       reactable::reactable(
        data = resultSubset() %>%
          dplyr::select("description",
                        "databaseName",
                        "rr",
                        "ci95Lb",
                        "ci95Ub",
                        "p",
                        "calibratedRr",
                        "calibratedCi95Lb",
                        "calibratedCi95Ub",
                        "calibratedP"),
        rownames = FALSE, 
        defaultPageSize = 15,
        showPageSizeOptions = T, 
        onClick = 'select', 
        selection = 'single',
        striped = T,
        
        columns = list(
          description = reactable::colDef( 
            filterable = TRUE,
            header = withTooltip(
              "Analysis", 
              "Analysis"
            )),
          databaseName = reactable::colDef( 
            filterable = TRUE,
            header = withTooltip(
              "Data source", 
              "Data source"
            )),
          rr = reactable::colDef( 
            header = withTooltip(
              "IRR", 
              "Incidence rate ratio (uncalibrated)"
            )),
          ci95Lb = reactable::colDef( 
            header = withTooltip(
              "LB", 
              "Lower bound of the 95 percent confidence interval (uncalibrated)"
            )),
          ci95Ub = reactable::colDef( 
            header = withTooltip(
              "UB", 
              "Upper bound of the 95 percent confidence interval (uncalibrated)"
            )),
          p = reactable::colDef( 
            header = withTooltip(
              "P", 
              "Two-sided p-value (uncalibrated)"
            )),
          calibratedRr = reactable::colDef( 
            header = withTooltip(
              "Cal.IRR", 
              "Incidence rate ratio (calibrated)"
            )),
          calibratedCi95Lb = reactable::colDef( 
            header = withTooltip(
              "Cal.LB", 
              "Lower bound of the 95 percent confidence interval (calibrated)"
            )),
          calibratedCi95Ub = reactable::colDef( 
            header = withTooltip(
              "Cal.UB", 
              "Upper bound of the 95 percent confidence interval (calibrated)"
            )),
          calibratedP = reactable::colDef( 
            header = withTooltip(
              "Cal.P", 
              "Two-sided p-value (calibrated)"
            ))
        )
       )
  
    })

    selectedRow <- shiny::reactive({
      if (getOption("shiny-test-env-enabled", default = FALSE)) {
        idx <- input$mainTableRowInput
      } else {
        idx <- reactable::getReactableState(
          outputId = 'mainTable', 
          name = 'selected'
        ) 
      }

      if (is.null(idx)) {
        return(NULL)
      } else {
        subset <- resultSubset()
        if (nrow(subset) == 0) {
          return(NULL)
        }
        row <- subset[idx,]
        return(row)
      }
    })

    output$rowIsSelected <- shiny::reactive({
      return(!is.null(selectedRow()))
    })
    
    shiny::outputOptions(output, "rowIsSelected", suspendWhenHidden = FALSE)

    output$powerTable <- shiny::renderTable({
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
        colnames(resTargetTable) <- c("Variable",
                                      "Cases",
                                      "Years observed",
                                      "Outcomes",
                                      "Persons exposed",
                                      "Years exposed",
                                      "Outcomes while exposed",
                                      "MDRR")
        return(resTargetTable)
      }
    })

    output$attritionPlot <- shiny::renderPlot({

      row <- selectedRow()
      if (is.null(row)) {
        return(NULL)
      } else {
        attrition <- getSccsAttrition(
          connectionHandler = connectionHandler,
          resultDatabaseSettings = resultDatabaseSettings,
          exposuresOutcomeSetId = row$exposuresOutcomeSetId,
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
        timeTrend <- getSccsTimeTrend(
          connectionHandler = connectionHandler,
          resultDatabaseSettings = resultDatabaseSettings,
          exposuresOutcomeSetId = row$exposuresOutcomeSetId,
          databaseId = row$databaseId,
          analysisId = row$analysisId
        )
        plotTimeTrend(timeTrend)
      }
    })

    output$timeToEventPlot <- shiny::renderPlot({
      row <- selectedRow()
      if (is.null(row)) {
        return(NULL)
      } else {
        timeToEvent <- getSccsTimeToEvent(
          connectionHandler = connectionHandler,
          resultsDatabaseSettings = resultDatabaseSettings,
          exposuresOutcomeSetId = row$exposuresOutcomeSetId,
          eraId = row$eraId,
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
          ageSpanning <- getSccsAgeSpanning(
            connectionHandler = connectionHandler,
            resultDatabaseSettings = resultDatabaseSettings,
            exposuresOutcomeSetId = row$exposuresOutcomeSetId,
            databaseId = row$databaseId,
            analysisId = row$analysisId
          )
          plotSpanning(ageSpanning, type = "age")
        } else {
          calendarTimeSpanning <- getSccsCalendarTimeSpanning(
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
        ageSpline <- getSccsSpline(
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
        seasonSpline <- getSccsSpline(
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
        calendarTimeSpline <- getSccsSpline(
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
        controlEstimates <- getSccsControlEstimates(
          connectionHandler = connectionHandler,
          resultDatabaseSettings,
          covariateId = row$covariateId,
          databaseId = row$databaseId,
          analysisId = row$analysisId
        )
        plotControlEstimates(controlEstimates)
      }
    })

    output$diagnosticsSummary <- shiny::renderTable({
      row <- selectedRow()
      if (is.null(row)) {
        return(NULL)
      } else {
        diagnosticsSummary <- getSccsDiagnosticsSummary(
          connectionHandler = connectionHandler,
          resultDatabaseSettings = resultDatabaseSettings,
          exposuresOutcomeSetId = row$exposuresOutcomeSetId,
          covariateId = row$covariateId,
          databaseId = row$databaseId,
          analysisId = row$analysisId
        )
        resTargetTable <- renderDiagnosticsSummary(diagnosticsSummary)
        return(resTargetTable)
      }
    })

  })
}

#' SCCS shiny module UI code
#' @description
#' Load the ui for the sccs module
#' @param id        id for module
#' @export
sccsView <- function(id = "sccs-module") {
  ns <- shiny::NS(id)
  tags <- shiny::tags
  
  shinydashboard::box(
    status = 'info', 
    width = 12,
    title = shiny::span( shiny::icon("people-arrows"), 'Self Controlled Case Series'),
    solidHeader = TRUE,
    
    shinydashboard::box(
      collapsible = TRUE,
      collapsed = TRUE,
      title = "Self Controlled Case Series Evidence",
      width = "100%"#,
      #shiny::htmlTemplate(system.file("cohort-diagnostics-www", "cohortCounts.html", package = utils::packageName()))
    ),
    
    tags$head(
      tags$style(
        type = "text/css", "
                                 #loadmessage {
                                 position: fixed;
                                 top: 0px;
                                 left: 0px;
                                 width: 100%;
                                 padding: 5px 0px 5px 0px;
                                 text-align: center;
                                 font-weight: bold;
                                 font-size: 100%;
                                 color: #000000;
                                 background-color: #ADD8E6;
                                 z-index: 105;
                                 }
                                 ")
    ),
    shiny::conditionalPanel(
      condition = "$('html').hasClass('shiny-busy')",
      ns = ns,
      tags$div("Processing...", id = "loadmessage")
    ),
    
    shinydashboard::box(
      collapsible = TRUE,
      title = "Options",
      width = "100%",
      
      shiny::fluidPage(
        shiny::fluidRow(
          shiny::column(
            width = 4,
            shinyWidgets::pickerInput(
              inputId = ns("exposuresOutcome"), 
              label = "Exposures-outcome", 
              choices = c()
            )
          ),
          shiny::column(
            width = 4,
            shinyWidgets::pickerInput(
              inputId = ns("database"), 
              label = "Data source",
              choices = c(),
              choicesOpt = list(style = rep_len("color: black;", 999)),
              multiple = T 
            )
          ),
          shiny::column(
            width = 4,
            shinyWidgets::pickerInput(
              inputId = ns("analysis"), 
              label = "Analysis",
              choices = c(),
              choicesOpt = list(style = rep_len("color: black;", 999)),
              multiple = T
              )
          )
        )
      )
    ),
    
    
    shinydashboard::box(
      width = '100%',
      reactable::reactableOutput(ns("mainTable")),
      
      shiny::conditionalPanel(
        "output.rowIsSelected == true",
        ns = ns,
        shiny::tabsetPanel(
          id = ns("detailsTabsetPanel"),
          shiny::tabPanel(
            "Power",
            shiny::div(shiny::strong("Table 1."), "For each variable of interest: the number of cases (people with at least one outcome), the number of years those people were observed, the number of outcomes, the number of subjects with at least one exposure, the number of patient-years exposed, the number of outcomes while exposed, and the minimum detectable relative risk (MDRR)."),
            shiny::tableOutput(ns("powerTable"))
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
              "Per calendar month the number of people observed, the unadjusted rate of the outcome, and the rate of the outcome after adjusting for age, season, and calendar time, if specified in the model. Red indicates months where the adjusted rate was significantly different from the mean adjusted rate."
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
          ),
          shiny::tabPanel(
            "Diagnostics summary",
            shiny::tableOutput(ns("diagnosticsSummary"))
            
          )
        )
      )
    )
  )
}

#' The location of the description module helper file
#'
#' @details
#' Returns the location of the description helper file
#'
#' @return
#' string location of the description helper file
#'
#' @export
sccsHelperFile <- function() {
  fileLoc <- system.file('sccs-www', "sccs.html", package = utils::packageName())
  return(fileLoc)
}
