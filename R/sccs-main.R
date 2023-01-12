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

  exposuresOutcomeSets <- getExposuresOutcomes(connectionHandler, resultDatabaseSettings$schema)
  exposuresOutcomeNames <- exposuresOutcomeSets %>%
    group_by(exposuresOutcomeSetId, outcomeName) %>%
    summarise(exposures = paste(exposureName, collapse = ", "), .groups = "drop") %>%
    mutate(name = sprintf("%s - %s", exposures, outcomeName))

  sccsAnalyses <- connectionHandler$tbl("sccs_analysis", databaseSchema = resultDatabaseSettings$schema) %>%
    dplyr::collect() %>%
    SqlRender::snakeCaseToCamelCaseNames()

  databases <- connectionHandler$tbl("database_meta_data", databaseSchema = resultDatabaseSettings$schema) %>%
    dplyr::collect() %>%
    SqlRender::snakeCaseToCamelCaseNames()

  shiny::moduleServer(id, function(input, output, session) {

    shiny::observe({
      # Dynamic loading of user selections
      shiny::updateSelectInput(session, "exposuresOutcome", choices = exposuresOutcomeNames$name)
      shiny::updateCheckboxGroupInput(session, "database", choices = databases$cdmSourceAbbreviation, selected = databases$cdmSourceAbbreviation)
      shiny::updateCheckboxGroupInput(session, "analysis", choices = sccsAnalyses$description, selected = sccsAnalyses$description)
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
                                resultsDatabaseSchema = resultDatabaseSettings$schema,
                                exposuresOutcomeSetId = exposuresOutcomeSetId,
                                databaseIds = databaseIds,
                                analysisIds = analysisIds)
      results <- results[order(results$analysisId),]

      idx <- (results$unblind == 0)
      if (any(idx)) {
        results$rr[idx] <- rep(NA, length(idx))
        results$ci95Ub[idx] <- rep(NA, length(idx))
        results$ci95Lb[idx] <- rep(NA, length(idx))
        results$logRr[idx] <- rep(NA, length(idx))
        results$seLogRr[idx] <- rep(NA, length(idx))
        results$p[idx] <- rep(NA, length(idx))
        results$calibratedRr[idx] <- rep(NA, length(idx))
        results$calibratedCi95Ub[idx] <- rep(NA, length(idx))
        results$calibratedCi95Lb[idx] <- rep(NA, length(idx))
        results$calibratedLogRr[idx] <- rep(NA, length(idx))
        results$calibratedSeLogRr[idx] <- rep(NA, length(idx))
        results$calibratedP[idx] <- rep(NA, length(idx))
      }
      return(results)
    })

    output$mainTable <- renderDataTable({
      table <- resultSubset()
      if (is.null(table) || nrow(table) == 0) {
        return(NULL)
      }
      table$description <- sccsAnalyses$description[match(table$analysisId, sccsAnalyses$analysisId)]
      table <- table %>%
        select("description",
               "databaseId",
               "rr",
               "ci95Lb",
               "ci95Ub",
               "p",
               "calibratedRr",
               "calibratedCi95Lb",
               "calibratedCi95Ub",
               "calibratedP")

      table$rr <- prettyHr(table$rr)
      table$ci95Lb <- prettyHr(table$ci95Lb)
      table$ci95Ub <- prettyHr(table$ci95Ub)
      table$p <- prettyHr(table$p)
      table$calibratedRr <- prettyHr(table$calibratedRr)
      table$calibratedCi95Lb <- prettyHr(table$calibratedCi95Lb)
      table$calibratedCi95Ub <- prettyHr(table$calibratedCi95Ub)
      table$calibratedP <- prettyHr(table$calibratedP)
      colnames(table) <- c("<span title=\"Analysis\">Analysis</span>",
                           "<span title=\"Data source\">Data source</span>",
                           "<span title=\"Incidence rate ratio (uncalibrated)\">IRR</span>",
                           "<span title=\"Lower bound of the 95 percent confidence interval (uncalibrated)\">LB</span>",
                           "<span title=\"Upper bound of the 95 percent confidence interval (uncalibrated)\">UB</span>",
                           "<span title=\"Two-sided p-value (uncalibrated)\">P</span>",
                           "<span title=\"Incidence rate ratio (calibrated)\">Cal.IRR</span>",
                           "<span title=\"Lower bound of the 95 percent confidence interval (calibrated)\">Cal.LB</span>",
                           "<span title=\"Upper bound of the 95 percent confidence interval (calibrated)\">Cal.UB</span>",
                           "<span title=\"Two-sided p-value (calibrated)\">Cal.P</span>")
      options = list(pageLength = 15,
                     searching = FALSE,
                     lengthChange = TRUE,
                     ordering = TRUE,
                     paging = TRUE)
      selection = list(mode = "single", target = "row")
      table <- datatable(table,
                         options = options,
                         selection = selection,
                         rownames = FALSE,
                         escape = FALSE,
                         class = "stripe nowrap compact")
      return(table)
    })

    selectedRow <- shiny::reactive({
      idx <- input$mainTable_rows_selected
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
    outputOptions(output, "rowIsSelected", suspendWhenHidden = FALSE)

    output$powerTable <- renderTable({
      row <- selectedRow()
      if (is.null(row)) {
        return(NULL)
      } else {
        table <- row %>%
          mutate(outcomeEvents = ifelse(unblind == 1, outcomeEvents, NA)) %>%
          select(
            "covariateName",
            "outcomeSubjects",
            "observedDays",
            "outcomeEvents",
            "covariateSubjects",
            "covariateDays",
            "covariateOutcomes",
            "mdrr"
          ) %>%
          mutate(observedDays = observedDays / 365.25,
                 covariateDays = covariateDays / 365.25)
        colnames(table) <- c("Variable",
                             "Cases",
                             "Years observed",
                             "Outcomes",
                             "Persons exposed",
                             "Years exposed",
                             "Outcomes while exposed",
                             "MDRR")
        return(table)
      }
    })

    output$attritionPlot <- renderPlot({
      row <- selectedRow()
      if (is.null(row)) {
        return(NULL)
      } else {
        attrition <- getAttrition(
          connectionHandler = connectionHandler,
          resultsDatabaseSchema = resultDatabaseSettings$schema,
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
        table <- getModel(
          connectionHandler = connectionHandler,
          resultsDatabaseSchema = resultDatabaseSettings$schema,
          exposuresOutcomeSetId = row$exposuresOutcomeSetId,
          databaseId = row$databaseId,
          analysisId = row$analysisId
        )

        table <- table %>%
          arrange(covariateId) %>%
          select(-"covariateId")

        colnames(table) <- c("Variable",
                             "IRR",
                             "LB",
                             "UB")
        return(table)
      }
    })

    output$timeTrendPlot <- shiny::renderPlot({
      row <- selectedRow()
      if (is.null(row)) {
        return(NULL)
      } else {
        timeTrend <- getTimeTrend(
          connectionHandler = connectionHandler,
          resultsDatabaseSchema = resultDatabaseSettings$schema,
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
        timeToEvent <- getTimeToEvent(
          connectionHandler = connectionHandler,
          resultsDatabaseSchema = resultDatabaseSettings$schema,
          exposuresOutcomeSetId = row$exposuresOutcomeSetId,
          eraId = row$eraId,
          covariateId = row$covariateId,
          databaseId = row$databaseId,
          analysisId = row$analysisId
        )
        plotTimeToEvent(timeToEvent)
      }
    })

    output$eventDepObservationPlot <- shiny::renderPlot({
      row <- selectedRow()
      if (is.null(row)) {
        return(NULL)
      } else {
        eventDepObservation <- getEventDepObservation(
          connectionHandler = connectionHandler,
          resultsDatabaseSchema = resultDatabaseSettings$schema,
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
          ageSpanning <- getAgeSpanning(
            connectionHandler = connectionHandler,
            resultsDatabaseSchema = resultDatabaseSettings$schema,
            exposuresOutcomeSetId = row$exposuresOutcomeSetId,
            databaseId = row$databaseId,
            analysisId = row$analysisId
          )
          plotSpanning(ageSpanning, type = "age")
        } else {
          calendarTimeSpanning <- getCalendarTimeSpanning(
            connectionHandler = connectionHandler,
            resultsDatabaseSchema = resultDatabaseSettings$schema,
            exposuresOutcomeSetId = row$exposuresOutcomeSetId,
            databaseId = row$databaseId,
            analysisId = row$analysisId
          )
          plotSpanning(calendarTimeSpanning, type = "calendar time")
        }
      }
    })

    output$ageSplinePlot <- renderPlot({
      row <- selectedRow()
      if (is.null(row)) {
        return(NULL)
      } else {
        ageSpline <- getSpline(
          connectionHandler = connectionHandler,
          resultsDatabaseSchema = resultDatabaseSettings$schema,
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
        seasonSpline <- getSpline(
          connectionHandler = connectionHandler,
          resultsDatabaseSchema = resultDatabaseSettings$schema,
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
        calendarTimeSpline <- getSpline(
          connectionHandler = connectionHandler,
          resultsDatabaseSchema = resultDatabaseSettings$schema,
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
        controlEstimates <- getControlEstimates(
          connectionHandler = connectionHandler,
          resultsDatabaseSchema = resultDatabaseSettings$schema,
          exposuresOutcomeSetId = row$exposuresOutcomeSetId,
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
        diagnosticsSummary <- getDiagnosticsSummary(
          connectionHandler = connectionHandler,
          resultsDatabaseSchema = resultDatabaseSettings$schema,
          exposuresOutcomeSetId = row$exposuresOutcomeSetId,
          covariateId = row$covariateId,
          databaseId = row$databaseId,
          analysisId = row$analysisId
        )
        table <- renderDiagnosticsSummary(diagnosticsSummary)
        return(table)
      }
    })

  })
}

#' SCCS shiny module UI code
#' @description
#' Load the ui for the sccs module
#' @param id        id for module
#' @export
sccsUi <- function(id = "sccs-module") {
  ns <- shiny::NS(id)
  tags <- shiny::tags
  shiny::fluidPage(
    style = "width:1500px;",
    shiny::titlePanel("SCCS Evidence Explorer"),
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
                                 ")),
    shiny::conditionalPanel(
      condition = "$('html').hasClass('shiny-busy')",
      ns = ns,
      tags$div("Processing...", id = "loadmessage")),
    shiny::fluidRow(
      shiny::column(
        3,
        shiny::selectInput(ns("exposuresOutcome"), "Exposures-outcome"),
        shiny::checkboxGroupInput(ns("database"), "Data source"),
        shiny::checkboxGroupInput(ns("analysis"), "Analysis")
      ),
      shiny::column(
        width = 9,
        DT::dataTableOutput(ns("mainTable")),
        shiny::conditionalPanel(
          "output.rowIsSelected == true",
          ns = ns,
          shiny::tabsetPanel(
            id = ns("detailsTabsetPanel"),
            shiny::tabPanel(
              "Power",
              shiny::div(strong("Table 1."), "For each variable of interest: the number of cases (people with at least one outcome), the number of years those people were observed, the number of outcomes, the number of subjects with at least one exposure, the number of patient-years exposed, the number of outcomes while exposed, and the minimum detectable relative risk (MDRR)."),
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
                    strong("Table 2."),
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
                  shiny::plotOutput("seasonSplinePlot"),
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
              shiny::div(strong("Figure 3."), "Number of subjects observed for 3 consecutive months, centered on the indicated month.")
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
              shiny::div(strong("Figure 6."), "Histograms for the number of months between the first occurrence of the outcome and the end of observation, stratified by whether the end of observation was censored (inferred as not being equal to the end of database time), or uncensored (inferred as having the subject still be observed at the end of database time)."
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
  )
}