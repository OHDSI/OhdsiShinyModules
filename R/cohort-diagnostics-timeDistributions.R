# Copyright 2022 Observational Health Data Sciences and Informatics
#
# This file is part of PatientLevelPrediction
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# Returns data from time_distribution table of Cohort Diagnostics results data model
getTimeDistributionResult <- function(dataSource,
                                      cohortIds,
                                      databaseIds,
                                      databaseTable) {
  data <- queryResultCovariateValue(
    dataSource = dataSource,
    cohortIds = cohortIds,
    databaseIds = databaseIds,
    analysisIds = c(8, 9, 10),
    temporalCovariateValue = FALSE,
    temporalCovariateValueDist = TRUE
  )
  if (!hasData(data)) {
    return(NULL)
  }
  temporalCovariateValueDist <- data$temporalCovariateValueDist
  if (!hasData(temporalCovariateValueDist)) {
    return(NULL)
  }
  data <- temporalCovariateValueDist %>%
    dplyr::inner_join(data$temporalCovariateRef,
      by = "covariateId"
    ) %>%
    dplyr::inner_join(data$temporalAnalysisRef,
      by = "analysisId"
    ) %>%
    dplyr::inner_join(databaseTable,
      by = "databaseId"
    ) %>%
    dplyr::rename(
      "timeMetric" = "covariateName",
      "averageValue" = "mean",
      "standardDeviation" = "sd"
    ) %>%
    dplyr::select(
      "cohortId",
      "databaseId",
      "databaseName",
      "timeMetric",
      "averageValue",
      "standardDeviation",
      "minValue",
      "p10Value",
      "p25Value",
      "medianValue",
      "p75Value",
      "p90Value",
      "maxValue"
    )
  return(data)
}


plotTimeDistribution <- function(data, shortNameRef = NULL) {
  errorMessage <- checkmate::makeAssertCollection()
  checkmate::assertTibble(
    x = data,
    any.missing = FALSE,
    min.rows = 1,
    min.cols = 5,
    null.ok = FALSE,
    add = errorMessage
  )
  checkmate::assertNames(
    x = colnames(data),
    must.include = c(
      "minValue",
      "p25Value",
      "medianValue",
      "p75Value",
      "maxValue"
    ),
    add = errorMessage
  )
  checkmate::reportAssertions(collection = errorMessage)

  plotData <-
    addShortName(data = data, shortNameRef = shortNameRef)

  plotData$tooltip <- c(
    paste0(
      plotData$shortName,
      "\nDatabase = ",
      plotData$databaseId,
      "\nMin = ",
      scales::comma(plotData$minValue, accuracy = 1),
      "\nP25 = ",
      scales::comma(plotData$p25Value, accuracy = 1),
      "\nMedian = ",
      scales::comma(plotData$medianValue, accuracy = 1),
      "\nP75 = ",
      scales::comma(plotData$p75Value, accuracy = 1),
      "\nMax = ",
      scales::comma(plotData$maxValue, accuracy = 1),
      "\nTime Measure = ",
      plotData$timeMetric,
      "\nAverage = ",
      scales::comma(x = plotData$averageValue, accuracy = 0.01)
    )
  )

  sortShortName <- plotData %>%
    dplyr::select("shortName") %>%
    dplyr::distinct() %>%
    dplyr::arrange(-as.integer(sub(
      pattern = "^C", "", x = .data$shortName
    )))

  plotData <- plotData %>%
    dplyr::arrange(
      shortName = factor(.data$shortName, levels = sortShortName$shortName),
      .data$shortName
    )

  plotData$shortName <- factor(plotData$shortName,
                               levels = sortShortName$shortName
  )

  plot <- ggplot2::ggplot(data = plotData) +
    ggplot2::aes(
      x = .data$shortName,
      ymin = .data$minValue,
      lower = .data$p25Value,
      middle = .data$medianValue,
      upper = .data$p75Value,
      ymax = .data$maxValue,
      average = .data$averageValue
    ) +
    ggplot2::geom_errorbar(size = 0.5) +
    ggiraph::geom_boxplot_interactive(
      ggplot2::aes(tooltip = .data$tooltip),
      stat = "identity",
      fill = grDevices::rgb(0, 0, 0.8, alpha = 0.25),
      size = 0.2
    ) +
    ggplot2::facet_grid(databaseName ~ timeMetric, scales = "free") +
    ggplot2::coord_flip() +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor.y = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      strip.background = ggplot2::element_blank(),
      strip.text.y = ggplot2::element_text(size = 5)
    )
  height <-
    1.5 + 0.4 * nrow(dplyr::distinct(.data = plotData, .data$databaseId, .data$shortName))
  plot <- ggiraph::girafe(
    ggobj = plot,
    options = list(
      ggiraph::opts_sizing(width = .7),
      ggiraph::opts_zoom(max = 5)
    ),
    width_svg = 12,
    height_svg = height
  )
}


#' timeDistributions view
#' @description
#' Use for customizing UI
#'
#' @param id    Namespace Id - use namespaced id ns("imeDistributions") inside diagnosticsExplorer module
#' @export
timeDistributionsView <- function(id) {
  ns <- shiny::NS(id)
  selectableCols <- c(
    "Average",
    "SD",
    "Min",
    "P10",
    "P25",
    "Median",
    "P75",
    "P90",
    "Max"
  )

  selectableTimeMeasures <- c(
    "observation time (days) prior to index",
    "observation time (days) after index",
    "time (days) between cohort start and end"
  )

  shiny::tagList(
    shinydashboard::box(
      collapsible = TRUE,
      collapsed = TRUE,
      title = "Time Distributions",
      width = "100%",
      shiny::htmlTemplate(system.file("cohort-diagnostics-www",  "timeDistribution.html", package = utils::packageName()))
    ),
    shinydashboard::box(
      status = "warning",
      width = "100%",
      shiny::tags$div(
        style = "max-height: 100px; overflow-y: auto",
        shiny::uiOutput(outputId = ns("selectedCohorts"))
      )
    ),
    shinydashboard::box(
      title = "Time Distributions",
      width = NULL,
      status = "primary",

      shiny::fluidRow(
        shiny::column(
          width = 2,
          shiny::radioButtons(
            inputId = ns("timeDistributionType"),
            label = "",
            choices = c("Table", "Plot"),
            selected = "Plot",
            inline = TRUE
          )
        ),
        shiny::column(
          width = 5,
          shinyWidgets::pickerInput(
            label = "View Time Measures",
            inputId = ns("selecatableTimeMeasures"),
            multiple = TRUE,
            selected = selectableTimeMeasures,
            choices = selectableTimeMeasures,
            options = shinyWidgets::pickerOptions(
              actionsBox = TRUE,
              liveSearch = TRUE,
              size = 10,
              dropupAuto = TRUE,
              liveSearchStyle = "contains",
              liveSearchPlaceholder = "Type here to search",
              virtualScroll = 50
            )
          )
        ),
        shiny::column(
          width = 5,
          shiny::conditionalPanel(
            condition = "input.timeDistributionType=='Table'",
            ns = ns,
            shinyWidgets::pickerInput(
              label = "View Columns",
              inputId = ns("selecatableCols"),
              multiple = TRUE,
              selected = selectableCols,
              choices = selectableCols,
              options = shinyWidgets::pickerOptions(
                actionsBox = TRUE,
                liveSearch = TRUE,
                size = 10,
                dropupAuto = TRUE,
                liveSearchStyle = "contains",
                liveSearchPlaceholder = "Type here to search",
                virtualScroll = 50
              )
            )
          )
        )
      ),
      shiny::conditionalPanel(
        condition = "input.timeDistributionType=='Table'",
        ns = ns,
        shinycssloaders::withSpinner(reactable::reactableOutput(outputId = ns("timeDistributionTable"))),
        csvDownloadButton(ns, "timeDistributionTable")
      ),
      shiny::conditionalPanel(
        condition = "input.timeDistributionType=='Plot'",
        ns = ns,
        shiny::tags$br(),
        shinycssloaders::withSpinner(ggiraph::ggiraphOutput(ns("timeDistributionPlot"), width = "100%", height = "100%"))
      )
    )
  )
}

timeDistributionsModule <- function(id,
                                    dataSource,
                                    selectedCohorts,
                                    selectedDatabaseIds,
                                    cohortIds,
                                    cohortTable = dataSource$cohortTable,
                                    databaseTable = dataSource$databaseTable) {
  ns <- shiny::NS(id)
  shiny::moduleServer(id, function(input, output, session) {
    output$selectedCohorts <- shiny::renderUI({ selectedCohorts() })

    # Time distribution -----
    ## timeDistributionData -----
    timeDistributionData <- shiny::reactive({
      shiny::validate(shiny::need(length(selectedDatabaseIds()) > 0, "No data sources chosen"))
      shiny::validate(shiny::need(length(cohortIds()) > 0, "No cohorts chosen"))

      data <- getTimeDistributionResult(
        dataSource = dataSource,
        cohortIds = cohortIds(),
        databaseIds = selectedDatabaseIds(),
        databaseTable = databaseTable
      )

      if (hasData(data)) {
        data <- data %>% dplyr::filter(.data$timeMetric %in% input$selecatableTimeMeasures)
      }

      return(data)
    })

    ## output: timeDistributionPlot -----
    output$timeDistributionPlot <- ggiraph::renderggiraph(expr = {
      data <- timeDistributionData()
      shiny::validate(shiny::need(hasData(data), "No data for this combination"))
      plot <- plotTimeDistribution(data = data, shortNameRef = cohortTable)
      return(plot)
    })

    ## output: timeDistributionTable -----
    output$timeDistributionTable <- reactable::renderReactable(expr = {
      data <- timeDistributionData()
      shiny::validate(shiny::need(hasData(data), "No data for this combination"))

      data <- data %>%
        dplyr::inner_join(cohortTable %>% dplyr::select("cohortName", "cohortId"), by = "cohortId") %>%
        dplyr::arrange(.data$databaseId, .data$cohortId) %>%
        dplyr::select(
          "cohortId",
          "Database" = "databaseName",
          "Cohort" = "cohortName",
          "TimeMeasure" = "timeMetric",
          "Average" = "averageValue",
          "SD" = "standardDeviation",
          "Min" = "minValue",
          "P10" = "p10Value",
          "P25" = "p25Value",
          "Median" = "medianValue",
          "P75" = "p75Value",
          "P90" = "p90Value",
          "Max" = "maxValue"
        ) %>%
        dplyr::select(dplyr::all_of(c("Database", "cohortId", "Cohort", "TimeMeasure", input$selecatableCols)))

      shiny::validate(shiny::need(hasData(data), "No data for this combination"))

      keyColumns <- c(
        "Database",
        "cohortId",
        "Cohort",
        "TimeMeasure"
      )
      dataColumns <- input$selecatableCols

      table <- getDisplayTableSimple(
        data = data,
        keyColumns = keyColumns,
        dataColumns = dataColumns
      )
      return(table)
    })
  })
}
