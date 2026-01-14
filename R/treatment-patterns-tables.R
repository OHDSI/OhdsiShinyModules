# @file treatment-patterns-main.R
#
# Copyright 2025 Observational Health Data Sciences and Informatics
#
# This file is part of OhdsiShinyModules
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


treatmentPatternsTabularViewer <- function(id) {
  ns <- shiny::NS(id)

  shiny::div(
    shiny::helpText("View tabular data for target cohorts and a database"),
    shinydashboard::box(
      collapsible = TRUE,
      title = "Options",
      width = "100%",
      shiny::uiOutput(ns("inputs"))
    ),
    shiny::conditionalPanel(
      condition = "output.showTables != 0",
      ns = ns,
      inputSelectionDfViewer(id = ns("inputSelected"), title = "Selected"),
      shiny::uiOutput(ns("tabularData"))
    )
  )
}

treatmentPatternsTabularServer <- function(
  id,
  connectionHandler,
  resultDatabaseSettings,
  reactiveTargetRow
) {
  shiny::moduleServer(id, function(input, output, session) {
    #--- states ---
    generateIcon <- shiny::reactiveVal(NULL)
    showTables <- shiny::reactiveVal(0)
    pathwayTable <- shiny::reactiveVal(NULL)
    summaryTable <- shiny::reactiveVal(NULL)

    #-- selection ui ---
    databaseNames <- shiny::reactive(unlist(strsplit(x = reactiveTargetRow()$databaseName, split = ", ")))

    output$inputs <- shiny::renderUI({
      shiny::div(
        shiny::fluidRow(
          shiny::column(
            width = 6,
            shinyWidgets::pickerInput(
              inputId = session$ns("databaseName"),
              label = "Database: ",
              choices = unique(databaseNames()),
              selected = databaseNames()[1],
              multiple = F,
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
            width = 2,
            shiny::textInput(inputId = session$ns("age"), label = "Age Filter", value = "all")
          ),
          shiny::column(
            width = 2,
            shiny::textInput(inputId = session$ns("indexYear"), label = "Year Filter", value = "all")
          ),
          shiny::column(
            width = 2,
            shiny::selectInput(inputId = session$ns("sex"), label = "Sex Filter", choices = c("all", "Male", "Female"), selected = "all")
          ),
          shiny::column(
            width = 3,
            shiny::actionButton(
              inputId = session$ns("generate"),
              icon = shiny::icon(generateIcon()),
              label = "Generate"
            )
          )
        )
      )
    })
    #---- Conditional Panel ----
    output$showTables <- shiny::reactive({
      showTables()
    })

    shiny::outputOptions(output, "showTables", suspendWhenHidden = FALSE)

    shiny::observeEvent(reactiveTargetRow(),
      {
        showTables(0)
        generateIcon(NULL)
        pathwayTable(NULL)
        summaryTable(NULL)
      },
      ignoreInit = TRUE
    )

    analysisGroup <- shiny::reactive({
      req(reactiveTargetRow())
      reactiveTargetRow() %>%
        dplyr::select("analysisId", "targetCohortName") %>%
        dplyr::distinct()
    })
    #---- generate click: fetch pathways ----
    inputSelectionDfServer(
      id = "inputSelected",
      dataFrameRow = selected,
      ncol = 2
    )

    selected <- shiny::reactiveVal(value = NULL)

    shiny::observeEvent(input$generate, {
      if (is.null(input$databaseName) | is.null(reactiveTargetRow())) {
        output$showDatabase <- shiny::reactive(0)

        shiny::showNotification("No databases selected")
      } else {
        showTables(1)
        generateIcon("redo")

        getPathway <- OhdsiReportGenerator::getTreatmentPathways(
          connectionHandler = connectionHandler,
          schema = resultDatabaseSettings$schema,
          tpTablePrefix = resultDatabaseSettings$tpTablePrefix,
          databaseTable = resultDatabaseSettings$databaseTable,
          targetIds = unique(reactiveTargetRow()$targetCohortId),
          analysisIds = unique(reactiveTargetRow()$analysisId),
          databaseNames = input$databaseName
        )
        pathwayTable(
          getPathway %>% dplyr::filter(
            age == input$age,
            indexYear == input$indexYear,
            sex == input$sex
          )
        )

        summaryTable(
          OhdsiReportGenerator::getEventDuration(
            connectionHandler = connectionHandler,
            schema = resultDatabaseSettings$schema,
            analysisIds = unique(reactiveTargetRow()$analysisId),
            tpTablePrefix = "tp_",
            databaseTable = "database_meta_data",
            databaseNames = input$databaseName,
            targetIds = unique(reactiveTargetRow()$targetCohortId)
          )
        )

        selected(
          data.frame(
            Analyses = paste(unique(reactiveTargetRow()$analysisId), collapse = ", "),
            Age = input$age,
            Targets = paste(unique(reactiveTargetRow()$targetCohortName), collapse = ", "),
            Year = input$indexYear,
            Databases = paste(input$databaseName, collapse = ", "),
            Sex = input$sex
          )
        )
      }
    })

    pathwayGroups <- shiny::reactive({
      req(pathwayTable())
      df <- pathwayTable() %>% dplyr::filter(
        age == input$age,
        indexYear == input$indexYear,
        sex == input$sex
      )
      split(df, interaction(df$analysisId, df$targetCohortName, drop = TRUE), drop = TRUE)
    })

    #---- render table ui ----
    output$tabularData <- shiny::renderUI({
      req(showTables() != 0)
      req(!is.null(analysisGroup()) && nrow(analysisGroup()) > 0)

      UIList <- lapply(seq_len(nrow(analysisGroup())), function(idx) {
        analysis <- analysisGroup()[idx, ]$analysisId
        target <- analysisGroup()[idx, ]$targetCohortName

        shinydashboard::box(
          collapsible = TRUE,
          status = "primary",
          solidHeader = TRUE,
          title = paste("Analysis:", analysis, "Target:", target),
          width = 12,
          if (is.null(pathwayGroups()[[paste0(analysis, ".", target)]])) {
            shiny::helpText("No analyses results to show")
          } else {
            shiny::tagList(
              sunburstPlotViewer(session$ns(paste0("sunburst_", idx))),
              OhdsiShinyModules:::resultTableViewer(id = session$ns(paste0("pathways_", idx)), boxTitle = "All Pathways"),
              OhdsiShinyModules:::resultTableViewer(id = session$ns(paste0("event_count_", idx)), boxTitle = "Event Cohort Counts"),
              OhdsiShinyModules:::resultTableViewer(id = session$ns(paste0("event_rank_", idx)), boxTitle = "Event Cohort Counts by Rank"),
              OhdsiShinyModules:::resultTableViewer(id = session$ns(paste0("duration_summary_", idx)), boxTitle = "Event Duration Summary")
            )
          }
        )
      })

      shiny::tagList(UIList)
    })

    #--- Render Server ---
    shiny::observeEvent(pathwayTable(), {
      req(showTables() != 0)
      req(!is.null(pathwayGroups()))
      req(!is.null(analysisGroup()) && nrow(analysisGroup()) > 0)

      lapply(seq_len(nrow(analysisGroup())), function(idx) {
        local({
          analysis <- analysisGroup()[idx, ]$analysisId
          target <- analysisGroup()[idx, ]$targetCohortName

          pathways <- pathwayGroups()[[paste0(analysis, ".", target)]] %>%
            dplyr::select(-sex, -age, -indexYear, -analysisId, -targetCohortId)

          if (!is.null(pathways)) {
            sunburstPlotServer(
              id = paste0("sunburst_", idx),
              pathwayTable = pathways,
              sunburstList = input$databaseName,
              filterColumn = "databaseName",
              filenamePrefix = paste0(analysis, "_", target),
              width = "100%",
              height = "700px",
              legend = list(w=400, h=30)
            )

            total <- sum(pathways$freq, na.rm = TRUE)

            # 1. Pathways table
            pathwaysDf <- createPathwaysTable(pathways = pathways, total = total)
            OhdsiShinyModules:::resultTableServer(
              id = paste0("pathways_", idx),
              df = pathwaysDf,
              details = data.frame(
                analysisId = analysis,
                target = target,
                Database = input$databaseName,
                description = "Treatment pathways observed in the selected target cohort and related event cohorts (frequency and step sequence)"
              ),
              downloadedFileName = paste0("pathways_", analysis, "_", target, "_", input$databaseName),
              colDefsInput = treatmentPatternsColDef(tableId = "pathways"),
              elementId = session$ns(paste0("pathways_", analysis, "_", target, "_", input$databaseName))
            )
            # 2. Event Table
            eventDf <- createEventCountTable(pathways = pathways, total = total)
            OhdsiShinyModules:::resultTableServer(
              id = paste0("event_count_", idx),
              df = eventDf,
              details = data.frame(
                analysisId = analysis,
                target = target,
                Database = input$databaseName,
                description = "Number of Unique events across all steps of the pathway"
              ),
              downloadedFileName = paste0("event_count_", analysis, "_", target, "_", input$databaseName),
              colDefsInput = treatmentPatternsColDef(tableId = "eventCount"),
              elementId = session$ns(paste0("event_count_", analysis, "_", target, "_", input$databaseName))
            )
            # 3. Event Rank Table
            eventRankDf <- createEventRankTable(pathways = pathways, total = total)
            OhdsiShinyModules:::resultTableServer(
              id = paste0("event_rank_", idx),
              df = eventRankDf,
              details = data.frame(
                analysisId = analysis,
                target = target,
                Database = input$databaseName,
                description = "Number of Unique events with in each steps of the pathway"
              ),
              downloadedFileName = paste0("event_rank_", analysis, "_", target, "_", input$databaseName),
              colDefsInput = treatmentPatternsColDef(tableId = "eventRank"),
              elementId = session$ns(paste0("event_rank_", analysis, "_", target, "_", input$databaseName))
            )
            # 4. Duration Summary table
            durationDf <- summaryTable() %>%
              dplyr::select(-analysisId, -targetCohortId)

            OhdsiShinyModules:::resultTableServer(
              id = paste0("duration_summary_", idx),
              df = durationDf,
              details = data.frame(
                analysisId = analysis,
                target = target,
                Database = input$databaseName,
                description = "Summary Statitics for each event duration"
              ),
              downloadedFileName = paste0("duration_summary_", analysis, "_", target, "_", input$databaseName),
              colDefsInput = treatmentPatternsColDef(tableId = "duration"),
              elementId = session$ns(paste0("duration_summary_", analysis, "_", target, "_", input$databaseName))
            )
          }
        })
      })
    })
  })
}


createPathwaysTable <- function(pathways, total) {
  stepCount <- pathways %>%
    dplyr::summarise(
      stepCount = max(stringr::str_count(pathway, "-") + 1, na.rm = TRUE)
    ) %>%
    dplyr::pull(stepCount)

  into <- paste0("step ", seq_len(stepCount))

  table <- pathways %>%
    tidyr::separate(
      pathway,
      into = into,
      sep = "-",
      fill = "right",
      extra = "merge"
    ) %>%
    dplyr::mutate(across(dplyr::starts_with("step "), stringr::str_trim)) %>%
    dplyr::mutate(pathwayPercent = freq / total)

  return(table)
}

createEventCountTable <- function(pathways, total) {
  table <- pathways %>%
    dplyr::mutate(pathwayNumber = dplyr::row_number()) %>%
    dplyr::mutate(event = stringr::str_split(pathway, "\\s*-\\s*")) %>%
    tidyr::unnest(c(event)) %>%
    dplyr::mutate(event = stringr::str_trim(event)) %>%
    dplyr::distinct(pathwayNumber, event, .keep_all = TRUE) %>%
    dplyr::group_by(event) %>%
    dplyr::summarise(freq = sum(freq, na.rm = TRUE), .groups = "drop") %>%
    dplyr::mutate(pathwayPercent = freq / total)

  return(table)
}

createEventRankTable <- function(pathways, total) {
  table <- pathways %>%
    dplyr::mutate(event = stringr::str_split(pathway, "\\s*-\\s*")) %>%
    tidyr::unnest_longer(
      col = event,
      indices_to = "rank",
      values_to = "event",
    ) %>%
    dplyr::mutate(event = stringr::str_trim(event)) %>%
    dplyr::group_by(rank, event) %>%
    dplyr::summarise(freq = sum(freq), .groups = "drop") %>%
    dplyr::mutate(pathwayPercent = freq / total)

  return(table)
}

treatmentPatternsColDef <- function(tableId) {
  colDef <- list()

  if (tableId == "pathways") {
    colDef <- list(
      databaseId = reactable::colDef(
        name = "DatabaseId",
        header = withTooltip("Database Id", "Unique representation for Database"),
        show = FALSE
      ),
      databaseName = reactable::colDef(
        name = "DatabaseName",
        header = withTooltip("Database Name", "Name of Database"),
        show = FALSE
      ),
      targetCohortName = reactable::colDef(
        name = "Target",
        header = withTooltip("Target Name", "Name of the Target Cohort"),
        filterable = TRUE,
        minWidth = 300
      ),
      freq = reactable::colDef(
        name = "Count",
        header = OhdsiShinyModules:::withTooltip("Count", "Frequency of the pathway"),
        filterable = TRUE
      ),
      pathwayPercent = reactable::colDef(
        name = "pathwayPercent",
        header = OhdsiShinyModules:::withTooltip("% with Pathway", "Percent of records with this pathway (freq / total freq)"),
        filterable = TRUE,
        format = reactable::colFormat(digits = 2, percent = TRUE)
      )
    )
  } else if (tableId == "eventCount") {
    colDef <- list(
      databaseId = reactable::colDef(
        name = "DatabaseId",
        header = withTooltip("Database Id", "Unique representation for Database"),
        show = FALSE
      ),
      databaseName = reactable::colDef(
        name = "DatabaseName",
        header = withTooltip("Database Name", "Name of Database"),
        show = FALSE
      ),
      event = reactable::colDef(
        name = "Event",
        header = withTooltip("Event Name", "Name of Event")
      ),
      freq = reactable::colDef(
        name = "count",
        header = OhdsiShinyModules:::withTooltip("Count", "Number of pathways with event"),
        filterable = TRUE
      ),
      pathwayPercent = reactable::colDef(
        name = "pathwayPercent",
        header = OhdsiShinyModules:::withTooltip("% with Pathway", "Percent of pathways with this event"),
        filterable = TRUE,
        format = reactable::colFormat(digits = 2, percent = TRUE)
      )
    )
  } else if (tableId == "eventRank") {
    colDef <- list(
      databaseId = reactable::colDef(
        name = "DatabaseId",
        header = withTooltip("Database Id", "Unique representation for Database"),
        show = FALSE
      ),
      databaseName = reactable::colDef(
        name = "DatabaseName",
        header = withTooltip("Database Name", "Name of Database"),
        show = FALSE
      ),
      rank = reactable::colDef(
        name = "Rank",
        header = OhdsiShinyModules:::withTooltip("Rank", "What step an event occured at"),
        filterable = TRUE
      ),
      event = reactable::colDef(
        name = "Event",
        header = OhdsiShinyModules:::withTooltip("Event", "Name of Event"),
        filterable = TRUE
      ),
      freq = reactable::colDef(
        name = "Count",
        header = OhdsiShinyModules:::withTooltip("Count", "Number of times an event occured at a step"),
        filterable = TRUE
      ),
      pathwayPercent = reactable::colDef(
        name = "pathwayPercent",
        header = OhdsiShinyModules:::withTooltip("% with Pathway", "Percent of pathways with an event at a step"),
        filterable = TRUE,
        format = reactable::colFormat(digits = 2, percent = TRUE)
      )
    )
  } else if (tableId == "duration") {
    colDef <- list(
      databaseId = reactable::colDef(
        name = "DatabaseId",
        header = withTooltip("Database Id", "Unique representation for Database"),
        show = FALSE
      ),
      databaseName = reactable::colDef(
        name = "DatabaseName",
        header = withTooltip("Database Name", "Name of Database"),
        show = FALSE
      ),
      targetCohortName = reactable::colDef(
        name = "Target",
        header = withTooltip("Target Name", "Name of the Target Cohort"),
        filterable = TRUE,
        minWidth = 300
      ),
      eventName = reactable::colDef(
        name = "Event",
        header = withTooltip("Event", "Name of Event"),
        filterable = TRUE,
        minWidth = 300
      ),
      rank = reactable::colDef(
        name = "Rank",
        header = OhdsiShinyModules:::withTooltip("Rank", "What step an event occured at"),
        filterable = TRUE
      ),
      eventCount = reactable::colDef(
        name = "Count",
        header = OhdsiShinyModules:::withTooltip("Count", "Number of times an event occured at a step"),
        filterable = TRUE
      ),
      durationAverage = reactable::colDef(
        name = "Average",
        header = OhdsiShinyModules:::withTooltip("Average", "Average number of days for event"),
        filterable = TRUE
      ),
      durationMax = reactable::colDef(
        name = "Max Duration",
        header = OhdsiShinyModules:::withTooltip("Max Duration", "Maximum number of days for event"),
        filterable = TRUE
      ),
      durationMin = reactable::colDef(
        name = "Min Duration",
        header = OhdsiShinyModules:::withTooltip("Min Duration", "Minimum number of days for event"),
        filterable = TRUE
      ),
      durationMedian = reactable::colDef(
        name = "Median",
        header = OhdsiShinyModules:::withTooltip("Medain", "Median number of days for event"),
        filterable = TRUE
      ),
      p25Value = reactable::colDef(
        name = "p25Value",
        header = OhdsiShinyModules:::withTooltip("p25Value", "25th percentile of event duration, in days"),
        filterable = TRUE
      ),
      p75Value = reactable::colDef(
        name = "p75Value",
        header = OhdsiShinyModules:::withTooltip("p75Value", "75th percentile of event duration, in days"),
        filterable = TRUE
      ),
      standardDeviation = reactable::colDef(
        name = "StDev",
        header = withTooltip("StDev", "The standard deviation value of the event durations, in days"),
        cell = function(v) {
          v <- suppressWarnings(as.numeric(v))
          if (is.na(v)) "" else if (v >= 0) round(v, 3) else paste0("< ", abs(round(v, 3)))
        }
      )
    )
  }
  return(colDef)
}
