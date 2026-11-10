# Copyright 2025 Observational Health Data Sciences and Informatics
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

#' Index event breakdown view
#' @description
#' Use for customizing UI
#'
#' @param id    Namespace Id - use namespaced id ns("indexEvents") inside diagnosticsExplorer module
#' @family CohortDiagnostics
#' @export
indexEventBreakdownView <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shinydashboard::box(
      collapsible = TRUE,
      collapsed = TRUE,
      title = "Index Events",
      width = "100%",
      shiny::htmlTemplate(system.file("cohort-diagnostics-www",  "indexEventBreakdown.html", package = utils::packageName()))
    ),
    shinydashboard::box(
      status = "warning",
      width = "100%",
      shiny::tags$div(
        style = "max-height: 100px; overflow-y: auto",
        shiny::uiOutput(outputId = ns("selectedCohort"))
      )
    ),
    shinydashboard::box(
      width = NULL,
      title = NULL,
      shiny::withTags(
        table(
          width = "100%",
          shiny::tags$tr(
            shiny::tags$td(
              shiny::radioButtons(
                inputId = ns("indexEventBreakdownTableRadioButton"),
                label = "Concept type",
                choices = c("All", "Standard concepts", "Non Standard Concepts"),
                selected = "All",
                inline = TRUE
              )
            ),
            shiny::tags$td(shiny::HTML("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;")),
            shiny::tags$td(
              shiny::radioButtons(
                inputId = ns("indexEventBreakdownTableFilter"),
                label = "Display",
                choices = c("Both", "Records", "Persons"),
                selected = "Persons",
                inline = TRUE
              )
            ),
            shiny::tags$td(
              shiny::checkboxInput(
                inputId = ns("showAsPercent"),
                label = "Show persons as percentage",
                value = TRUE
              )
            )
          ),
          # only rendered by indexEventBreakdownServer when the loaded results
          # include cohort_start_year (CohortDiagnostics 4.0.0 and later)
          shiny::tags$tr(
            shiny::tags$td(
              shiny::uiOutput(outputId = ns("yearControls"))
            )
          )
        )
      ),
      shinycssloaders::withSpinner(reactable::reactableOutput(outputId = ns("breakdownTable"))),
      reactableCsvDownloadButton(ns, "breakdownTable"),
      shiny::uiOutput(outputId = ns("yearPlotArea"))
    )
  )
}


# Is the column present in the data frame? used to support results exported by
# versions of CohortDiagnostics that predate an optional results column
hasIndexEventBreakdownColumn <- function(data, columnName) {
  if (!hasData(data)) {
    return(FALSE)
  }
  return(columnName %in% colnames(data))
}

# Does the index_event_breakdown table on the server have a column?
# Errors (e.g. the table does not exist yet) are treated as "column not present"
# so that the module degrades to its previous behaviour instead of failing.
hasIndexEventBreakdownColumnInResults <- function(dataSource,
                                                 columnName) {
  tablesOnServer <- dataSource$resultsTablesOnServer
  if (!hasData(tablesOnServer)) {
    tablesOnServer <- .availableTables(
      dataSource$connectionHandler,
      dataSource$schema
    )
  }
  tableName <- dataSource$prefixTable("index_event_breakdown")
  if (!tolower(tableName) %in% tolower(tablesOnServer)) {
    return(FALSE)
  }

  return(tryCatch(
    {
      # LIMIT 1 (SqlRender translates to TOP for sql_server) - we only need the
      # field names, not the rows
      dataSource$connectionHandler$queryDb(
        sql = "SELECT * FROM @schema.@table_name LIMIT 1;",
        schema = dataSource$schema,
        table_name = tableName,
        snakeCaseToCamelCase = TRUE
      ) %>%
        hasIndexEventBreakdownColumn(columnName = columnName)
    },
    error = function(e) {
      FALSE
    }
  ) )
}

getIndexEventBreakdown <- function(dataSource,
                                   cohortIds,
                                   cohortCount,
                                   databaseIds,
                                   cohortStartYears = NULL) {
  errorMessage <- checkmate::makeAssertCollection()
  errorMessage <-
    checkErrorCohortIdsDatabaseIds(
      cohortIds = cohortIds,
      databaseIds = databaseIds,
      errorMessage = errorMessage
    )
  checkmate::reportAssertions(collection = errorMessage)

  sql <- "SELECT index_event_breakdown.*,
              concept.concept_name,
              concept.domain_id,
              concept.vocabulary_id,
              concept.standard_concept,
              concept.concept_code
            FROM  @schema.@table_name index_event_breakdown
            INNER JOIN  @vocabulary_database_schema.@concept_table concept
              ON index_event_breakdown.concept_id = concept.concept_id
            WHERE database_id in (@database_id)
              AND cohort_id in (@cohort_ids);"
  data <-
    dataSource$connectionHandler$queryDb(
      sql = sql,
      schema = dataSource$schema,
      vocabulary_database_schema = dataSource$vocabularyDatabaseSchema,
      cohort_ids = cohortIds,
      database_id = quoteLiterals(databaseIds),
      table_name = dataSource$prefixTable("index_event_breakdown"),
      concept_table = dataSource$prefixVocabTable("concept"),
      snakeCaseToCamelCase = TRUE
    ) %>%
      tidyr::tibble()

  # Results exported before cohort_start_year was added to the results data
  # model have no year dimension - mark those rows so the UI can degrade
  if (!hasIndexEventBreakdownColumn(data,
                                    columnName = "cohortStartYear")) {
    data <- data %>%
      dplyr::mutate(cohortStartYear = NA_integer_, .after = "cohortId")
  }

  # drop any legacy breakdown rows that carry no year so the rows shown by year
  # sum to the unstratified totals
  data <- data %>%
    dplyr::filter(!is.na(.data$cohortStartYear))

  if (hasData(cohortStartYears)) {
    data <- data %>%
      dplyr::filter(.data$cohortStartYear %in% cohortStartYears)
  }

  data <- data %>%
    dplyr::inner_join(cohortCount,
                      by = c("databaseId", "cohortId")
    ) %>%
    dplyr::mutate(
      subjectPercent = .data$subjectCount / .data$cohortSubjects,
      conceptPercent = .data$conceptCount / .data$cohortEntries
    )

  return(data)
}

#' Plot the index event breakdown by cohort start year
#' @description
#' Plotly plot showing, for each database, the number of persons whose cohort
#' entry had each index event concept on the cohort start date, broken down by
#' the calendar year of the cohort start date. A concept whose series starts
#' late or ends early (e.g. a deprecated HCPCS code) shows up as a line that
#' stops before the other concepts.
#' @param data data.frame as returned by getIndexEventBreakdown containing
#'  conceptId, conceptName, cohortId, databaseId, cohortStartYear, subjectCount,
#'  conceptCount and subjectPercent
#' @param cohortTable data.frame with the cohortId and cohortName of the cohorts
#'  in data
#' @param showAsPercent if TRUE plot persons as a percentage of the cohort
#'  subjects (subjectPercent) rather than the persons count
#' @return a plotly object, or NULL when there is nothing to plot
#' @family CohortDiagnostics
#' @noRd
plotIndexEventBreakdownByYear <- function(data,
                                          cohortTable = NULL,
                                          showAsPercent = FALSE) {
  if (!hasData(data) || !"cohortStartYear" %in% colnames(data)) {
    return(NULL)
  }
  data <- data %>%
    dplyr::filter(
      .data$conceptId > 0,
      !is.na(.data$cohortStartYear),
      !is.na(.data$conceptName)
    )
  if (!hasData(data)) {
    return(NULL)
  }

  cohortNames <- as.character(data$cohortId)
  if (hasData(cohortTable) &&
        all(c("cohortId", "cohortName") %in% colnames(cohortTable))) {
    nameIndex <- match(data$cohortId, cohortTable$cohortId)
    found <- !is.na(nameIndex)
    cohortNames[found] <- as.character(cohortTable$cohortName[nameIndex[found]])
  }
  data <- data %>%
    dplyr::mutate(cohortName = cohortNames)

  showAsPercent <- isTRUE(showAsPercent)
  valueColumn <- if (showAsPercent) "subjectPercent" else "subjectCount"
  yAxisTitle <- if (showAsPercent) {
    "persons (% of cohort subjects)"
  } else {
    "persons"
  }
  formatValue <- function(value) {
    if (showAsPercent) {
      sprintf("%.2f%%", 100 * value)
    } else {
      scales::comma(value, accuracy = 1)
    }
  }

  # one series per database/cohort/concept, ordered by the total number of
  # persons so the most common index events get the most visible colours
  series <- data %>%
    dplyr::group_by(
      .data$databaseId,
      .data$cohortId,
      .data$cohortName,
      .data$conceptId,
      .data$conceptName
    ) %>%
    dplyr::summarise(
      totalPersons = sum(.data$subjectCount, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::arrange(
      dplyr::desc(.data$totalPersons),
      .data$cohortId,
      .data$conceptName
    ) %>%
    dplyr::mutate(seriesIndex = dplyr::row_number())
  # a fixed colour reference so a concept keeps its colour across databases
  seriesKey <- paste(series$cohortId, series$conceptId, sep = "-")
  seriesKeyOrder <- unique(seriesKey)
  nColors <- min(length(seriesKeyOrder), 12)
  colorPallet <- RColorBrewer::brewer.pal(max(c(3, nColors)), "Set3")
  series$color <- colorPallet[
    ((match(seriesKey, seriesKeyOrder) - 1) %% length(colorPallet)) + 1
  ]
  series$seriesLabel <- paste0(
    series$conceptId, ": ", series$conceptName
  )

  years <- sort(unique(data$cohortStartYear))
  databases <- unique(data$databaseId)
  nDatabases <- length(databases)
  subplots <- list()

  for (db in databases) {
    subplots[[length(subplots) + 1]] <-
      plotly::plot_ly(height = 400)
    subplots[[length(subplots)]] <- subplots[[length(subplots)]] %>%
      plotly::layout(
        plot_bgcolor = "#e5ecf6",
        xaxis = list(
          title = "cohort start year",
          tickmode = "array",
          tickvals = years,
          ticktext = as.character(years),
          zerolinecolor = "#ffff",
          zerolinewidth = 2,
          gridcolor = "ffff"
        ),
        yaxis = list(
          title = yAxisTitle,
          zerolinecolor = "#ffff",
          zerolinewidth = 2,
          gridcolor = "ffff"
        ),
        showlegend = FALSE
      )

    dbSeries <- series %>%
      dplyr::filter(.data$databaseId == db)
    for (i in seq_len(nrow(dbSeries))) {
      conceptData <- data %>%
        dplyr::filter(
          .data$databaseId == db,
          .data$cohortId == dbSeries$cohortId[[i]],
          .data$conceptId == dbSeries$conceptId[[i]]
        ) %>%
        dplyr::group_by(.data$cohortStartYear) %>%
        dplyr::summarise(
          value = sum(.data[[valueColumn]], na.rm = TRUE),
          persons = sum(.data$subjectCount, na.rm = TRUE),
          records = sum(.data$conceptCount, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        dplyr::arrange(.data$cohortStartYear)
      if (!hasData(conceptData)) {
        next
      }

      hoverText <- paste0(
        dbSeries$seriesLabel[[i]],
        "<br>Cohort = ", dbSeries$cohortName[[i]],
        "<br>Year = ", conceptData$cohortStartYear,
        "<br>Persons = ", scales::comma(conceptData$persons, accuracy = 1),
        "<br>Records = ", scales::comma(conceptData$records, accuracy = 1),
        "<br>Shown = ", formatValue(conceptData$value)
      )

      subplots[[length(subplots)]] <- subplots[[length(subplots)]] %>%
        plotly::add_markers(
          x = conceptData$cohortStartYear,
          y = conceptData$value,
          name = dbSeries$seriesLabel[[i]],
          text = hoverText,
          hoverinfo = "text",
          color = I(dbSeries$color[[i]]),
          showlegend = FALSE
        ) %>%
        plotly::add_lines(
          x = conceptData$cohortStartYear,
          y = conceptData$value,
          name = dbSeries$seriesLabel[[i]],
          text = hoverText,
          hoverinfo = "text",
          color = I(dbSeries$color[[i]]),
          showlegend = TRUE,
          legendgroup = dbSeries$seriesLabel[[i]]
        )
    }
  }

  # Add titles for each subplot - mirrors plotTimeDistribution
  # getIndexEventBreakdown() does not return databaseName, so fall back to the
  # databaseId rather than indexing a column that may not exist
  hasDatabaseName <- "databaseName" %in% colnames(data)
  topAnnotations <- list()
  for (i in seq_len(nDatabases)) {
    databaseName <- if (hasDatabaseName) {
      unique(data$databaseName[data$databaseId == databases[[i]]])
    } else {
      databases[[i]]
    }
    databaseName <- if (hasData(databaseName)) {
      databaseName[[1]]
    } else {
      databases[[i]]
    }
    topAnnotations[[length(topAnnotations) + 1]] <- list(
      text = addTextBreaks(text = databaseName, length = 25),
      showarrow = FALSE,
      x = (i - 1) / nDatabases + 1 / nDatabases * 0.5,
      y = 1.0,
      xref = "paper",
      yref = "paper",
      xanchor = "center",
      yanchor = "bottom"
    )
  }

  plt <- plotly::subplot(subplots,
    nrows = 1,
    shareY = FALSE,
    shareX = TRUE
  ) %>%
    plotly::layout(annotations = topAnnotations, showlegend = TRUE)

  return(plt)
}


indexEventBreakdownModule <- function(id,
                                      dataSource,
                                      selectedCohort,
                                      targetCohortId,
                                      selectedDatabaseIds,
                                      cohortCountTable = dataSource$cohortCountTable,
                                      databaseTable = dataSource$dbTable) {
  ns <- shiny::NS(id)

  serverFunction <- function(input, output, session) {

    output$selectedCohort <- shiny::renderUI(selectedCohort())

    # Does the loaded results data model have the cohort_start_year column?
    # FALSE for results exported by CohortDiagnostics versions predating the
    # year dimension - the year controls are then hidden and the table shows
    # the unstratified breakdown as before
    yearDimensionAvailable <- shiny::reactive(x = {
      if (length(targetCohortId()) == 0 || length(selectedDatabaseIds()) == 0) {
        return(FALSE)
      }
      return(hasIndexEventBreakdownColumnInResults(
        dataSource = dataSource,
        columnName = "cohortStartYear"
      ))
    })
    # logical (not a list of one) so the UI can use it directly
    yearDimensionAvailableFlag <- shiny::reactive(x = {
      isTRUE(yearDimensionAvailable())
    })

    # All years present in the results for the selected cohorts/databases, used
    # to build the year select input. NA rows (pre year-dimension results) are
    # dropped so nothing is offered when there is no year information.
    availableYears <- shiny::reactive(x = {
      if (!yearDimensionAvailableFlag()) {
        return(NULL)
      }
      data <- indexEventBreakDownDataAll()
      if (!hasData(data) ||
        !"cohortStartYear" %in% colnames(data)) {
        return(NULL)
      }
      years <- data$cohortStartYear %>%
        unique() %>%
        stats::na.omit() %>%
        as.numeric() %>%
        sort()
      if (!hasData(years)) {
        return(NULL)
      }
      return(years)
    })

    output$yearControls <- shiny::renderUI({
      if (!yearDimensionAvailableFlag() || !hasData(availableYears())) {
        return(NULL)
      }
      shiny::selectInput(
        inputId = ns("indexEventBreakdownYear"),
        label = "Cohort start year",
        choices = c(
          "All years" = "all",
          stats::setNames(
            as.character(availableYears()),
            as.character(availableYears())
          )
        ),
        selected = "all",
        multiple = FALSE,
        # selectize is initialised from the options present when the control is
        # first rendered. This control is re-rendered once the years are known,
        # and selectize does not pick up the new <option> elements, leaving the
        # dropdown with only "All years". A plain select always reflects the
        # server-side choices.
        selectize = FALSE
      )
    })

    # Index event breakdown -----------
    indexEventBreakDownDataAll <- shiny::reactive(x = {
      if (length(targetCohortId()) > 0 &&
        length(selectedDatabaseIds()) > 0) {
        data <- getIndexEventBreakdown(
          dataSource = dataSource,
          cohortCount = cohortCountTable,
          cohortIds = targetCohortId(),
          databaseIds = selectedDatabaseIds()
        )
        if (any(
          is.null(data),
          nrow(data) == 0
        )) {
          return(NULL)
        }
        if (!is.null(data)) {
          if (!"domainTable" %in% colnames(data)) {
            data$domainTable <- "Not in data"
          }
          if (!"domainField" %in% colnames(data)) {
            data$domainField <- "Not in data"
          }
          return(data)
        } else {
          return(NULL)
        }
      } else {
        return(NULL)
      }
    })

    # Limit the breakdown to the selected cohort start year (when available)
    indexEventBreakDownData <- shiny::reactive(x = {
      data <- indexEventBreakDownDataAll()
      if (!hasData(data)) {
        return(NULL)
      }
      if (!yearDimensionAvailableFlag()) {
        return(data)
      }
      selectedYear <- input$indexEventBreakdownYear
      if (is.null(selectedYear) ||
        length(selectedYear) == 0 ||
        is.na(selectedYear[[1]]) ||
        selectedYear[[1]] %in% c("", "all")) {
        return(data)
      }
      return(data %>% dplyr::filter(.data$cohortStartYear %in% as.numeric(selectedYear[[1]])))
    })

    indexEventBreakDownDataFilteredByRadioButton <-
      shiny::reactive(x = {
        data <- indexEventBreakDownData()
        if (!is.null(data) && nrow(data) > 0) {
          if (input$indexEventBreakdownTableRadioButton == "All") {
            return(data)
          } else if (input$indexEventBreakdownTableRadioButton == "Standard concepts") {
            return(data %>% dplyr::filter(.data$standardConcept == "S"))
          } else {
            return(data %>% dplyr::filter(is.na(.data$standardConcept)))
          }
        } else {
          return(NULL)
        }
      })

    # Same concept-type filtering as the table, but WITHOUT the cohort start
    # year filter. The year plot exists to show the trend across years, so it
    # must keep every year even when the table is restricted to one.
    indexEventBreakDownDataForYearPlot <- shiny::reactive(x = {
      data <- indexEventBreakDownDataAll()
      if (is.null(data) || nrow(data) == 0) {
        return(NULL)
      }
      if (input$indexEventBreakdownTableRadioButton == "All") {
        return(data)
      } else if (input$indexEventBreakdownTableRadioButton == "Standard concepts") {
        return(data %>% dplyr::filter(.data$standardConcept == "S"))
      } else {
        return(data %>% dplyr::filter(is.na(.data$standardConcept)))
      }
    })

    output$breakdownTable <- reactable::renderReactable(expr = {
      shiny::validate(shiny::need(length(selectedDatabaseIds()) > 0, "No data sources chosen"))
      shiny::validate(shiny::need(length(targetCohortId()) > 0, "No cohorts chosen chosen"))

      showDataAsPercent <- input$showAsPercent
      data <- indexEventBreakDownDataFilteredByRadioButton()

      shiny::validate(shiny::need(
        all(!is.null(data), nrow(data) > 0),
        "There is no data for the selected combination."
      ))

      shiny::validate(shiny::need(
        nrow(data) > 0,
        "No data available for selected combination."
      ))

      # the year column is only kept when the results have a year dimension
      showYear <- yearDimensionAvailableFlag() &&
        "cohortStartYear" %in% colnames(data)

      distinctColumns <- c(
        "conceptId",
        "conceptName",
        "domainField",
        "databaseId",
        "vocabularyId",
        "conceptCode",
        "conceptCount",
        "subjectCount",
        "subjectPercent",
        "conceptPercent"
      )
      if (showYear) {
        distinctColumns <- c(distinctColumns, "cohortStartYear")
      }

      data <- data %>%
        dplyr::arrange(.data$databaseId) %>%
        dplyr::select(dplyr::all_of(distinctColumns)) %>%
        dplyr::filter(.data$conceptId > 0) %>%
        dplyr::distinct()

      if (showDataAsPercent) {
        data <- data %>%
          dplyr::rename(
            "persons" = "subjectPercent"
          )
      } else {
        data <- data %>%
          dplyr::rename(
            "persons" = "subjectCount"
          )
      }

      data <- data %>%
        dplyr::rename(
          "records" = "conceptCount"
        )

      data <- data %>%
        dplyr::arrange(dplyr::desc(abs(dplyr::across(
          c("records", "persons")
        ))))

      keyColumnFields <-
        c("conceptId", "conceptName", "conceptCode", "domainField", "vocabularyId")
      if (showYear) {
        keyColumnFields <- c(keyColumnFields, "cohortStartYear")
      }
      if (input$indexEventBreakdownTableFilter == "Persons") {
        dataColumnFields <- c("persons")
        countLocation <- 1
      } else if (input$indexEventBreakdownTableFilter == "Records") {
        dataColumnFields <- c("records")
        countLocation <- 1
      } else {
        dataColumnFields <- c("persons", "records")
        countLocation <- 2
      }

      countsForHeader <-
        getDisplayTableHeaderCount(
          dataSource = dataSource,
          databaseIds = selectedDatabaseIds(),
          cohortIds = targetCohortId(),
          source = "cohort",
          fields = input$indexEventBreakdownTableFilter
        )

      getDisplayTableGroupedByDatabaseId(
        data = data,
        databaseTable = databaseTable,
        headerCount = countsForHeader,
        keyColumns = keyColumnFields,
        countLocation = countLocation,
        dataColumns = dataColumnFields,
        showDataAsPercent = showDataAsPercent,
        excludedColumnFromPercentage = "records",
        sort = TRUE
      )
    })

    # Plot of the breakdown by cohort start year - only when the results have
    # the year dimension (older results keep the table-only view)
    output$yearPlotArea <- shiny::renderUI({
      if (!yearDimensionAvailableFlag()) {
        return(NULL)
      }
      shiny::tagList(
        shiny::tags$h4("Index events by cohort start year"),
        shinycssloaders::withSpinner(plotly::plotlyOutput(
          outputId = ns("yearPlot"),
          width = "100%",
          height = "450px"
        ))
      )
    })

    output$yearPlot <- plotly::renderPlotly(expr = {
      shiny::validate(shiny::need(
        yearDimensionAvailableFlag(),
        "Results do not contain a cohort start year"
      ))
      data <- indexEventBreakDownDataForYearPlot()
      shiny::validate(shiny::need(
        hasData(data),
        "There is no data for the selected combination."
      ))
      plot <- plotIndexEventBreakdownByYear(
        data = data,
        cohortTable = dataSource$cohortTable,
        showAsPercent = isTRUE(input$showAsPercent)
      )
      shiny::validate(shiny::need(
        !is.null(plot),
        "There is no data to plot for the selected combination."
      ))
      return(plot)
    })
  }


  return(shiny::moduleServer(id, serverFunction))
}
