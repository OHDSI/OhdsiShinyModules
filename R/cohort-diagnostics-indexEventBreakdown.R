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
        )
      ),
      shinycssloaders::withSpinner(reactable::reactableOutput(outputId = ns("breakdownTable"))),
      reactableCsvDownloadButton(ns, "breakdownTable"),
      # Only rendered when the results carry a cohort start year: the table then
      # has one row per code and expanding a row shows that code over time, for
      # every selected database at once.
      shiny::uiOutput(outputId = ns("percentNote")),
      shiny::uiOutput(outputId = ns("yearDetailHint"))
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

  # cohort_start_year arrives with migration 8. Results uploaded from an older
  # CohortDiagnostics (3.6.x) do not have the column at all, and results from a
  # schema that has not been migrated do not follow the -1 = overall convention.
  # Either way the rows are plain overall rows, so add the column as NA and
  # splitIndexEventBreakdown() will treat every row as overall. The -1 rows
  # themselves must be kept - they are the overall figures.
  hasYearColumn <- hasIndexEventBreakdownColumn(
    data,
    columnName = "cohortStartYear"
  )
  if (!hasYearColumn) {
    data <- data %>%
      dplyr::mutate(cohortStartYear = NA_integer_, .after = "cohortId")
  }

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
#' Split an index event breakdown into overall figures and year detail
#' @description
#' The breakdown is stored as two kinds of row:
#'
#'  * one overall row per code, carrying counts over the whole cohort, marked by
#'    `cohortStartYear == -1`; and
#'  * one row per code per cohort start year, for the trend.
#'
#' The overall rows must be used as they stand. They cannot be reconstructed by
#' adding the per-year rows together: `subjectCount` is a count of distinct
#' people *within* a period, so summing it across years counts a subject once per
#' year they appear in and can exceed the number of people in the cohort. For the
#' demo cohort the per-year subject counts sum to 2.6x the true distinct count.
#'
#' @param data data.frame from getIndexEventBreakdown
#' @return list with `overall` (one row per database/cohort/code) and `byYear`
#'  (the per-year rows, or NULL when the results predate the year breakdown).
#'  Results exported before the year breakdown have no `cohortStartYear` at all;
#'  those rows are treated as overall.
#' @family CohortDiagnostics
#' @noRd
splitIndexEventBreakdown <- function(data) {
  if (!hasData(data)) {
    return(list(overall = data, byYear = NULL))
  }

  if (!"cohortStartYear" %in% colnames(data)) {
    # results from before the year breakdown: every row is already an overall row
    return(list(overall = data, byYear = NULL))
  }

  overall <- data %>% dplyr::filter(is.na(.data$cohortStartYear) | .data$cohortStartYear == -1)
  byYear <- data %>% dplyr::filter(!is.na(.data$cohortStartYear) & .data$cohortStartYear != -1)

  # Older exports that were migrated have only overall rows tagged -1; newer ones
  # always provide both. Guard against a results set that somehow carries only
  # year rows, where the overall figures would otherwise silently disappear.
  if (!hasData(overall)) {
    return(list(overall = data, byYear = NULL))
  }

  return(list(
    overall = overall,
    byYear = if (hasData(byYear)) byYear else NULL
  ))
}

#' Prepare per-year rows for one code, across all selected databases
#' @description
#' Normalises the per-year rows of a single index event code so that every
#' selected database is represented, and computes the plotted value.
#'
#' Databases with no rows for the code are kept rather than dropped, so that a
#' database that genuinely has no occurrences is distinguishable from one that
#' failed to report. Trend lines are interpolated across a database's missing
#' years (`linear` gaps) so a gap in the source data does not read as a drop to
#' zero; the table still shows only the years that were actually reported.
#'
#' @param data one code's per-year rows (from the year-stratified breakdown)
#' @param databaseIds every selected database id, used to keep absent ones
#' @param databaseTable data frame with databaseId and databaseName
#' @param showAsPercent plot persons as a share of cohort subjects
#' @return a data frame ordered by database then year
#' @family CohortDiagnostics
#' @noRd
prepareIndexEventYearDetail <- function(data,
                                        databaseIds,
                                        databaseTable,
                                        showAsPercent = FALSE) {
  if (!hasData(data)) {
    return(NULL)
  }

  years <- data %>%
    dplyr::filter(
      !is.na(.data$cohortStartYear),
      .data$cohortStartYear != -1
    ) %>%
    dplyr::mutate(
      # share of the cohort's subjects with this code in this year; the
      # denominator is the cohort size for that database, so a percentage is
      # comparable across databases of very different size
      persons = if (showAsPercent) .data$subjectPercent else .data$subjectCount
    )

  if (!hasData(years)) {
    return(NULL)
  }

  if (hasData(databaseTable)) {
    years <- years %>%
      dplyr::left_join(
        databaseTable %>% dplyr::select("databaseId", "databaseName"),
        by = "databaseId"
      )
  }
  if (!"databaseName" %in% colnames(years)) {
    years <- years %>% dplyr::mutate(databaseName = .data$databaseId)
  }

  return(years %>% dplyr::arrange(.data$databaseId, .data$cohortStartYear))
}

#' Shared explanation of what the percentages mean
#' @description
#' One glossary note for the whole view, so that the meaning of the plotted
#' percentages is stated once and in epidemiological terms rather than repeated
#' under every plot.
#' @return a shiny tag
#' @family CohortDiagnostics
#' @noRd
indexEventBreakdownPercentNote <- function() {
  shiny::tags$div(
    class = "alert alert-info",
    style = "font-size: 92%; margin-bottom: 8px;",
    shiny::tags$strong("What the percentages mean. "),
    "Each figure is the proportion of people in a database's cohort who ",
    shiny::tags$em("had this code in that cohort start year"),
    ", out of all people in that same database's cohort. ",
    "The denominator is that database's whole cohort, not the number of ",
    "people starting in that particular year, so the percentages do not sum ",
    "to 100% across years or across codes; a code used for 20% of a cohort in ",
    "one year and 10% the next describes two different groups of people. ",
    "Percentages are comparable across databases of very different size, ",
    "which raw person counts are not. ",
    "Lines are broken where a database reported no cohort start year, and ",
    "these figures describe which code was recorded, not the rate at which ",
    "the condition occurred in that year."
  )
}

#' Detail panel for one index event code
#' @description
#' Renders the cohort-start-year breakdown for a single code across every
#' selected database: one trend plot carrying a line per database, laid out
#' beside one table that has a row per database and year. Side by side rather
#' than stacked, so the trend can be read against its numbers without
#' scrolling; they stack vertically only when the viewport is too narrow.
#' @param data one code's per-year rows (from the year-stratified breakdown)
#' @param databaseIds every selected database id, used to keep absent ones
#' @param databaseTable data frame with databaseId and databaseName
#' @param showAsPercent render persons as a percentage of cohort subjects
#' @param valueLabel what the persons column represents, for the table header
#' @return a shiny tag list
#' @family CohortDiagnostics
#' @noRd
indexEventBreakdownYearDetail <- function(data,
                                          databaseIds = NULL,
                                          databaseTable = NULL,
                                          showAsPercent = FALSE,
                                          valueLabel = "Persons") {
  years <- prepareIndexEventYearDetail(
    data = data,
    databaseIds = databaseIds,
    databaseTable = databaseTable,
    showAsPercent = showAsPercent
  )

  if (!hasData(years)) {
    return(shiny::tags$em("No cohort start year detail available."))
  }

  nDatabases <- dplyr::n_distinct(years$databaseId)

  # rows for the databases that actually reported a year breakdown
  plotted <- years %>% dplyr::filter(!is.na(.data$persons))

  if (!hasData(plotted)) {
    return(shiny::tags$em("No cohort start year detail available."))
  }

  # 300px keeps a readable aspect for one database and still fits four or five
  # lines legibly; grows slowly so many databases do not crowd the legend
  plotHeight <- max(300, 120 + 34 * nDatabases)

  hoverLabel <- if (showAsPercent) valueLabel else paste0(valueLabel, " (%{y:,.0f})")
  yAxisTitle <- if (showAsPercent) paste0(valueLabel, " of cohort") else valueLabel
  hovertemplate <- paste0(
    "<b>%{text}</b><br>",
    "Cohort start year %{x}<br>",
    hoverLabel, "<extra></extra>"
  )

  # a horizontal legend keeps short labels on one or two rows; with more
  # databases a vertical legend on the right is clearer than a wrapping one
  horizontalLegend <- nDatabases > 4 && nDatabases <= 6
  legendOrientation <- if (horizontalLegend) "h" else "v"
  legendX <- if (horizontalLegend) 0 else 1
  legendXAnchor <- if (horizontalLegend) "left" else "right"

  trendPlot <- plotly::plot_ly(
    data = plotted,
    x = ~cohortStartYear,
    y = ~persons,
    color = ~databaseName,
    colors = grDevices::hcl.colors(
      max(3, nDatabases),
      palette = "Dark 3"
    ),
    text = ~databaseName,
    type = "scatter",
    mode = "lines+markers",
    height = plotHeight,
    hovertemplate = hovertemplate
  ) %>%
    plotly::layout(
      margin = list(l = 55, r = 10, t = 10, b = 45),
      xaxis = list(
        title = "Cohort start year",
        tickmode = "array",
        tickvals = sort(unique(plotted$cohortStartYear)),
        ticktext = as.character(sort(unique(plotted$cohortStartYear)))
      ),
      yaxis = list(
        title = yAxisTitle,
        rangemode = "tozero"
      ),
      legend = list(
        orientation = legendOrientation,
        x = legendX,
        y = if (horizontalLegend) 1.12 else 1,
        xanchor = legendXAnchor,
        font = list(size = 10)
      ),
      hovermode = "closest"
    ) %>%
    plotly::config(displayModeBar = FALSE)

  detailTable <- reactable::reactable(
    data = years %>%
      dplyr::select("databaseName", "cohortStartYear", "persons", "conceptCount"),
    columns = list(
      databaseName = reactable::colDef(name = "Database", minWidth = 120),
      cohortStartYear = reactable::colDef(name = "Cohort start year", minWidth = 130),
      persons = reactable::colDef(
        name = valueLabel,
        format = if (showAsPercent) {
          reactable::colFormat(percent = TRUE, digits = 1)
        } else {
          reactable::colFormat(separator = TRUE, digits = 0)
        }
      ),
      conceptCount = reactable::colDef(
        name = "Records",
        format = reactable::colFormat(separator = TRUE, digits = 0)
      )
    ),
    defaultPageSize = 10,
    pagination = nrow(years) > 10,
    compact = TRUE,
    bordered = TRUE,
    fullWidth = TRUE,
    defaultSorted = list(databaseName = "asc", cohortStartYear = "asc")
  )

  return(shiny::tags$div(
    style = "padding: 8px 12px; background: #f7f7f7;",
    shiny::h5(paste0("Cohort start year detail by database (", nDatabases, ")")),
    # table beside the plot while there is room, stacked when there is not:
    # flex-wrap lets each child drop to its own line below its min-width
    shiny::div(
      class = "cd-year-detail",
      style = paste0(
        "display: flex; flex-wrap: wrap; gap: 12px; align-items: flex-start;"
      ),
      shiny::div(
        style = "flex: 1 1 480px; min-width: 320px;",
        trendPlot
      ),
      shiny::div(
        style = "flex: 0 1 420px; min-width: 280px;",
        detailTable
      )
    )
  ))
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

    # Is the year dimension available in these results?
    #
    # cohort_start_year was added by migration 8, so the appropriate test is
    # whether that migration has been applied - the same way cohort-overlap
    # gates on migration 6. Results uploaded from a 3.6.x CohortDiagnostics
    # predate the migration, and a schema that has not been migrated has
    # neither the column nor the -1 overall rows.
    #
    # The migration record is authoritative only when the schema actually has a
    # migrations table. If it does not, the schema was created incorrectly (the
    # package warns about this at start-up) and the recorded migrations are
    # unknowable, so fall back to checking for the column itself rather than
    # hiding the report for data that is in fact fine.
    yearDimensionAvailable <- shiny::reactive(x = {
      if (length(targetCohortId()) == 0 || length(selectedDatabaseIds()) == 0) {
        return(FALSE)
      }
      migrationRecorded <- hasData(dataSource$migrations)
      if (migrationRecorded && !migrationPresent(dataSource$migrations, 8)) {
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

    indexEventBreakDownDataFilteredByRadioButton <-
      shiny::reactive(x = {
        data <- indexEventBreakDownDataAll()
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

      distinctColumns <- c(
        "conceptId",
        "conceptName",
        "domainField",
        "databaseId",
        "vocabularyId",
        "conceptCode",
        "conceptCount",
        "subjectCount"
      )

      # Split the stored rows into the overall figures (one per code) and the
      # per-year detail. The overall rows are used exactly as stored - they are
      # NOT recovered by adding the year rows together, because subjectCount is
      # distinct-within-a-period and would be double counted.
      split <- splitIndexEventBreakdown(
        data %>% dplyr::filter(.data$conceptId > 0) %>% dplyr::distinct()
      )
      data <- split$overall %>% dplyr::arrange(.data$databaseId)
      perYearDetail <- split$byYear

      # persons must be worked out before the column list narrows, because the
      # percentage needs the cohort denominators
      if (showDataAsPercent) {
        # formatDataCellValueInDisplayTable multiplies by 100 itself, so this
        # must be a 0-1 fraction. The numerator is the stored overall subject
        # count, which is a distinct count over the whole cohort.
        data <- data %>%
          dplyr::mutate(
            persons = dplyr::if_else(
              !is.na(.data$cohortSubjects) & .data$cohortSubjects > 0,
              .data$subjectCount / .data$cohortSubjects,
              NA_real_
            )
          )
      } else {
        data <- data %>%
          dplyr::rename("persons" = "subjectCount")
      }

      # `persons` and `records` are created above, so keep them whatever the
      # column list says (the raw counts they are derived from are dropped)
      keepColumns <- unique(c(
        intersect(distinctColumns, colnames(data)),
        intersect(c("persons", "records"), colnames(data))
      ))
      data <- data %>%
        dplyr::select(dplyr::all_of(keepColumns))

      data <- data %>%
        dplyr::rename(
          "records" = "conceptCount"
        )

      data <- data %>%
        dplyr::arrange(dplyr::desc(abs(dplyr::across(
          c("records", "persons")
        ))))

      # one row per code when year-stratified, so cohortStartYear is no longer a
      # key column - it lives in the expandable detail instead
      keyColumnFields <-
        c("conceptId", "conceptName", "conceptCode", "domainField", "vocabularyId")
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

      # When year-stratified, each summary row expands into that code's
      # per-year counts, for *every* selected database at once. The summary
      # table is small (one row per code) but the per-year detail can be very
      # large - thousands of codes times decades of years - so it is never sent
      # to the browser. Instead it is split once here, keyed by code only (not
      # by database), and an expansion looks up its own slice.
      #
      # details() receives the displayed data frame and a row index into it,
      # so the code is identified from that row rather than from any client
      # side state. That keeps working if the table is later switched to
      # server side pagination, where the index is not a position in the full
      # data.
      details <- NULL
      if (!is.null(perYearDetail)) {
        detailByCode <- split(perYearDetail, as.character(perYearDetail$conceptId))
        personLabel <- if (showDataAsPercent) "Persons (%)" else "Persons"
        details <- function(index) {
          row <- data[index, , drop = FALSE]
          if (nrow(row) == 0) {
            return(NULL)
          }
          rows <- detailByCode[[as.character(row$conceptId[[1]])]]
          if (is.null(rows)) {
            return(NULL)
          }
          indexEventBreakdownYearDetail(
            data = rows,
            databaseIds = selectedDatabaseIds(),
            databaseTable = databaseTable,
            showDataAsPercent = showDataAsPercent,
            valueLabel = personLabel
          )
        }
      }

      getDisplayTableGroupedByDatabaseId(
        data = data,
        databaseTable = databaseTable,
        headerCount = countsForHeader,
        keyColumns = keyColumnFields,
        countLocation = countLocation,
        dataColumns = dataColumnFields,
        showDataAsPercent = showDataAsPercent,
        excludedColumnFromPercentage = "records",
        sort = TRUE,
        details = details
      )
    })

    # Hint shown under the table when the results carry a cohort start year, so
    # it is obvious that a row expands into that code's year-by-year detail
    # across all selected databases
    output$yearDetailHint <- shiny::renderUI({
      if (!yearDimensionAvailableFlag()) {
        return(NULL)
      }
      shiny::tags$p(
        class = "text-muted",
        style = "font-size: 90%; margin-top: 6px;",
        "Select a row to see that code's count for each cohort start year, for every selected database."
      )
    })

    # State the meaning of the percentages once for the whole view
    output$percentNote <- shiny::renderUI({
      if (!yearDimensionAvailableFlag()) {
        return(NULL)
      }
      indexEventBreakdownPercentNote()
    })
  }


  return(shiny::moduleServer(id, serverFunction))
}
