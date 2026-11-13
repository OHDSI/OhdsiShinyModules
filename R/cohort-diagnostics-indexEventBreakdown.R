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
      # has one row per code and expanding a row shows that code over time.
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

  # Results exported before cohort_start_year was added to the results data
  # model have no year dimension - mark those rows so the UI can degrade
  hasYearColumn <- hasIndexEventBreakdownColumn(
    data,
    columnName = "cohortStartYear"
  )
  if (!hasYearColumn) {
    # no year dimension at all: treat every row as an overall row
    data <- data %>%
      dplyr::mutate(cohortStartYear = NA_integer_, .after = "cohortId")
  }
  # Note: -1 rows are the OVERALL figures for each code and must be kept -
  # splitIndexEventBreakdown() separates them from the per-year rows.

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

#' Detail panel for one index event code
#' @description
#' Renders the cohort-start-year breakdown for a single code: a small table of
#' counts per year and a sparkline of the trend. Used as the row detail of the
#' summary table, so that only the codes the user is interested in are shown
#' year by year rather than every code at once.
#' @param data one code's per-year rows (from the year-stratified breakdown)
#' @param showAsPercent render persons as a percentage of cohort subjects
#' @param valueLabel what the persons column represents, for the table header
#' @return a shiny tag list
#' @family CohortDiagnostics
#' @noRd
indexEventBreakdownYearDetail <- function(data,
                                          showAsPercent = FALSE,
                                          valueLabel = "Persons") {
  if (!hasData(data)) {
    return(shiny::tags$em("No cohort start year detail available."))
  }

  years <- data %>%
    dplyr::filter(!is.na(.data$cohortStartYear)) %>%
    dplyr::arrange(.data$cohortStartYear) %>%
    dplyr::mutate(
      persons = if (showAsPercent) .data$subjectPercent else .data$subjectCount
    )

  if (!hasData(years)) {
    return(shiny::tags$em("No cohort start year detail available."))
  }

  # a single year has no trend to plot, so show only the table
  sparkline <- NULL
  if (dplyr::n_distinct(years$cohortStartYear) > 1) {
    sparkline <- plotly::plot_ly(
      data = years,
      x = ~cohortStartYear,
      y = ~persons,
      type = "scatter",
      mode = "lines+markers",
      height = 160,
      hovertemplate = paste0(
        "Year %{x}<br>",
        valueLabel, " %{y:,.0f}<extra></extra>"
      )
    ) %>%
      plotly::layout(
        margin = list(l = 40, r = 10, t = 10, b = 30),
        xaxis = list(title = "", tickmode = "array",
                     tickvals = years$cohortStartYear,
                     ticktext = as.character(years$cohortStartYear)),
        yaxis = list(title = valueLabel, rangemode = "tozero"),
        showlegend = FALSE
      )
  }

  detailTable <- reactable::reactable(
    data = years %>%
      dplyr::select("cohortStartYear", "persons", "conceptCount"),
    columns = list(
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
    fullWidth = FALSE
  )

  return(shiny::tags$div(
    style = "padding: 8px 12px; background: #f7f7f7;",
    detailTable,
    sparkline
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
      # per-year counts. The details function receives the displayed data frame
      # and the row index, so the code is looked up by conceptId + databaseId,
      # both of which survive the wide pivot as key columns.
      details <- NULL
      if (!is.null(perYearDetail)) {
        detailRows <- perYearDetail
        personLabel <- if (showDataAsPercent) "Persons (%)" else "Persons"
        details <- function(index) {
          row <- data[index, , drop = FALSE]
          if (nrow(row) == 0) {
            return(NULL)
          }
          rows <- detailRows %>%
            dplyr::filter(
              .data$conceptId == row$conceptId[[1]],
              .data$databaseId == row[["databaseId"]][[1]]
            )
          indexEventBreakdownYearDetail(
            data = rows,
            showAsPercent = showDataAsPercent,
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
    output$yearDetailHint <- shiny::renderUI({
      if (!yearDimensionAvailableFlag()) {
        return(NULL)
      }
      shiny::tags$p(
        class = "text-muted",
        style = "font-size: 90%; margin-top: 6px;",
        "Select a row to see that code's count for each cohort start year."
      )
    })
  }


  return(shiny::moduleServer(id, serverFunction))
}
