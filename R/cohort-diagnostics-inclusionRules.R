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

#' inclusion Rules View
inclusionRulesView <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
     shinydashboard::box(
      collapsible = TRUE,
      collapsed = TRUE,
      title = "Inclusion Rules",
      width = "100%",
      shiny::htmlTemplate(system.file("cohort-diagnostics-www",  "inclusionRuleStats.html", package = utils::packageName()))
    ),
    shinydashboard::box(
      status = "warning",
      width = "100%",
      tags$div(
        style = "max-height: 100px; overflow-y: auto",
        shiny::uiOutput(outputId = ns("selectedCohort"))
      )
    ),
    shinydashboard::box(
      title = NULL,
      width = NULL,
      htmltools::withTags(
        table(
          width = "100%",
          tr(
            td(
              align = "left",
              shiny::radioButtons(
                inputId = ns("inclusionRuleTableFilters"),
                label = "Inclusion Rule Events",
                choices = c("All", "Meet", "Gain", "Remain"),
                selected = "All",
                inline = TRUE
              )
            ),
            tags$td(
              shiny::checkboxInput(
                inputId = ns("inclusionRulesShowAsPercent"),
                label = "Show as percent",
                value = TRUE
              )
            ),
            td(
              align = "right",
            )
          )
        )
      ),
      shinycssloaders::withSpinner(reactable::reactableOutput(outputId = ns("inclusionRuleTable"))),
      csvDownloadButton(ns, "inclusionRuleTable")
    )
  )
}

#' inclusion Rules Module
#'
#'
#'
#'
inclusionRulesModule <- function(id,
                                 dataSource,
                                 cohortTable,
                                 databaseTable,
                                 selectedCohort,
                                 targetCohortId,
                                 selectedDatabaseIds) {

  shiny::moduleServer(id, function(input, output, session) {
    output$selectedCohort <- shiny::renderUI(selectedCohort())

    # Inclusion rules table ------------------
    output$inclusionRuleTable <- reactable::renderReactable(expr = {
      validate(need(length(selectedDatabaseIds()) > 0, "No data sources chosen"))
      table <- getInclusionRuleStats(
        dataSource = dataSource,
        cohortIds = targetCohortId(),
        databaseIds = selectedDatabaseIds(),
        mode = 0
      )
      validate(need(hasData(table), "There is no data for the selected combination."))

      showDataAsPercent <- input$inclusionRulesShowAsPercent

      if (showDataAsPercent) {
        table <- table %>%
          dplyr::mutate(
            Meet = meetSubjects / totalSubjects,
            Gain = gainSubjects / totalSubjects,
            Remain = remainSubjects / totalSubjects,
            id = ruleSequenceId
          )
      } else {
        table <- table %>%
          dplyr::mutate(
            Meet = meetSubjects,
            Gain = gainSubjects,
            Remain = remainSubjects,
            Total = totalSubjects,
            id = ruleSequenceId
          )
      }

      table <- table %>%
        dplyr::arrange(cohortId,
                       databaseId,
                       id)

      validate(need(
        (nrow(table) > 0),
        "There is no data for the selected combination."
      ))

      keyColumnFields <-
        c("id", "ruleName")
      countLocation <- 1
      
      if (any(!hasData(input$inclusionRuleTableFilters),
              input$inclusionRuleTableFilters == "All")) {
        dataColumnFields <- c("Meet", "Gain", "Remain")
      } else {
        dataColumnFields <- c(input$inclusionRuleTableFilters)
      }

      if (all(hasData(showDataAsPercent), !showDataAsPercent)) {
        dataColumnFields <- c(dataColumnFields, "Total")
      }

      countsForHeader <-
        getDisplayTableHeaderCount(
          dataSource = dataSource,
          databaseIds = selectedDatabaseIds(),
          cohortIds = targetCohortId(),
          source = "cohort",
          fields = "Persons"
        )

      maxCountValue <-
        getMaxValueForStringMatchedColumnsInDataFrame(
          data = table,
          string = dataColumnFields
        )

      getDisplayTableGroupedByDatabaseId(
        data = table,
        databaseTable = databaseTable,
        headerCount = countsForHeader,
        keyColumns = keyColumnFields,
        countLocation = countLocation,
        dataColumns = dataColumnFields,
        showDataAsPercent = showDataAsPercent,
        sort = FALSE
      )
    })
  })
}
