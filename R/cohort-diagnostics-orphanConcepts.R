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

#' Orphan Concepts View
#' @description
#' Use for customizing UI
#'
#' @param id    Namespace Id - use namespaced id ns("orphanConcepts") inside diagnosticsExplorer module
#' @export
orpahanConceptsView <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shinydashboard::box(
      collapsible = TRUE,
      collapsed = TRUE,
      title = "Orphan Concepts",
      width = "100%",
      shiny::htmlTemplate(system.file("cohort-diagnostics-www",  "orphanConcepts.html", package = utils::packageName()))
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
      title = NULL,
      width = NULL,
      htmltools::withTags(
        table(
          width = "100%",
          shiny::tags$tr(
            shiny::tags$td(
              shiny::radioButtons(
                inputId = ns("orphanConceptsType"),
                label = "Filters",
                choices = c("All", "Standard Only", "Non Standard Only"),
                selected = "All",
                inline = TRUE
              )
            ),
            shiny::tags$td(shiny::HTML("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;")),
            shiny::tags$td(
              shiny::radioButtons(
                inputId = ns("orphanConceptsColumFilterType"),
                label = "Display",
                choices = c("All", "Persons", "Records"),
                selected = "All",
                inline = TRUE
              )
            )
          )
        )
      ),
      shinycssloaders::withSpinner(reactable::reactableOutput(outputId = ns("orphanConceptsTable"))),
      csvDownloadButton(ns, "orphanConceptsTable")
    )
  )
}


getOrphanConceptResult <- function(dataSource,
                                   databaseIds,
                                   cohortId,
                                   conceptSetId = NULL) {
  sql <- "SELECT oc.*,
              cs.concept_set_name,
              c.concept_name,
              c.vocabulary_id,
              c.concept_code,
              c.standard_concept
            FROM  @results_database_schema.@orphan_table_name oc
            INNER JOIN  @results_database_schema.@cs_table_name cs
              ON oc.cohort_id = cs.cohort_id
                AND oc.concept_set_id = cs.concept_set_id
            INNER JOIN  @vocabulary_database_schema.@concept_table c
              ON oc.concept_id = c.concept_id
            WHERE oc.cohort_id = @cohort_id
              AND database_id in (@database_ids)
              {@concept_set_id != \"\"} ? { AND oc.concept_set_id IN (@concept_set_id)};"
  data <-
    dataSource$connectionHandler$queryDb(
      sql = sql,
      results_database_schema = dataSource$resultsDatabaseSchema,
      vocabulary_database_schema = dataSource$vocabularyDatabaseSchema,
      cohort_id = cohortId,
      database_ids = quoteLiterals(databaseIds),
      orphan_table_name = dataSource$prefixTable("orphan_concept"),
      cs_table_name = dataSource$prefixTable("concept_sets"),
      concept_table = dataSource$prefixVocabTable("concept"),
      concept_set_id = conceptSetId,
      snakeCaseToCamelCase = TRUE
    ) %>%
      tidyr::tibble()
  return(data)
}

orphanConceptsModule <- function(id,
                                 dataSource,
                                 selectedCohort,
                                 selectedDatabaseIds,
                                 targetCohortId,
                                 selectedConceptSets,
                                 conceptSetIds,
                                 databaseTable = dataSource$databaseTable) {
  ns <- shiny::NS(id)
  shiny::moduleServer(id, function(input, output, session) {
    output$selectedCohorts <- shiny::renderUI({ selectedCohort() })


    # Orphan concepts table --------------------
    orphanConceptsDataReactive <- shiny::reactive(x = {
      shiny::validate(shiny::need(length(targetCohortId()) > 0, "No cohorts chosen"))
      data <- getOrphanConceptResult(
        dataSource = dataSource,
        cohortId = targetCohortId(),
        databaseIds = selectedDatabaseIds()
      )
      
      if (!hasData(data)) {
        return(NULL)
      }
      data <- data %>%
        dplyr::arrange(dplyr::desc(.data$conceptCount))
      return(data)
    })
    
    # Reactive below developed for testing purposes
    # Focuses on filtering the standard vs. non-standard codes
    filteringStandardConceptsReactive <- shiny::reactive(x = {
      data <- orphanConceptsDataReactive()
      shiny::validate(shiny::need(hasData(data), "There is no data for the selected combination."))
      
      
      if (hasData(selectedConceptSets())) {
        if (!is.null(selectedConceptSets())) {
          if (length(conceptSetIds()) > 0) {
            data <- data %>%
              dplyr::filter(.data$conceptSetId %in% conceptSetIds())
          } else {
            data <- data[0,]
          }
        }
      }

      if (input$orphanConceptsType == "Standard Only") {
        data <- data %>%
          dplyr::filter(.data$standardConcept == "S")
      } else if (input$orphanConceptsType == "Non Standard Only") {
        data <- data %>%
          dplyr::filter(is.na(.data$standardConcept) |
                          (
                            all(!is.na(.data$standardConcept), .data$standardConcept != "S")
                          ))
      }
      
      return (data)
      
    })

    output$orphanConceptsTable <- reactable::renderReactable(expr = {
      data <- filteringStandardConceptsReactive()
      shiny::validate(shiny::need(hasData(data), "There is no data for the selected combination."))
    

      data <- data %>%
        dplyr::select(
          "databaseId",
          "cohortId",
          "conceptId",
          "conceptSubjects",
          "conceptCount"
        ) %>%
        dplyr::group_by(
          .data$databaseId,
          .data$cohortId,
          .data$conceptId
        ) %>%
        dplyr::summarise(
          conceptSubjects = sum(.data$conceptSubjects),
          conceptCount = sum(.data$conceptCount),
          .groups = "keep"
        ) %>%
        dplyr::ungroup() %>%
        dplyr::arrange(
          .data$databaseId,
          .data$cohortId
        ) %>%
        dplyr::inner_join(
          data %>%
            dplyr::select(
              "conceptId",
              "databaseId",
              "cohortId",
              "conceptName",
              "vocabularyId",
              "conceptCode"
            ),
          by = c("databaseId", "cohortId", "conceptId")
        ) %>%
        dplyr::rename(
          "persons" = "conceptSubjects",
          "records" = "conceptCount"
        ) %>%
        dplyr::arrange(dplyr::desc(abs(dplyr::across(
          c("records", "persons")
        ))))

      keyColumnFields <-
        c("conceptId", "conceptName", "vocabularyId", "conceptCode")
      if (input$orphanConceptsColumFilterType == "Persons") {
        dataColumnFields <- c("persons")
        countLocation <- 1
      } else if (input$orphanConceptsColumFilterType == "Records") {
        dataColumnFields <- c("records")
        countLocation <- 1
      } else {
        dataColumnFields <- c("persons", "records")
        countLocation <- 2
      }
      countsForHeader <-
        getDisplayTableHeaderCount(
          dataSource = dataSource,
          databaseIds = data$databaseId %>% unique(),
          cohortIds = data$cohortId %>% unique(),
          source = "cohort",
          fields = input$orphanConceptsColumFilterType
        )

      showDataAsPercent <- FALSE
      ## showDataAsPercent set based on UI selection - proportion

      displayTable <- getDisplayTableGroupedByDatabaseId(
        data = data,
        databaseTable = databaseTable,
        headerCount = countsForHeader,
        keyColumns = keyColumnFields,
        countLocation = countLocation,
        dataColumns = dataColumnFields,
        showDataAsPercent = showDataAsPercent,
        sort = TRUE
      )
      return(displayTable)
    })

  })
}
