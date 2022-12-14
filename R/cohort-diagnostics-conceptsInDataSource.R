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

#' concepts In DataSource View
#' @description
#' Use for customizing UI
#'
#' @param id    Namespace Id - use namespaced id ns("conceptsInDataSource") inside diagnosticsExplorer module
#' @export
conceptsInDataSourceView <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shinydashboard::box(
      collapsible = TRUE,
      collapsed = TRUE,
      title = "Concepts in Data Source",
      width = "100%",
      shiny::htmlTemplate(system.file("cohort-diagnostics-www",  "conceptsInDataSource.html", package = utils::packageName()))
    ),
    shinydashboard::box(
      status = "warning",
      width = "100%",
      tags$div(
        style = "max-height: 100px; overflow-y: auto",
        shiny::uiOutput(outputId = ns("selectedCohorts"))
      )
    ),
    shinydashboard::box(
      title = NULL,
      width = NULL,
      tags$table(
        width = "100%",
        tags$tr(
          tags$td(
            shiny::radioButtons(
              inputId = ns("includedType"),
              label = "",
              choices = c("Source fields", "Standard fields"),
              selected = "Standard fields",
              inline = TRUE
            )
          ),
          tags$td(
            shiny::radioButtons(
              inputId = ns("conceptsInDataSourceTableColumnFilter"),
              label = "",
              choices = c("Both", "Persons", "Records"),
              #
              selected = "Persons",
              inline = TRUE
            )
          )
        ),
      ),
      shinycssloaders::withSpinner(reactable::reactableOutput(outputId = ns("conceptsInDataSourceTable"))),
      csvDownloadButton(ns, "conceptsInDataSourceTable")
    )
  )
}


getConceptsInCohort <-
  function(dataSource,
           cohortId,
           databaseIds) {
    sql <- "SELECT concepts.*,
            	c.concept_name,
            	c.vocabulary_id,
            	c.domain_id,
            	c.standard_concept,
            	c.concept_code
            FROM (
            	SELECT isc.database_id,
            		isc.cohort_id,
            		isc.concept_id,
            		0 source_concept_id,
            		max(concept_subjects) concept_subjects,
            		sum(concept_count) concept_count
            	FROM @results_database_schema.@table_name isc
            	WHERE isc.cohort_id = @cohort_id
            		AND isc.database_id IN (@database_ids)
            	GROUP BY isc.database_id,
            		isc.cohort_id,
            		isc.concept_id

            	UNION

            	SELECT c.database_id,
            		c.cohort_id,
            		c.source_concept_id as concept_id,
            		1 source_concept_id,
            		max(c.concept_subjects) concept_subjects,
            		sum(c.concept_count) concept_count
            	FROM @results_database_schema.@table_name c
            	WHERE c.cohort_id = @cohort_id
            		AND c.database_id IN (@database_ids)
            	GROUP BY
            	    c.database_id,
            		c.cohort_id,
            		c.source_concept_id
            	) concepts
            INNER JOIN @results_database_schema.@concept_table c ON concepts.concept_id = c.concept_id
            WHERE c.invalid_reason IS NULL;"
    data <-
      dataSource$connectionHandler$queryDb(
        sql = sql,
        results_database_schema = dataSource$resultsDatabaseSchema,
        cohort_id = cohortId,
        database_ids = quoteLiterals(databaseIds),
        table_name = dataSource$prefixTable("included_source_concept"),
        concept_table = dataSource$prefixTable("concept"),
        snakeCaseToCamelCase = TRUE
      ) %>%
        tidyr::tibble()
    return(data)
  }


conceptsInDataSourceModule <- function(id,
                                       dataSource,
                                       selectedCohort,
                                       selectedDatabaseIds,
                                       targetCohortId,
                                       selectedConceptSets,
                                       databaseTable) {
  ns <- shiny::NS(id)
  shiny::moduleServer(id, function(input, output, session) {
    output$selectedCohorts <- shiny::renderUI({ selectedCohort() })
    # Concepts in data source------
    conceptsInDataSourceReactive <- shiny::reactive(x = {
      validate(need(
        all(!is.null(selectedDatabaseIds()), length(selectedDatabaseIds()) > 0),
        "No data sources chosen"
      ))
      validate(need(
        all(!is.null(targetCohortId()), length(targetCohortId()) > 0),
        "No cohort chosen"
      ))
      data <- getConceptsInCohort(
        dataSource = dataSource,
        cohortId = targetCohortId(),
        databaseIds = selectedDatabaseIds()
      )
      return(data)
    })

    conceptSetIds <- shiny::reactive({
      selectedConceptSets()
    })

    getResolvedConcepts <- shiny::reactive({
      output <- resolvedConceptSet(
        dataSource = dataSource,
        databaseIds = selectedDatabaseIds(),
        cohortId = targetCohortId()
      )
      if (!hasData(output)) {
        return(NULL)
      }
      return(output)
    })

    ### getMappedConceptsReactive ----
    getMappedConcepts <- shiny::reactive({
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Getting concepts mapped to concept ids resolved by concept set expression (may take time)", value = 0)
      output <- mappedConceptSet(dataSource = dataSource,
                                 databaseIds = selectedDatabaseIds(),
                                 cohortId = targetCohortId())
      if (!hasData(output)) {
        return(NULL)
      }
      return(output)
    })

    getFilteredConceptIds <- shiny::reactive({
      validate(need(hasData(selectedDatabaseIds()), "No data sources chosen"))
      validate(need(hasData(targetCohortId()), "No cohort chosen"))
      validate(need(hasData(conceptSetIds()), "No concept set id chosen"))
      resolved <- getResolvedConcepts()
      mapped <- getMappedConcepts()
      output <- c()
      if (hasData(resolved)) {
        resolved <- resolved %>%
          dplyr::filter(databaseId %in% selectedDatabaseIds()) %>%
          dplyr::filter(cohortId %in% targetCohortId()) %>%
          dplyr::filter(conceptSetId %in% conceptSetIds())
        output <- c(output, resolved$conceptId) %>% unique()
      }
      if (hasData(mapped)) {
        mapped <- mapped %>%
          dplyr::filter(databaseId %in% selectedDatabaseIds()) %>%
          dplyr::filter(cohortId %in% targetCohortId()) %>%
          dplyr::filter(conceptSetId %in% conceptSetIds())
        output <- c(output, mapped$conceptId) %>% unique()
      }

      if (hasData(output)) {
        return(output)
      } else {
        return(NULL)
      }
    })

    output$conceptsInDataSourceTable <- reactable::renderReactable(expr = {
      validate(need(hasData(selectedDatabaseIds()), "No cohort chosen"))
      validate(need(hasData(targetCohortId()), "No cohort chosen"))

      data <- conceptsInDataSourceReactive()
      validate(need(
        hasData(data),
        "No data available for selected combination"
      ))
      if (hasData(selectedConceptSets())) {
        if (length(getFilteredConceptIds()) > 0) {
          data <- data %>%
            dplyr::filter(conceptId %in% getFilteredConceptIds())
        }
      }
      validate(need(
        hasData(data),
        "No data available for selected combination"
      ))

      if (input$includedType == "Source fields") {
        data <- data %>%
          dplyr::filter(conceptId > 0) %>%
          dplyr::filter(sourceConceptId == 1) %>%
          dplyr::rename(standard = standardConcept)
        keyColumnFields <-
          c("conceptId", "conceptName", "vocabularyId", "conceptCode")
      }
      if (input$includedType == "Standard fields") {
        data <- data %>%
          dplyr::filter(conceptId > 0) %>%
          dplyr::filter(sourceConceptId == 0) %>%
          dplyr::rename(standard = standardConcept)
        keyColumnFields <-
          c("conceptId", "conceptName", "vocabularyId")
      }

      validate(need(hasData(data), "No data available for selected combination"))
      data <- data %>%
        dplyr::rename(
          persons = conceptSubjects,
          records = conceptCount
        ) %>%
        dplyr::arrange(dplyr::desc(abs(dplyr::across(c("records", "persons")))))

      if (input$conceptsInDataSourceTableColumnFilter == "Persons") {
        dataColumnFields <- c("persons")
        countLocation <- 1
      } else if (input$conceptsInDataSourceTableColumnFilter == "Records") {
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
          fields = input$conceptsInDataSourceTableColumnFilter
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
