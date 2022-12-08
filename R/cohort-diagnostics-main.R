# Copyright 2022 Observational Health Data Sciences and Informatics
#
# This file is part of CohortDiagnostics
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

#' Cohort Diagnostics Explorer main module
#'
#' @param connectionHandler             ResultModelManager ConnectionHander instance
#' @param resultDatabaseSettings        results database settings
#' @export
cohortDiagnosticsSever <- function(id = "DiagnosticsExplorer",
                                   connectionHandler,
                                   resultDatabaseSettings) {

  checkmate::assertR6(connectionHandler, "ConnectionHandler")

  ns <- shiny::NS(id)
  shiny::moduleServer(id, function(input, output, session) {
    # Reacive: targetCohortId
    targetCohortId <- shiny::reactive({
      return(cohortTable$cohortId[cohortTable$compoundName == input$targetCohort])
    })

    # Reacive: cohortIds
    cohortIds <- shiny::reactive({
      cohortTable %>%
        dplyr::filter(compoundName %in% input$cohorts) %>%
        dplyr::select(cohortId) %>%
        dplyr::pull()
    })

    selectedConceptSets <- shiny::reactive({
      input$conceptSetsSelected
    })

    # conceptSetIds ----
    conceptSetIds <- shiny::reactive(x = {
      conceptSetsFiltered <- conceptSets %>%
        dplyr::filter(conceptSetName %in% selectedConceptSets()) %>%
        dplyr::filter(cohortId %in% targetCohortId()) %>%
        dplyr::select(conceptSetId) %>%
        dplyr::pull() %>%
        unique()
      return(conceptSetsFiltered)
    })

    databaseChoices <- databaseTable$databaseId
    names(databaseChoices) <- databaseTable$databaseName

    ## ReactiveValue: selectedDatabaseIds ----
    selectedDatabaseIds <- shiny::reactive({
      if (!is.null(input$tabs)) {
        if (input$tabs %in% c(
          "compareCohortCharacterization",
          "compareTemporalCharacterization",
          "temporalCharacterization",
          "databaseInformation"
        )) {
          return(input$database)
        } else {
          return(input$databases)
        }
      }
    })


    shiny::observe({
      shinyWidgets::updatePickerInput(session = session,
                                      inputId = "database",
                                      choices = databaseChoices,
                                      selected = databaseChoices[[1]],
      )
      shinyWidgets::updatePickerInput(session = session,
                                      inputId = "databases",
                                      choices = databaseChoices,
                                      selected = databaseChoices[[1]],
      )
    })

    ## ReactiveValue: selectedTemporalTimeIds ----
    selectedTemporalTimeIds <- reactiveVal(NULL)
    shiny::observeEvent(eventExpr = {
      list(
        input$timeIdChoices_open,
        input$timeIdChoices,
        input$tabs
      )
    }, handlerExpr = {
      if (isFALSE(input$timeIdChoices_open) ||
        !is.null(input$tabs) & !is.null(envir$temporalCharacterizationTimeIdChoices)) {
        selectedTemporalTimeIds(
          envir$temporalCharacterizationTimeIdChoices %>%
            dplyr::filter(temporalChoices %in% input$timeIdChoices) %>%
            dplyr::pull(timeId) %>%
            unique() %>%
            sort()
        )
      }
    })

    cohortSubset <- shiny::reactive({
      return(cohortTable %>%
               dplyr::arrange(cohortId))
    })

    shiny::observe({
      subset <- cohortSubset()$compoundName
      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "targetCohort",
        choicesOpt = list(style = rep_len("color: black;", 999)),
        choices = subset
      )
    })

    shiny::observe({
      subset <- cohortSubset()$compoundName
      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "cohorts",
        choicesOpt = list(style = rep_len("color: black;", 999)),
        choices = subset,
        selected = c(subset[1], subset[2])
      )
    })


    inputCohortIds <- shiny::reactive({
      if (input$tabs == "cohortCounts" |
        input$tabs == "cohortOverlap" |
        input$tabs == "incidenceRate" |
        input$tabs == "timeDistribution") {
        subset <- input$cohorts
      } else {
        subset <- input$targetCohort
      }

      return(subset)
    })

    shiny::observe({
      shinyWidgets::updatePickerInput(
        session = session,
        inputId = paste0("targetCohort", input$tabs),
        choicesOpt = list(style = rep_len("color: black;", 999)),
        choices = inputCohortIds(),
        selected = inputCohortIds()
      )
    })

    shiny::observe({
      shinyWidgets::updatePickerInput(
        session = session,
        inputId = paste0("database", input$tabs),
        choicesOpt = list(style = rep_len("color: black;", 999)),
        choices = selectedDatabaseIds(),
        selected = selectedDatabaseIds()
      )
    })

    shiny::observe({
      subset <- cohortSubset()$compoundName
      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "comparatorCohort",
        choicesOpt = list(style = rep_len("color: black;", 999)),
        choices = subset,
        selected = subset[2]
      )
    })

    # Characterization (Shared across) -------------------------------------------------
    ## Reactive objects ----
    ### getConceptSetNameForFilter ----
    getConceptSetNameForFilter <- shiny::reactive(x = {
      if (!hasData(targetCohortId()) || !hasData(selectedDatabaseIds())) {
        return(NULL)
      }

      jsonExpression <- cohortSubset() %>%
        dplyr::filter(cohortId == targetCohortId()) %>%
        dplyr::select(json)
      jsonExpression <-
        RJSONIO::fromJSON(jsonExpression$json, digits = 23)
      expression <-
        getConceptSetDetailsFromCohortDefinition(cohortDefinitionExpression = jsonExpression)
      if (is.null(expression)) {
        return(NULL)
      }

      expression <- expression$conceptSetExpression %>%
        dplyr::select(name)
      return(expression)
    })

    shiny::observe({
      subset <- getConceptSetNameForFilter()$name %>%
        sort() %>%
        unique()
      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "conceptSetsSelected",
        choicesOpt = list(style = rep_len("color: black;", 999)),
        choices = subset
      )
    })

    selectedCohorts <- shiny::reactive({
      cohorts <- cohortSubset() %>%
        dplyr::filter(cohortId %in% cohortIds()) %>%
        dplyr::arrange(cohortId) %>%
        dplyr::select(compoundName)
      return(apply(cohorts, 1, function(x) {
        tags$tr(lapply(x, tags$td))
      }))
    })

    selectedCohort <- shiny::reactive({
      return(input$targetCohort)
    })

    if ("cohort" %in% enabledReports) {
      cohortDefinitionsModule(id = "cohortDefinitions",
                              dataSource = dataSource,
                              cohortDefinitions = cohortSubset,
                              cohortTable = cohortTable,
                              cohortCount = cohortCountTable,
                              databaseTable = databaseTable)
    }

    if ("includedSourceConcept" %in% enabledReports) {
      conceptsInDataSourceModule(id = "conceptsInDataSource",
                                 dataSource = dataSource,
                                 selectedCohort = selectedCohort,
                                 selectedDatabaseIds = selectedDatabaseIds,
                                 targetCohortId = targetCohortId,
                                 selectedConceptSets = selectedConceptSets,
                                 cohortTable = cohortTable,
                                 databaseTable = databaseTable)
    }

    if ("orphanConcept" %in% enabledReports) {
      orphanConceptsModule("orphanConcepts",
                           dataSource = dataSource,
                           selectedCohort = selectedCohort,
                           selectedDatabaseIds = selectedDatabaseIds,
                           targetCohortId = targetCohortId,
                           selectedConceptSets = selectedConceptSets,
                           conceptSetIds = conceptSetIds)
    }

    if ("cohortCount" %in% enabledReports) {
      cohortCountsModule(id = "cohortCounts",
                         dataSource = dataSource,
                         cohortTable = cohortTable, # The injection of tables like this should be removed
                         databaseTable = databaseTable, # The injection of tables like this should be removed
                         selectedCohorts = selectedCohorts,
                         selectedDatabaseIds = selectedDatabaseIds,
                         cohortIds = cohortIds)
    }

    if ("indexEventBreakdown" %in% enabledReports) {
      indexEventBreakdownModule("indexEvents",
                                dataSource = dataSource,
                                cohortTable = cohortTable,
                                databaseTable = databaseTable,
                                selectedCohort = selectedCohort,
                                targetCohortId = targetCohortId,
                                selectedDatabaseIds = selectedDatabaseIds)
    }

    if ("visitContext" %in% enabledReports) {
      visitContextModule(id = "visitContext",
                         dataSource = dataSource,
                         selectedCohort = selectedCohort,
                         selectedDatabaseIds = selectedDatabaseIds,
                         targetCohortId = targetCohortId,
                         cohortTable = cohortTable,
                         databaseTable = databaseTable)
    }

    if ("relationship" %in% enabledReports) {
      cohortOverlapModule(id = "cohortOverlap",
                          dataSource = dataSource,
                          selectedCohorts = selectedCohorts,
                          selectedDatabaseIds = selectedDatabaseIds,
                          targetCohortId = targetCohortId,
                          cohortIds = cohortIds,
                          cohortTable = cohortTable)
    }

    if ("temporalCovariateValue" %in% enabledReports) {
      timeDistributionsModule(id = "timeDistributions",
                              dataSource = dataSource,
                              selectedCohorts = selectedCohorts,
                              cohortIds = cohortIds,
                              selectedDatabaseIds = selectedDatabaseIds,
                              cohortTable = cohortTable,
                              databaseTable = databaseTable)

      characterizationModule(id = "characterization",
                             dataSource = dataSource,
                             cohortTable = cohortTable,
                             databaseTable = databaseTable,
                             temporalAnalysisRef = envir$temporalAnalysisRef,
                             analysisNameOptions = envir$analysisNameOptions,
                             domainIdOptions = envir$domainIdOptions,
                             characterizationTimeIdChoices = envir$characterizationTimeIdChoices)

      compareCohortCharacterizationModule("compareCohortCharacterization",
                                          dataSource = dataSource,
                                          cohortTable = cohortTable,
                                          databaseTable = databaseTable,
                                          conceptSets = conceptSets,
                                          temporalAnalysisRef = envir$temporalAnalysisRef,
                                          analysisNameOptions = envir$analysisNameOptions,
                                          domainIdOptions = envir$domainIdOptions,
                                          temporalChoices = envir$temporalChoices)
    }

    if ("incidenceRate" %in% enabledReports) {
      incidenceRatesModule(id = "incidenceRates",
                           connectionHandler = dataSource,
                           selectedCohorts = selectedCohorts,
                           cohortIds = cohortIds,
                           selectedDatabaseIds = selectedDatabaseIds,
                           cohortTable = cohortTable)
    }

    databaseInformationModule(id = "databaseInformation",
                              dataSource = dataSource,
                              selectedDatabaseIds = selectedDatabaseIds,
                              databaseMetadata = envir$databaseMetadata)

  })

}
