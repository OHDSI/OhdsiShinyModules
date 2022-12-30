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


# NOTE: here it would be nice to use dbplyr tables - this would allow lazy loading of resources
# however, renaming the columns causes an error and its not obvious how it could be resolved
loadResultsTable <- function(dataSource, tableName, required = FALSE, tablePrefix = "") {
  selectTableName <- paste0(tablePrefix, tableName)
  resultsTablesOnServer <-
    tolower(DatabaseConnector::dbListTables(dataSource$connectionHandler$getConnection(),
                                            schema = dataSource$resultsDatabaseSchema))

  if (required || selectTableName %in% resultsTablesOnServer) {
    if (tableIsEmpty(dataSource, selectTableName)) {
      return(data.frame())
    }

    tryCatch(
    {
      table <- DatabaseConnector::dbReadTable(
        dataSource$connectionHandler$getConnection(),
        paste(dataSource$resultsDatabaseSchema, selectTableName, sep = ".")
      )
    },
      error = function(err) {
        stop(
          "Error reading from ",
          paste(dataSource$resultsDatabaseSchema, selectTableName, sep = "."),
          ": ",
          err$message
        )
      }
    )
    colnames(table) <-
      SqlRender::snakeCaseToCamelCase(colnames(table))
    return(table)
  }

  return(data.frame())
}

# Create empty objects in memory for all other tables. This is used by the Shiny app to decide what tabs to show:
tableIsEmpty <- function(dataSource, tableName) {
  sql <- "SELECT * FROM @result_schema.@table LIMIT 1"
  row <- data.frame()
  tryCatch({
    row <- dataSource$connectionHandler$queryDb(
      sql,
      result_schema = dataSource$resultsDatabaseSchema,
      table = tableName
    )

  }, error = function(...) {
    message("Table not found: ", tableName)
  })

  return(nrow(row) == 0)
}

#' Get enable cd reports from available data
#' @param dataSource           C
#' @export
getEnabledCdReports <- function(dataSource) {
  enabledReports <- c()
  resultsTables <- tolower(DatabaseConnector::dbListTables(dataSource$connectionHandler$getConnection(),
                                                           schema = dataSource$resultsDatabaseSchema))

  for (table in dataSource$dataModelSpecifications$tableName %>% unique()) {
    if (dataSource$prefixTable(table) %in% resultsTables) {
      if (!tableIsEmpty(dataSource, dataSource$prefixTable(table))) {
        enabledReports <- c(enabledReports, SqlRender::snakeCaseToCamelCase(table))
      }
    }
  }
  enabledReports <- c(enabledReports, "cohort", "database")

  enabledReports
}

#' Create a CD data source from a database
#'
#' @description use this to create an interface to cohort diagnostics results data
#' NOTE: I think this would make a good R6 class for other objects in this package so you could query them outside of
#' a shiny app. E.g. if you wanted to make a custom R markdown template
#'
#' @param connectionHandler An instance of a ResultModelManager::connectionHander - manages a connection to a database.
#' @param schema The schema containing the results tables in the database.
#' @param vocabularyDatabaseSchema The schema containing the vocabulary tables in the database. If not provided, defaults to `resultsDatabaseSchema`.
#' @param tablePrefix An optional prefix to add to the table names.
#' @param cohortTableName The name of the cohort table in the database.
#' @param databaseTableName The name of the database table in the database.
#' @param dataModelSpecificationsPath The path to a file containing specifications for the data model used by the database.
#' @return An object of class `CdDataSource`.
#'
#' @import utils
#' @importFrom DatabaseConnector dbListTables
#' #' @export
createCdDatabaseDataSource <- function(connectionHandler,
                                       schema,
                                       vocabularyDatabaseSchema = schema,
                                       tablePrefix = "",
                                       cohortTableName = paste0(tablePrefix, "cohort"),
                                       databaseTableName = paste0(tablePrefix, "database"),
                                       dataModelSpecificationsPath = system.file("cohort-diagnostics-ref",
                                                                                 "resultsDataModelSpecification.csv",
                                                                                 package = utils::packageName()),
                                       displayProgress = FALSE) {

  checkmate::assertR6(connectionHandler, "ConnectionHandler")
  checkmate::assertString(schema)
  checkmate::assertString(vocabularyDatabaseSchema, null.ok = TRUE)
  checkmate::assertString(tablePrefix, null.ok = TRUE)
  checkmate::assertString(cohortTableName, null.ok = TRUE)
  checkmate::assertString(databaseTableName, null.ok = TRUE)
  checkmate::assertFileExists(dataModelSpecificationsPath)

  if (is.null(vocabularyDatabaseSchema)) {
    vocabularyDatabaseSchema <- schema
  }
  if (is.null(tablePrefix)) {
    tablePrefix <- ""
  }
  if (is.null(cohortTableName)) {
    cohortTableName <- paste0(tablePrefix, "cohort")
  }
  if (is.null(databaseTableName)) {
    databaseTableName <- paste0(tablePrefix, "datatbase")
  }

  if (displayProgress)
    shiny::setProgress(value = 0.05, message = "Getting settings")

  dataSource <- list(
    connectionHandler = connectionHandler,
    resultsDatabaseSchema = schema,
    vocabularyDatabaseSchema = vocabularyDatabaseSchema,
    dbms = connectionHandler$dbms(),
    resultsTablesOnServer = tolower(DatabaseConnector::dbListTables(connectionHandler$getConnection(),
                                                                    schema = schema)),
    tablePrefix = tablePrefix,
    prefixTable = function(tableName) { paste0(tablePrefix, tableName) },
    prefixVocabTable = function(tableName) {
      # don't prexfix table if we us a dedicated vocabulary schema
      if (vocabularyDatabaseSchema == schema)
        return(paste0(tablePrefix, tableName))

      return(tableName)
    },
    cohortTableName = cohortTableName,
    databaseTableName = databaseTableName,
    dataModelSpecifications = read.csv(dataModelSpecificationsPath)
  )

  if (displayProgress)
    shiny::setProgress(value = 0.05, message = "Getting enabled reports")

  dataSource$enabledReports <- getEnabledCdReports(dataSource)

  if (displayProgress)
    shiny::setProgress(value = 0.1, message = "Getting database information")
  dataSource$databaseTable <- getDatabaseTable(dataSource)

  if (displayProgress)
    shiny::setProgress(value = 0.2, message = "Getting cohorts")

  dataSource$cohortTable <- getCohortTable(dataSource)

  if (displayProgress)
    shiny::setProgress(value = 0.6, message = "Getting concept sets")

  dataSource$conceptSets <- loadResultsTable(dataSource, "concept_sets", tablePrefix = dataSource$tablePrefix)

  if (displayProgress)
    shiny::setProgress(value = 0.7, message = "Getting counts")

  dataSource$cohortCountTable <- loadResultsTable(dataSource, "cohort_count", required = TRUE, tablePrefix = dataSource$tablePrefix)

  dataSource$enabledReports <- dataSource$enabledReports

  if (displayProgress)
    shiny::setProgress(value = 0.7, message = "Getting Temporal References")

  dataSource$temporalAnalysisRef <- loadResultsTable(dataSource, "temporal_analysis_ref", tablePrefix = dataSource$tablePrefix)

  dataSource$temporalChoices <- getResultsTemporalTimeRef(dataSource = dataSource)
  dataSource$temporalCharacterizationTimeIdChoices <- dataSource$temporalChoices %>%
    dplyr::arrange(sequence)

  dataSource$characterizationTimeIdChoices <- dataSource$temporalChoices %>%
    dplyr::filter(isTemporal == 0) %>%
    dplyr::filter(primaryTimeId == 1) %>%
    dplyr::arrange(sequence)


  if (!is.null(dataSource$temporalAnalysisRef)) {
    dataSource$temporalAnalysisRef <- dplyr::bind_rows(
      dataSource$temporalAnalysisRef,
      dplyr::tibble(
        analysisId = c(-201, -301),
        analysisName = c("CohortEraStart", "CohortEraOverlap"),
        domainId = "Cohort",
        isBinary = "Y",
        missingMeansZero = "Y"
      )
    )

    dataSource$domainIdOptions <- dataSource$temporalAnalysisRef %>%
      dplyr::select(domainId) %>%
      dplyr::pull(domainId) %>%
      unique() %>%
      sort()

    dataSource$analysisNameOptions <- dataSource$temporalAnalysisRef %>%
      dplyr::select(analysisName) %>%
      dplyr::pull(analysisName) %>%
      unique() %>%
      sort()
  }

  class(dataSource) <- "CdDataSource"
  return(dataSource)
}

# SO much of the app requires this table in memory - it would be much better to re-write queries to not need it!
getDatabaseTable <- function(dataSource) {
  databaseTable <- loadResultsTable(dataSource, dataSource$databaseTableName, required = TRUE)

  if (nrow(databaseTable) > 0 &
    "vocabularyVersion" %in% colnames(databaseTable)) {
    databaseTable <- databaseTable %>%
      dplyr::mutate(
        databaseIdWithVocabularyVersion = paste0(databaseId, " (", vocabularyVersion, ")")
      )
  }

  databaseTable
}

# SO much of the app requires this table in memory - it would be much better to re-write queries to not need it!
getCohortTable <- function(dataSource) {
  if (tableIsEmpty(dataSource, dataSource$cohortTableName)) {
    return(data.frame())
  }
  cohortTable <- dataSource$connectionHandler$queryDb("SELECT cohort_id, cohort_name FROM @schema.@table_name",
                                                      schema = dataSource$resultsDatabaseSchema,
                                                      table_name = dataSource$cohortTableName)

  # Old label
  if ("cohortDefinitionId" %in% names(cohortTable)) {
    cohortTable <- cohortTable %>% dplyr::mutate(cohortId = cohortDefinitionId)
  }

  cohortTable <- cohortTable %>%
    dplyr::arrange(cohortId) %>%
    dplyr::mutate(shortName = paste0("C", cohortId)) %>%
    dplyr::mutate(compoundName = paste0(shortName, ": ", cohortName))

  cohortTable
}

getResultsTemporalTimeRef <- function(dataSource) {
  sql <- "SELECT *
            FROM @results_database_schema.@table_name;"
  temporalTimeRef <-
    dataSource$connectionHandler$queryDb(
      sql = sql,
      results_database_schema = dataSource$resultsDatabaseSchema,
      table_name = dataSource$prefixTable("temporal_time_ref")
    )

  if (nrow(temporalTimeRef) == 0) {
    return(NULL)
  }

  temporalChoices <- temporalTimeRef %>%
    dplyr::mutate(temporalChoices = paste0("T (", startDay, "d to ", endDay, "d)")) %>%
    dplyr::arrange(startDay, endDay) %>%
    dplyr::select(
      timeId,
      startDay,
      endDay,
      temporalChoices
    ) %>%
    dplyr::mutate(primaryTimeId = dplyr::if_else(
      condition = (
        (startDay == -365 & endDay == -31) |
          (startDay == -30 & endDay == -1) |
          (startDay == 0 & endDay == 0) |
          (startDay == 1 & endDay == 30) |
          (startDay == 31 & endDay == 365) |
          (startDay == -365 & endDay == 0) |
          (startDay == -30 & endDay == 0)
      ),
      true = 1,
      false = 0
    )) %>%
    dplyr::mutate(isTemporal = dplyr::if_else(
      condition = (
        (endDay == 0 & startDay == -30) |
          (endDay == 0 & startDay == -180) |
          (endDay == 0 & startDay == -365) |
          (endDay == 0 & startDay == -9999)
      ),
      true = 0,
      false = 1
    )) %>%
    dplyr::arrange(startDay, timeId, endDay)

  temporalChoices <- dplyr::bind_rows(
    temporalChoices %>% dplyr::slice(0),
    dplyr::tibble(
      timeId = -1,
      temporalChoices = "Time invariant",
      primaryTimeId = 1,
      isTemporal = 0
    ),
    temporalChoices
  ) %>%
    dplyr::mutate(sequence = dplyr::row_number())

  return(temporalChoices)
}


#' Cohort Diagnostics Explorer main module
#' @param id                            module Id
#' @param connectionHandler             ResultModelManager ConnectionHander instance
#' @param resultDatabaseSettings        results database settings
#' @export
cohortDiagnosticsSever <- function(id,
                                   connectionHandler,
                                   resultDatabaseSettings,
                                   dataSource = NULL) {
  ns <- shiny::NS(id)

  checkmate::assertClass(dataSource, "CdDataSource", null.ok = TRUE)
  if (is.null(dataSource)) {
    checkmate::assertR6(connectionHandler, "ConnectionHandler", null.ok = FALSE)
    dataSource <-
      createCdDatabaseDataSource(
        connectionHandler = connectionHandler,
        schema = resultDatabaseSettings$schema,
        vocabularyDatabaseSchema = resultDatabaseSettings$vocabularyDatabaseSchema,
        tablePrefix = resultDatabaseSettings$tablePrefix,
        cohortTableName = resultDatabaseSettings$cohortTable,
        databaseTableName = resultDatabaseSettings$databaseTable,
        displayProgress = TRUE
      )
  }

  shiny::moduleServer(id, function(input, output, session) {
    databaseTable <- dataSource$databaseTable
    cohortTable <- dataSource$cohortTable
    conceptSets <- dataSource$conceptSets
    cohortCountTable <- dataSource$cohortCountTable
    enabledReports <- dataSource$enabledReports
    temporalAnalysisRef <- dataSource$temporalAnalysisRef
    temporalChoices <- dataSource$temporalChoices
    temporalCharacterizationTimeIdChoices <- dataSource$temporalCharacterizationTimeIdChoices
    characterizationTimeIdChoices <- dataSource$characterizationTimeIdChoices
    domainIdOptions <- dataSource$domainIdOptions
    analysisNameOptions <- dataSource$analysisNameOptions

    shiny::observe({

      selection <- c(
        "Cohort Definitions" = "cohortDefinitions",
        "Database Information" = "databaseInformation"
      )
      if ("cohortCount" %in% dataSource$enabledReports)
        selection["Cohort Counts"] <- "cohortCounts"

      if ("indexEvents" %in% dataSource$enabledReports)
        selection["Index Events"] <- "indexEvents"

      if ("temporalCovariateValue" %in% dataSource$enabledReports) {
        selection["Cohort Characterization"] <- "characterization"
        selection["Compare Cohort Characterization"] <- "compareCohortCharacterization"
        selection["Time Distributions"] <- "timeDistributions"
      }

      if ("relationship" %in% dataSource$enabledReports)
        selection["Cohort Overlap"] <- "cohortOverlap"

      if ("cohortInclusion" %in% dataSource$enabledReports)
        selection["Inclusion Rule Statistics"] <- "inclusionRules"

      if ("incidenceRate" %in% dataSource$enabledReports)
        selection["Incidence"] <- "incidenceRates"

      if ("visitContext" %in% dataSource$enabledReports)
        selection["Visit Context"] <- "visitContext"

      if ("includedSourceConcept" %in% dataSource$enabledReports)
        selection["Concepts In Data Source"] <- "conceptsInDataSource"

      if ("orphanConcepts" %in% dataSource$enabledReports)
        selection["Orphan Concepts"] <- "orphanConcepts"

      shiny::updateSelectInput(
        inputId = "tabs",
        label = "Select Report",
        choices = selection,
        selected = c("cohortDefinitions")
      )
    })

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
        !is.null(input$tabs) & !is.null(temporalCharacterizationTimeIdChoices)) {
        selectedTemporalTimeIds(
          temporalCharacterizationTimeIdChoices %>%
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

    # Characterization (Shared across) -------------------------------------------------
    ## Reactive objects ----
    ### getConceptSetNameForFilter ----
    getConceptSetNameForFilter <- shiny::reactive(x = {
      if (!hasData(targetCohortId())) {
        return(NULL)
      }

      dataSource$conceptSets %>%
        dplyr::filter(cohortId == targetCohortId()) %>%
        dplyr::mutate(name = conceptSetName) %>%
        dplyr::select(name)

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
                              cohortDefinitions = cohortSubset)
    }

    if ("includedSourceConcept" %in% enabledReports) {
      conceptsInDataSourceModule(id = "conceptsInDataSource",
                                 dataSource = dataSource,
                                 selectedCohort = selectedCohort,
                                 selectedDatabaseIds = selectedDatabaseIds,
                                 targetCohortId = targetCohortId,
                                 selectedConceptSets = selectedConceptSets,
                                 databaseTable = databaseTable)
    }

    if ("orphanConcept" %in% enabledReports) {
      orphanConceptsModule("orphanConcepts",
                           dataSource = dataSource,
                           databaseTable = databaseTable,
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
                                selectedCohort = selectedCohort,
                                targetCohortId = targetCohortId,
                                cohortCountTable = cohortCountTable,
                                selectedDatabaseIds = selectedDatabaseIds)
    }

    if ("visitContext" %in% enabledReports) {
      visitContextModule(id = "visitContext",
                         dataSource = dataSource,
                         selectedCohort = selectedCohort,
                         selectedDatabaseIds = selectedDatabaseIds,
                         targetCohortId = targetCohortId,
                         cohortCountTable = cohortCountTable,
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
                              selectedDatabaseIds = selectedDatabaseIds)

      characterizationModule(id = "characterization",
                             dataSource = dataSource)

      compareCohortCharacterizationModule(id = "compareCohortCharacterization",
                                          dataSource = dataSource)
    }

    if ("incidenceRate" %in% enabledReports) {
      incidenceRatesModule(id = "incidenceRates",
                           dataSource = dataSource,
                           selectedCohorts = selectedCohorts,
                           cohortIds = cohortIds,
                           selectedDatabaseIds = selectedDatabaseIds,
                           cohortTable = cohortTable)
    }

    if ("cohortInclusion" %in% enabledReports) {
      inclusionRulesModule(id = "inclusionRules",
                           dataSource = dataSource,
                           databaseTable = databaseTable,
                           selectedCohort = selectedCohort,
                           targetCohortId = targetCohortId,
                           selectedDatabaseIds = selectedDatabaseIds)

    }
    databaseInformationModule(id = "databaseInformation",
                              dataSource = dataSource,
                              selectedDatabaseIds = selectedDatabaseIds,
                              databaseTable = databaseTable)

  }

  )

}
