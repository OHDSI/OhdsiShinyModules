context("characterization-database")

# move this to setup as will be used by all modules
targetCohort <- OhdsiShinyModules:::getTargetsUsedInCharMain(
  connectionHandler = connectionHandlerCharacterization,
  schema = resultDatabaseSettingsCharacterization$schema,
  cTablePrefix = resultDatabaseSettingsCharacterization$cTablePrefix,
  cgTablePrefix = resultDatabaseSettingsCharacterization$cgTablePrefix,
  ciTablePrefix = resultDatabaseSettingsCharacterization$incidenceTablePrefix
)

targets4module <- targetCohort %>% dplyr::filter(as.integer(.data$databaseComparator) == 1)

characterizationTargetTable <- OhdsiReportGenerator::getCharacterizationTargetSettings(
  connectionHandler = connectionHandlerCharacterization,
  schema = resultDatabaseSettingsCharacterization$schema,
  targetIds = targets4module$cohortDefinitionId[1],
  addDatabaseDetails = TRUE,
  databaseTable = resultDatabaseSettingsCharacterization$databaseTable,
  cgTablePrefix = resultDatabaseSettingsCharacterization$cgTablePrefix,
  cTablePrefix = resultDatabaseSettingsCharacterization$cTablePrefix
)

# keep only rows for this module
characterizationTargetTable <- characterizationTargetTable %>%
  dplyr::filter(as.integer(.data$databaseComparator) == 1)

shiny::testServer(
  app = characterizationDatabaseComparisonServer, 
  args = list(
    connectionHandler = connectionHandlerCharacterization,
    resultDatabaseSettings = resultDatabaseSettingsCharacterization,
    reactiveCharacterizationTargetTable = shiny::reactive(characterizationTargetTable)
    ), 
  expr = {

    testthat::expect_true(nrow(reactiveCharacterizationTargetTable()) > 0)

    # target row is initially NULL
    testthat::expect_true(is.null(reactiveCharacterizationTargetRowId()))

    # Trigger module table initialization once so reset observers settle.
    testthat::expect_true(nrow(moduleCharacterizationTargetTable()) > 0)
    session$flushReact()

    reactiveCharacterizationTargetRowId(1)
    session$flushReact()

    # In testServer, selection can be reset on first table change; set again once.
    if (nrow(reactiveTargetRow()) == 0) {
      reactiveCharacterizationTargetRowId(1)
      session$flushReact()
    }

    # reactiveTargetRow() should now be selected
    testthat::expect_true(nrow(reactiveTargetRow()) == 1)

    # Find a target row + database pair that has comparator data.
    selectedTargetRowId <- NA_integer_
    selectedDatabaseId <- NA_character_
    selectedDatabaseName <- NA_character_

    for (targetRow in seq_len(nrow(moduleCharacterizationTargetTable()))) {
      reactiveCharacterizationTargetRowId(targetRow)
      session$flushReact()

      if (nrow(reactiveTargetRow()) == 0) {
        next
      }

      if (is.null(databaseNames()) || length(databaseNames()) == 0 || is.null(databaseIds()) || length(databaseIds()) == 0) {
        next
      }

      for (dbIndex in seq_along(databaseIds())) {
        candidate <- characterizatonGetCohortData(
          connectionHandler = connectionHandler,
          resultDatabaseSettings = resultDatabaseSettings,
          characterizationTargetIds = reactiveTargetRow()$characterizationTargetId,
          databaseIds = databaseIds()[dbIndex],
          minThreshold = 0.02
        )

        hasBinary <- !is.null(candidate$covariates) && nrow(candidate$covariates) > 0
        hasRef <- !is.null(candidate$covRef) && nrow(candidate$covRef) > 0

        if (isTRUE(hasBinary) && isTRUE(hasRef)) {
          selectedTargetRowId <- targetRow
          selectedDatabaseId <- databaseIds()[dbIndex]
          selectedDatabaseName <- databaseNames()[dbIndex]
          break
        }
      }

      if (!is.na(selectedTargetRowId)) {
        break
      }
    }

    testthat::expect_true(!is.na(selectedTargetRowId))
    testthat::expect_true(!is.na(selectedDatabaseId))
    testthat::expect_true(!is.na(selectedDatabaseName))

    # Set selected row/db that are known to have data.
    reactiveCharacterizationTargetRowId(selectedTargetRowId)
    session$flushReact()
    if (nrow(reactiveTargetRow()) == 0) {
      reactiveCharacterizationTargetRowId(selectedTargetRowId)
      session$flushReact()
    }

    testthat::expect_true(length(databaseIds()) > 0)
    testthat::expect_true(length(databaseNames()) > 0)
    
    # set inputs
    session$setInputs(
      databaseNames = selectedDatabaseName,
      minThreshold = 0.02
    )

    session$setInputs(
      generate = TRUE
    )

    session$flushReact()
    testthat::expect_true(!is.null(plotResult()))
    testthat::expect_true(nrow(plotResult()$covariates) > 0)
    testthat::expect_true(nrow(plotResult()$covRef) > 0)
    
    resultTable <- characterizatonGetCohortData(
      connectionHandler = connectionHandler,
      resultDatabaseSettings = resultDatabaseSettings,
      characterizationTargetIds = reactiveTargetRow()$characterizationTargetId,
      databaseIds = selectedDatabaseId,
      minThreshold = 0.02
    )
    testthat::expect_true(inherits(resultTable$covariates , 'data.frame'))
    testthat::expect_true(nrow(resultTable$covariates ) > 0)
    testthat::expect_true(inherits(resultTable$covRef , 'data.frame'))
    testthat::expect_true(nrow(resultTable$covRef ) > 0)
    
    continuousTable <- characterizatonGetCohortComparisonDataContinuous(
      connectionHandler = connectionHandler,
      resultDatabaseSettings = resultDatabaseSettings,
      characterizationTargetIds = reactiveTargetRow()$characterizationTargetId,
      databaseIds = selectedDatabaseId
    )
    testthat::expect_true(nrow(continuousTable$covariates) > 0)
    
  
  })

test_that("Test characterizationDatabaseComparison ui", {
  # Test ui
  ui <- characterizationDatabaseComparisonViewer(id = 'viewer')
  checkmate::expect_list(ui)
})

