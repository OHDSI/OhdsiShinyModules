context("characterization-riskFactors")

# move this to setup as will be used by all modules
targetCohort <- OhdsiShinyModules:::getTargetsUsedInCharMain(
  connectionHandler = connectionHandlerCharacterization,
  schema = resultDatabaseSettingsCharacterization$schema,
  cTablePrefix = resultDatabaseSettingsCharacterization$cTablePrefix,
  cgTablePrefix = resultDatabaseSettingsCharacterization$cgTablePrefix,
  ciTablePrefix = resultDatabaseSettingsCharacterization$incidenceTablePrefix
)

targets4module <- targetCohort %>% dplyr::filter(as.integer(.data$riskFactors) == 1)

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
  dplyr::filter(as.integer(.data$riskFactors) == 1)


shiny::testServer(
  app = characterizationRiskFactorServer, 
  args = list(
    connectionHandler = connectionHandlerCharacterization ,
    resultDatabaseSettings = resultDatabaseSettingsCharacterization,
    reactiveCharacterizationTargetTable = shiny::reactive(characterizationTargetTable)
    ), 
  expr = {

    testthat::expect_true(nrow(reactiveCharacterizationTargetTable()) > 0)

    # target pop row is initially NULL
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

    # now reactiveOutcomesUsed should be set but the row should be NULL
    testthat::expect_true(nrow(reactiveOutcomesUsed()) > 0)
    testthat::expect_true(is.null(reactiveOutcomeCaseRowId()))

    # Find an outcome/database pair that actually has risk factor rows.
    selectedOutcomeRowId <- NA_integer_
    selectedDatabaseId <- NA_character_
    selectedDatabaseName <- NA_character_

    for (outcomeRow in seq_len(nrow(reactiveOutcomesUsed()))) {
      for (dbIndex in seq_along(databaseIds())) {
        candidate <- characterizationGetRiskFactorData(
          connectionHandler = connectionHandler,
          resultDatabaseSettings = resultDatabaseSettings,
          characterizationCaseId = reactiveOutcomesUsed()$characterizationCaseId[outcomeRow],
          databaseId = databaseIds()[dbIndex]
        )

        hasBinary <- !is.null(candidate$binary) && nrow(candidate$binary) > 0
        hasContinuous <- !is.null(candidate$continuous) && nrow(candidate$continuous) > 0

        if (isTRUE(hasBinary) && isTRUE(hasContinuous)) {
          selectedOutcomeRowId <- outcomeRow
          selectedDatabaseId <- databaseIds()[dbIndex]
          selectedDatabaseName <- databaseNames()[dbIndex]
          break
        }
      }
      if (!is.na(selectedOutcomeRowId)) {
        break
      }
    }

    testthat::expect_true(!is.na(selectedOutcomeRowId))
    testthat::expect_true(!is.na(selectedDatabaseId))
    testthat::expect_true(!is.na(selectedDatabaseName))

    # now set the outcome using a row that has data
    reactiveOutcomeCaseRowId(selectedOutcomeRowId)
    session$flushReact()

    # Outcome selection can similarly reset after outcomes table first updates.
    if (nrow(reactiveSelectedOutcomeCaseRow()) == 0) {
      reactiveOutcomeCaseRowId(selectedOutcomeRowId)
      session$flushReact()
    }

    testthat::expect_true(nrow(reactiveSelectedOutcomeCaseRow()) == 1)

    # check database
    testthat::expect_true(length(databaseNames()) > 0 )
    testthat::expect_true(length(databaseIds()) > 0 )
    
    #Test characterizationGetRiskFactorData
    data <- characterizationGetRiskFactorData(
      connectionHandler = connectionHandlerCharacterization ,
      resultDatabaseSettings = resultDatabaseSettingsCharacterization,
      characterizationCaseId = reactiveSelectedOutcomeCaseRow()$characterizationCaseId[1],
      databaseId = selectedDatabaseId
    )
    
    testthat::expect_true(inherits(data, 'list'))
    testthat::expect_true(nrow(data$binary) > 0 )
    testthat::expect_true(nrow(data$continuous) > 0 )
    
    session$setInputs(databaseName = selectedDatabaseName)
    session$setInputs(generate = 1)
    session$flushReact()

    # generate should run and keep selected row stable
    testthat::expect_true(reactiveOutcomeCaseRowId() == selectedOutcomeRowId)
    
    #testthat::expect_true(inherits(allData, 'list'))
    #testthat::expect_true( nrow(allData$binary) > 0 )
    #testthat::expect_true( nrow( allData$continuous) > 0 )
    

    
  })


test_that("Test characterizationRiskFactorViewer ui", {
  # Test ui
  ui <- characterizationRiskFactorViewer(id = 'viewer')
  checkmate::expect_list(ui)
})

test_that("Test characteriationRiskFactorColDefs", {
  colDefs <- characteriationRiskFactorColDefs()
  testthat::expect_true(inherits( colDefs, 'list'))
  testthat::expect_true(length(colDefs) > 0 )
})

test_that("Test characteriationRiskFactorContColDefs", {
  colDefs <- characteriationRiskFactorContColDefs()
  testthat::expect_true(inherits( colDefs, 'list'))
  testthat::expect_true(length(colDefs) > 0 )
})
