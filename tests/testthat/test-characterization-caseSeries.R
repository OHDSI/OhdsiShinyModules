context("characterization-caseSeries")

# move this to setup as will be used by all modules
targetCohort <- OhdsiShinyModules:::getTargetsUsedInCharMain(
    connectionHandler = connectionHandlerCharacterization,
    schema = resultDatabaseSettingsCharacterization$schema, 
    cTablePrefix = resultDatabaseSettingsCharacterization$cTablePrefix,
    cgTablePrefix = resultDatabaseSettingsCharacterization$cgTablePrefix,
    ciTablePrefix = resultDatabaseSettingsCharacterization$incidenceTablePrefix
)

targets4module <- targetCohort %>% dplyr::filter(as.integer(.data$caseSeries) == 1)

characterizationTargetTable <- OhdsiReportGenerator::getCharacterizationTargetSettings(
  connectionHandler = connectionHandlerCharacterization,
  schema = resultDatabaseSettingsCharacterization$schema,
  targetIds = targets4module$cohortDefinitionId[1],
  addDatabaseDetails = TRUE,
  databaseTable = resultDatabaseSettingsCharacterization$databaseTable,
  cgTablePrefix = resultDatabaseSettingsCharacterization$cgTablePrefix,
  cTablePrefix = resultDatabaseSettingsCharacterization$cTablePrefix
)

# skip to table with case series
characterizationTargetTable <- characterizationTargetTable %>% dplyr::filter(as.integer(.data$caseSeries) == 1)



shiny::testServer(
  app = characterizationCaseSeriesServer, 
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

    # Find an outcome/database pair that actually has case series rows.
    selectedOutcomeRowId <- NA_integer_
    selectedDatabaseId <- NA_character_
    selectedDatabaseName <- NA_character_

    for (outcomeRow in seq_len(nrow(reactiveOutcomesUsed()))) {
      for (dbIndex in seq_along(databaseIds())) {
        candidate <- characterizationGetCaseSeriesData(
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
    
    # now set the data input and generate
    session$setInputs(databaseName = selectedDatabaseName)
    session$setInputs(generate = 2)
    session$flushReact()
    
    # check reactiveCaseSeriesData() has data
    testthat::expect_true(inherits(reactiveCaseSeriesData(), 'list'))
    testthat::expect_true(nrow(reactiveCaseSeriesData()$binary) > 0 )
    testthat::expect_true(nrow(reactiveCaseSeriesData()$continuous) > 0 )
    
    testthat::expect_true(!is.null(reactiveCaseSeriesData()))
    
    
    # checking this function inside server due to progress message
    data <- characterizationGetCaseSeriesData(
      connectionHandler = connectionHandler,
      resultDatabaseSettings = resultDatabaseSettings,
      characterizationCaseId = reactiveSelectedOutcomeCaseRow()$characterizationCaseId[1],
      databaseId = selectedDatabaseId
    )
    testthat::expect_true(inherits(data, 'list'))
    testthat::expect_true(!is.null(data$binary))
    testthat::expect_true(!is.null(data$continuous))
    
  
  })


test_that("Test characterizationCaseSeriesViewer ui", {
  # Test ui
  ui <- characterizationCaseSeriesViewer(id = 'viewer')
  checkmate::expect_list(ui)
})


test_that("Test colDefsBinary", {
  colDefs <- colDefsBinary('test')
  testthat::expect_true(inherits( colDefs, 'list'))
  testthat::expect_true(length(colDefs) > 0 )
})

test_that("Test colDefsContinuous", {
  colDefs <- colDefsContinuous()
  testthat::expect_true(inherits( colDefs, 'list'))
  testthat::expect_true(length(colDefs) > 0 )
})
