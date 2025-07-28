context("characterization-cohorts")

targetCohort <- OhdsiReportGenerator::getTargetTable(
  connectionHandler = connectionHandlerCharacterization,
  schema = resultDatabaseSettingsCharacterization$schema, 
  ciTablePrefix = resultDatabaseSettingsCharacterization$incidenceTablePrefix
)

shiny::testServer(
  app = characterizationCohortComparisonServer, 
  args = list(
    connectionHandler = connectionHandlerCharacterization,
    resultDatabaseSettings = resultDatabaseSettingsCharacterization,
    targetTable = targetCohort,
    reactiveTargetRow = shiny::reactive(targetCohort[1,])
    ), 
  expr = {
    
    testthat::expect_true(length(databaseNames()) > 0)
    testthat::expect_true(length(databaseIds()) > 0)
    
    # set inputs
    session$setInputs(
      databaseName = databaseNames()[1]
    )
    
    # set reactiveComparatorRow() to second row 
    reactiveComparatorRowId(2)
    
    # test generate
    session$setInputs(
      generate = T
    )
    
    testthat::expect_true(inherits(selected(),'data.frame'))
    
    resultTable <- characterizatonGetCohortData(
      connectionHandler = connectionHandler,
      resultDatabaseSettings = resultDatabaseSettings,
      targetIds = c(1,2),
      databaseIds = databaseIds()[1],
      minThreshold = 0.01,
      addSMD = T
    )
    testthat::expect_true(nrow(resultTable) > 0)
    
    countTable <- characterizatonGetCohortCounts(
      connectionHandler = connectionHandler,
      resultDatabaseSettings = resultDatabaseSettings,
      targetIds = c(1,2),
      databaseIds = databaseIds()[1]
    )
    testthat::expect_true(nrow(countTable) > 0)
    
    
    continuousTable <- characterizatonGetCohortComparisonDataContinuous(
      connectionHandler = connectionHandler,
      resultDatabaseSettings = resultDatabaseSettings,
      targetIds = c(1,2),
      databaseIds = databaseIds()[1]
    )
    testthat::expect_true(nrow(continuousTable) > 0)
    
  
  })

test_that("Test characterizationTable ui", {
  # Test ui
  ui <- characterizationCohortComparisonViewer(id = 'viewer')
  checkmate::expect_list(ui)
})


test_that("Test characterizationCohortsColumns", {
  cols <- characterizationCohortsColumns()
  testthat::expect_true(inherits(cols, 'list'))
})


test_that("Test characterizationCohortsColumnsContinuous", {
  cols <- characterizationCohortsColumnsContinuous()
  testthat::expect_true(inherits(cols, 'list'))
})

