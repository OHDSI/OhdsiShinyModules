context("characterization-database")

targetCohort <- OhdsiReportGenerator::getTargetTable(
  connectionHandler = connectionHandlerCharacterization,
  schema = resultDatabaseSettingsCharacterization$schema
)

shiny::testServer(
  app = characterizationDatabaseComparisonServer, 
  args = list(
    connectionHandler = connectionHandlerCharacterization,
    resultDatabaseSettings = resultDatabaseSettingsCharacterization,
    reactiveTargetRow = shiny::reactive(
      targetCohort[1,]
    )
    ), 
  expr = {
    
    testthat::expect_true(length(databaseIds()) > 0)
    testthat::expect_true(length(databaseNames()) > 0)
    
    # set inputs
    session$setInputs(
      databaseNames = databaseNames()[1],
      minThreshold = 0.02
    )

    session$setInputs(
      generate = T
    )
    
    testthat::expect_true(inherits(selected(),'data.frame'))
    
    resultTable <- characterizatonGetDatabaseComparisonData(
      connectionHandler = connectionHandler,
      resultDatabaseSettings = resultDatabaseSettings,
      targetIds = c(1),
      databaseIds = databaseIds()[1],
      minThreshold = 0.02
    )
    testthat::expect_true(inherits(resultTable$table , 'data.frame'))
    testthat::expect_true(nrow(resultTable$table ) > 0)
    testthat::expect_true(inherits(resultTable$databaseNames , 'data.frame'))
    testthat::expect_true(nrow(resultTable$databaseNames ) > 0)
    
    countTable <- characterizatonGetCohortCounts(
      connectionHandler = connectionHandler,
      resultDatabaseSettings = resultDatabaseSettings,
      targetIds = c(1),
      databaseIds = databaseIds()[1]
    )
    testthat::expect_true(nrow(countTable) > 0)
    
    
    continuousTable <- characterizatonGetCohortComparisonDataContinuous(
      connectionHandler = connectionHandler,
      resultDatabaseSettings = resultDatabaseSettings,
      targetIds = c(1),
      databaseIds = databaseIds()[1],
      pivot = F
    )
    testthat::expect_true(nrow(continuousTable) > 0)
    
  
  })

test_that("Test characterizationDatabaseComparison ui", {
  # Test ui
  ui <- characterizationDatabaseComparisonViewer(id = 'viewer')
  checkmate::expect_list(ui)
})


test_that("Test getMinCovaraiteThreshold", {

  minVal <- getMinCovaraiteThreshold(
    connectionHandler = connectionHandlerCharacterization,
    resultDatabaseSettings = resultDatabaseSettingsCharacterization
  )
  
  testthat::expect_true(minVal >= 0)
  
})

