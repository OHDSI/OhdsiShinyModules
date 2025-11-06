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
      generate = TRUE
    )
    
    testthat::expect_true(inherits(selected(),'data.frame'))
    
    resultTable <- characterizatonGetCohortData(
      connectionHandler = connectionHandler,
      resultDatabaseSettings = resultDatabaseSettings,
      targetIds = targetCohort$cohortId[1],
      databaseIds = databaseIds()[1],
      minThreshold = 0.02
    )
    testthat::expect_true(inherits(resultTable$covariates , 'data.frame'))
    testthat::expect_true(nrow(resultTable$covariates ) > 0)
    testthat::expect_true(inherits(resultTable$covRef , 'data.frame'))
    testthat::expect_true(nrow(resultTable$covRef ) > 0)
    
    continuousTable <- characterizatonGetCohortComparisonDataContinuous(
      connectionHandler = connectionHandler,
      resultDatabaseSettings = resultDatabaseSettings,
      targetIds = targetCohort$cohortId[1],
      databaseIds = databaseIds()[1]
    )
    testthat::expect_true(nrow(continuousTable$covariates) > 0)
    
  
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

