context("sccs-main")

shiny::testServer(sccsServer, args = list(
  id = "testSccsServer",
  connectionHandler = connectionHandlerSccs,
  resultDatabaseSettings = resultDatabaseSettingsSccs
), {
  
  inputSelected(
    list(
      analysis = 13,
      outcome = 11123,
      exposure = 1
    )
  )

})

test_that("Test cd ui", {
  # Test ui
  ui <- sccsView()
  checkmate::expect_list(ui)
  checkmate::expect_file_exists(sccsHelperFile())
})


test_that("Test sccsGetOutcomes", {
result <- sccsGetOutcomes(
  connectionHandler  = connectionHandlerSccs, 
  resultDatabaseSettings = resultDatabaseSettingsSccs
)

testthat::expect_true(result == 11123)
testthat::expect_true(names(result) == 'outcome test')

})

test_that("Test sccsGetExposures", {
  result <- sccsGetExposures(
    connectionHandler  = connectionHandlerSccs, 
    resultDatabaseSettings = resultDatabaseSettingsSccs
  )
  
  testthat::expect_true(result == 1)
  testthat::expect_true(names(result) == 'target test')
  
})

test_that("Test sccsGetDatabases", {
  result <- sccsGetDatabases(
    connectionHandler  = connectionHandlerSccs, 
    resultDatabaseSettings = resultDatabaseSettingsSccs
  )
  
  testthat::expect_true(result == 1)
  testthat::expect_true(names(result) == 'madeup')
  
})

test_that("Test sccsGetAnalyses", {
  result <- sccsGetAnalyses(
    connectionHandler  = connectionHandlerSccs, 
    resultDatabaseSettings = resultDatabaseSettingsSccs
  )
  
  testthat::expect_true(result == 13)
  testthat::expect_true(names(result) == 'SCCS, start:20210101time-at-risk 1-42')
  
})


test_that("Test getSccsResults", {
  
  result <- getSccsResults(
    connectionHandler  = connectionHandlerSccs, 
    resultDatabaseSettings = resultDatabaseSettingsSccs,
    exposureIds = 1,
    outcomeIds = 11123,
    analysisIds = 13
  )
  
  testthat::expect_true(nrow(result) >0)
  
})

test_that("Test getSccsModel", {
  
  result <- getSccsModel(
    connectionHandler  = connectionHandlerSccs, 
    resultDatabaseSettings = resultDatabaseSettingsSccs,
    exposureId = 1,
    outcomeId = 11123,
    databaseId = 1,
    analysisId = 13
  )
  
  testthat::expect_true(nrow(result) >0)
  
})

test_that("Test getSccsTimeTrend", {
  
  result <- getSccsTimeTrend(
    connectionHandler  = connectionHandlerSccs, 
    resultDatabaseSettings = resultDatabaseSettingsSccs,
    exposureId = 1,
    outcomeId = 11123,
    databaseId = 1,
    analysisId = 13
  )
  
  testthat::expect_true(nrow(result) >0)
  
})


test_that("Test getSccsTimeToEvent", {
  
  result <- getSccsTimeToEvent(
    connectionHandler  = connectionHandlerSccs, 
    resultDatabaseSettings = resultDatabaseSettingsSccs,
    exposureId = 1,
    outcomeId = 11123,
    covariateId = 1001,
    databaseId = 1,
    analysisId = 13
  )
  
  testthat::expect_true(nrow(result) >0)
  
})

test_that("Test getSccsAttrition ", {
  
  result <- getSccsAttrition(
    connectionHandler  = connectionHandlerSccs, 
    resultDatabaseSettings = resultDatabaseSettingsSccs,
    outcomeId = 11123,
    covariateId = 1001,
    databaseId = 1,
    analysisId = 13
  )
  
  testthat::expect_true(nrow(result) >0)
  
})

test_that("Test getSccsEventDepObservation ", {
  
  result <- getSccsEventDepObservation(
    connectionHandler  = connectionHandlerSccs, 
    resultDatabaseSettings = resultDatabaseSettingsSccs,
    outcomeId = 11123,
    databaseId = 1,
    analysisId = 13
  )
  
  testthat::expect_true(nrow(result) >0)
  
})


test_that("Test getSccsAgeSpanning", {
  
  result <- getSccsAgeSpanning(
    connectionHandler  = connectionHandlerSccs, 
    resultDatabaseSettings = resultDatabaseSettingsSccs,
    outcomeId = 11123,
    databaseId = 1,
    analysisId = 13
  )
  
  testthat::expect_true(nrow(result) >0)
  
})

test_that("Test getSccsCalendarTimeSpanning", {
  
  result <- getSccsCalendarTimeSpanning(
    connectionHandler  = connectionHandlerSccs, 
    resultDatabaseSettings = resultDatabaseSettingsSccs,
    outcomeId = 11123,
    databaseId = 1,
    analysisId = 13
  )
  
  testthat::expect_true(nrow(result) >0)
  
})


test_that("Test getSccsSpline", {
  
  result <- getSccsSpline(
    connectionHandler  = connectionHandlerSccs, 
    resultDatabaseSettings = resultDatabaseSettingsSccs,
    outcomeId = 11123,
    databaseId = 1,
    analysisId = 13, 
    splineType = 'calendar time'
  )
  
  testthat::expect_true(nrow(result) >0)
  
})


test_that("Test getSccsControlEstimates", {
  
  result <- getSccsControlEstimates(
    connectionHandler  = connectionHandlerSccs, 
    resultDatabaseSettings = resultDatabaseSettingsSccs,
    databaseId = 1,
    analysisId = 13, 
    covariateId = 1001 # could have era_id and outcome_id instead?
  )
  
  testthat::expect_true(nrow(result) >0)
  
})

test_that("Test getSccsDiagnosticsSummary", {
  
  result <- getSccsDiagnosticsSummary(
    connectionHandler  = connectionHandlerSccs, 
    resultDatabaseSettings = resultDatabaseSettingsSccs,
    outcomeId = 11123,
    databaseId = 1,
    exposureId = 1,
    analysisId = 13, 
    covariateId = 1001 # could have era_id and outcome_id instead?
  )
  
  testthat::expect_true(nrow(result) >0)
  
})



