context("estimation-sccs-results-full")

test_that("Test sccs results full ui", {
  # Test ui
  ui <- estimationSccsFullResultViewer(id = 'test_sccs')
  checkmate::expect_list(ui)
})

sccsData <- function()
{
  return(data.frame(
    outcome = 'made up',
    databaseName = 1,
    covariateName = 1,
    indication = 1,
    description = 'fgfgf',
    unblind = 1,
    outcomeEvents = 50,
    outcomeSubjects = 200,
    observedDays = 1000,
    covariateSubjects = 40,
    covariateDays = 300,
    covariateOutcomes = 10,
    mdrr = 1.2,
    calibratedRr = 1.2,
    calibratedCi95Lb = 1.1,
    calibratedCi95Ub = 1.8
  ))
}


shiny::testServer(
  estimationSccsFullResultServer, 
  args = list(
    id = "estimationSccsFullResultServer",
    connectionHandler = connectionHandlerEstimation,
    resultDatabaseSettings = resultDatabaseSettingsEstimation,
    selectedRow = sccsData,
    actionCount = shiny::reactive(NULL)
  ), {
    
    # check when actionCount() changes the power tab is selected
    
    # check powerTable() is data.frame
    testthat::expect_true(inherits(powerTable(), 'data.frame'))
    testthat::expect_true(nrow(powerTable()) > 0 )
    
    # check colDefsInput is list
    testthat::expect_true(inherits(colDefsInput, 'list'))
    
    # check all the extraction tables
  
 
    
    
})

test_that("Test estimationGetSccsAttrition", {

  result <- estimationGetSccsAttrition(
    connectionHandler = connectionHandlerEstimation,
    resultDatabaseSettings = resultDatabaseSettingsEstimation,
    exposuresOutcomeSetId = -140625231,
    databaseId = 'eunomia',
    analysisId = 1,
    covariateId = 1000
  )
  testthat::expect_true(inherits(result, 'data.frame'))
  testthat::expect_true(nrow(result) >= 1)
  
  testthat::expect_true(result$outcomeEvents[1] >= result$outcomeEvents[length(result$outcomeEvents)])
  
})

test_that("Test estimationGetSccsModel", {
  result <- estimationGetSccsModel(
    connectionHandler = connectionHandlerEstimation,
    resultDatabaseSettings = resultDatabaseSettingsEstimation,
    exposuresOutcomeSetId = -140625231,
    databaseId = 'eunomia',
    analysisId = 1,
    exposureId = 1
  )
  
  testthat::expect_true(inherits(result, 'data.frame'))
  testthat::expect_true(nrow(result) >= 1)
  
  testthat::expect_true('Age spline component 1' %in% result$covariateName)
  testthat::expect_true('Seasonality spline component 1' %in% result$covariateName)
  testthat::expect_true('Calendar time spline component 1' %in% result$covariateName)
  testthat::expect_true('Exposure of interest : Exposure cohort 1' %in% result$covariateName)
  
  
})


test_that("Test estimationGetSccsTimeTrend", {
  result <- estimationGetSccsTimeTrend(
    connectionHandler = connectionHandlerEstimation,
    resultDatabaseSettings = resultDatabaseSettingsEstimation,
    exposureId = 1,
    exposuresOutcomeSetId = -140625231,
    databaseId = 'eunomia',
    analysisId = 1
  )
  
  testthat::expect_true(inherits(result, 'data.frame'))
  testthat::expect_true(nrow(result) >= 1)
  
})

test_that("Test estimationGetSccsTimeToEvent", {
  result <- estimationGetSccsTimeToEvent(
    connectionHandler = connectionHandlerEstimation,
    resultDatabaseSettings = resultDatabaseSettingsEstimation,
    exposureId = 1,
    exposuresOutcomeSetId = -140625231,
    covariateId = 1000,
    databaseId = 'eunomia',
    analysisId = 1
) 
  
  testthat::expect_true(inherits(result, 'data.frame'))
  testthat::expect_true(nrow(result) >= 1)
  
  
})

test_that("Test estimationGetSccsEventDepObservation", {
  result <- estimationGetSccsEventDepObservation(
    connectionHandler = connectionHandlerEstimation,
    resultDatabaseSettings = resultDatabaseSettingsEstimation,
    exposuresOutcomeSetId = -140625231,
    databaseId = 'eunomia',
    analysisId = 1
)
  
  testthat::expect_true(inherits(result, 'data.frame'))
  testthat::expect_true(nrow(result) >= 1)
  
})


test_that("Test estimationGetSccsAgeSpanning", {
  result <- estimationGetSccsAgeSpanning(
    connectionHandler = connectionHandlerEstimation,
    resultDatabaseSettings = resultDatabaseSettingsEstimation,
    exposuresOutcomeSetId = -140625231,
    databaseId = 'eunomia',
    analysisId = 1
  ) 
  
  testthat::expect_true(inherits(result, 'data.frame'))
  testthat::expect_true(nrow(result) >= 1)
  
})

test_that("Test estimationGetSccsAgeSpanning", {
  result <- estimationGetSccsCalendarTimeSpanning(
    connectionHandler = connectionHandlerEstimation,
    resultDatabaseSettings = resultDatabaseSettingsEstimation,
    exposuresOutcomeSetId = -140625231,
    databaseId = 'eunomia',
    analysisId = 1
  )
  
  testthat::expect_true(inherits(result, 'data.frame'))
  testthat::expect_true(nrow(result) >= 1)
  
  
})


test_that("Test estimationGetSccsSpline", {
  result <- estimationGetSccsSpline(
    connectionHandler = connectionHandlerEstimation,
    resultDatabaseSettings = resultDatabaseSettingsEstimation,
    exposuresOutcomeSetId = -140625231,
    databaseId = 'eunomia',
    analysisId = 1,
    splineType = "age"
) 
  testthat::expect_true(inherits(result, 'data.frame'))
  testthat::expect_true(nrow(result) >= 1)
  
  
})


test_that("Test estimationGetSccsControlEstimates", {
  result <- estimationGetSccsControlEstimates(
    connectionHandler = connectionHandlerEstimation,
    resultDatabaseSettings = resultDatabaseSettingsEstimation,
    databaseId = 'eunomia',
    analysisId = 1,
    covariateId = 1000,
    eraId = 3
) 
  
  testthat::expect_true(inherits(result$plotResult, 'data.frame'))
  # null so no result
  testthat::expect_true(nrow(result$plotResult) == 0)
  
  
})
