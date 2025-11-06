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
    connectionHandler = connectionHandlerCharacterization,
    resultDatabaseSettings = resultDatabaseSettingsCharacterization,
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
    connectionHandler = connectionHandlerCharacterization,
    resultDatabaseSettings = resultDatabaseSettingsCharacterization,
    exposuresOutcomeSetId = 2086096871,
    databaseId = '388020256',
    analysisId = 1,
    covariateId = 1001
  )
  testthat::expect_true(inherits(result, 'data.frame'))
  testthat::expect_true(nrow(result) >= 1)
  
  testthat::expect_true(result$outcomeEvents[1] >= result$outcomeEvents[length(result$outcomeEvents)])
  
})

test_that("Test estimationGetSccsModel", {
  result <- estimationGetSccsModel(
    connectionHandler = connectionHandlerCharacterization,
    resultDatabaseSettings = resultDatabaseSettingsCharacterization,
    exposuresOutcomeSetId = 2086096871,
    databaseId = '388020256',
    analysisId = 1,
    exposureId = NULL
  )
  
  testthat::expect_true(inherits(result, 'data.frame'))
  testthat::expect_true(nrow(result) >= 1)
  
  testthat::expect_true(unique(result$exposuresOutcomeSetId) ==  2086096871)
  testthat::expect_true(unique(result$databaseId) ==  388020256)
  testthat::expect_true(unique(result$analysisId) ==  1)
  
})


test_that("Test estimationGetSccsTimeTrend", {
  result <- estimationGetSccsTimeTrend(
    connectionHandler = connectionHandlerCharacterization,
    resultDatabaseSettings = resultDatabaseSettingsCharacterization,
    exposureId = 1,
    exposuresOutcomeSetId = 2086096871,
    databaseId = '388020256',
    analysisId = 1
  )
  
  testthat::expect_true(inherits(result, 'data.frame'))
  testthat::expect_true(nrow(result) >= 1)
  
})

test_that("Test estimationGetSccsTimeToEvent", {
  result <- estimationGetSccsTimeToEvent(
    connectionHandler = connectionHandlerCharacterization,
    resultDatabaseSettings = resultDatabaseSettingsCharacterization,
    exposureId = 1,
    exposuresOutcomeSetId = 2086096871,
    covariateId = 1001,
    databaseId = '388020256',
    analysisId = 1
) 
  
  testthat::expect_true(inherits(result, 'data.frame'))
  testthat::expect_true(nrow(result) >= 1)
  
  
})

test_that("Test estimationGetSccsEventDepObservation", {
  result <- estimationGetSccsEventDepObservation(
    connectionHandler = connectionHandlerCharacterization,
    resultDatabaseSettings = resultDatabaseSettingsCharacterization,
    exposuresOutcomeSetId = 2086096871,
    databaseId = '388020256',
    analysisId = 1
)
  
  testthat::expect_true(inherits(result, 'data.frame'))
  testthat::expect_true(nrow(result) >= 1)
  
})


test_that("Test estimationGetSccsAgeSpanning", {
  result <- estimationGetSccsAgeSpanning(
    connectionHandler = connectionHandlerCharacterization,
    resultDatabaseSettings = resultDatabaseSettingsCharacterization,
    exposuresOutcomeSetId = 2086096871,
    databaseId = '388020256',
    analysisId = 1
  ) 
  
  testthat::expect_true(inherits(result, 'data.frame'))
  testthat::expect_true(nrow(result) >= 1)
  
})

test_that("Test estimationGetSccsAgeSpanning", {
  result <- estimationGetSccsCalendarTimeSpanning(
    connectionHandler = connectionHandlerCharacterization,
    resultDatabaseSettings = resultDatabaseSettingsCharacterization,
    exposuresOutcomeSetId = 2086096871,
    databaseId = '388020256',
    analysisId = 1
  )
  
  testthat::expect_true(inherits(result, 'data.frame'))
  testthat::expect_true(nrow(result) >= 1)
  
  
})


test_that("Test estimationGetSccsSpline", {
  result <- estimationGetSccsSpline(
    connectionHandler = connectionHandlerCharacterization,
    resultDatabaseSettings = resultDatabaseSettingsCharacterization,
    exposuresOutcomeSetId = 2086096871,
    databaseId = '388020256',
    analysisId = 1,
    splineType = "season"
) 
  testthat::expect_true(inherits(result, 'data.frame'))
  testthat::expect_true(nrow(result) >= 1)
  
  testthat::expect_true(unique(result$splineType) == 'season')
  
  
})


test_that("Test estimationGetSccsControlEstimates", {
  result <- estimationGetSccsControlEstimates(
    connectionHandler = connectionHandlerCharacterization,
    resultDatabaseSettings = resultDatabaseSettingsCharacterization,
    databaseId = '388020256',
    analysisId = 1,
    covariateId = 1001,
    eraId = 3,
    covariateAnalysisId = 1001
) 
  
  testthat::expect_true(inherits(result, 'data.frame'))
  # null so no result
  testthat::expect_true(nrow(result) == 0)
  testthat::expect_true('ease' %in% colnames(result))
  
})
