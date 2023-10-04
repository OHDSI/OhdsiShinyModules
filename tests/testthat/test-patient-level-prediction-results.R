context("patient-level-prediction-results")

shiny::testServer(
  app = patientLevelPredictionResultsServer, 
  args = list(
    connectionHandler = connectionHandlerPlp,
    resultDatabaseSettings = resultDatabaseSettingsPlp,
    modelDesignId = shiny::reactiveVal(NULL), 
    developmentDatabaseId = shiny::reactiveVal(1),
    performanceId = shiny::reactiveVal(1),
    performance = list(
      performanceId = shiny::reactiveVal(1),
      modelDevelopment = shiny::reactiveVal(1)
      ),
    tracker = shiny::reactive(1)
  ), 
  expr = {
    
    expect_true(is.null(modelDesignId()))
 
    # get performance details
    results <- getPlpPerformanceSelection(
    connectionHandler = connectionHandler, 
    resultDatabaseSettings = resultDatabaseSettings,
    performanceId = performanceId
    )
    
    testthat::expect_true(
      sum(
        colnames(results) %in% c(
        "developmentDb",
        "validationDb",
        "validationTarget",
        "validationOutcome",
        "validationTar"
      )
      ) == 5
      )
    
    # makes sure no errors when setting modelDesignId
    modelDesignId(1)
    
  })



test_that("Test prediction resulta ui", {
  # Test ui
  ui <- patientLevelPredictionResultsViewer()
  checkmate::expect_list(ui)
})



