context("patient-level-prediction-main")

shiny::testServer(
  app = patientLevelPredictionServer, 
  args = list(
    connectionHandler = connectionHandlerCharacterization,
    resultDatabaseSettings = resultDatabaseSettingsCharacterization
  ), 
  expr = {
    
    expect_true(performanceRowId() == 0)
    testthat::expect_is(performances, 'data.frame')
    expect_true(nrow(performances) > 0 )
    
    
    # set the performanceRowId()
    performanceRowId(1)
    
    # check each view loads selects tab
    session$setInputs(tabView = 'View Models')
    session$flushReact()
    
    # add check to see whether tab changed
    #testthat::expect_true(input$resultTab == 'View Models')
  
    session$setInputs(tabView = 'Generate Plots')
    #testthat::expect_true(input$resultTab == 'Generate Plots')
    
    session$setInputs(tabView = 'View Threshold Performances')
    #testthat::expect_true(input$resultTab == 'View Threshold Performances')
    
    session$setInputs(tabView = 'View Diagnostics')
    #testthat::expect_true(input$resultTab == 'View Diagnostics')
    
    
  })



test_that("Test prediction ui", {
  # Test ui
  ui <- patientLevelPredictionViewer()
  checkmate::expect_list(ui)
})



