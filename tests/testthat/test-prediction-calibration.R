context("prediction-calibration")

shiny::testServer(
  app = predictionCalibrationServer, 
  args = list(
    performanceId = shiny::reactiveVal(1),
    connectionHandler = connectionHandlerPlp,
    inputSingleView = shiny::reactiveVal("Calibration"),
    mySchema = resultDatabaseSettingsPlp$schema,
    myTableAppend = resultDatabaseSettingsPlp$tablePrefix
  ), 
  expr = {
    
    
    # should have discrimination results
    expect_true(nrow(sumTable())>0)
    
    # check the view to trigger event
    inputSingleView(NULL)
    inputSingleView("Calibration")
    
    session$setInputs(show_view = list(index = 1)) 
  
    # check helpers
    session$setInputs(calHelp = T) 
    session$setInputs(demoHelp = T) 
    
  })