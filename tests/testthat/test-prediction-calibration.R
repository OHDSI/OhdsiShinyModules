context("prediction-calibration")

shiny::testServer(
  app = predictionCalibrationServer, 
  args = list(
    performanceId = shiny::reactiveVal(1),
    con = connectionPlp,
    inputSingleView = shiny::reactiveVal("Calibration"),
    mySchema = schemaTest,
    targetDialect = dbmsTest,
    myTableAppend = tablePrefixTest
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
