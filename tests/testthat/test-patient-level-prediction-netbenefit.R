context("patient-level-prediction-netbenefit")

shiny::testServer(
  app = patientLevelPredictionNbServer, 
  args = list(
    performanceId = shiny::reactiveVal(NULL),
    connectionHandler = connectionHandlerPlp,
    inputSingleView = shiny::reactiveVal("Discrimination"),
    resultDatabaseSettings = resultDatabaseSettingsPlp
  ), 
  expr = {
    
    expect_true(is.null(thresholdSummary()))
    
    performanceId(1)
    inputSingleView('Net Benefit')
    expect_true(!is.null(thresholdSummary()))
    
    # check no errors
    session$setInputs(nbSelectInput = 'CV')
    session$setInputs(nbSelectInput = 'Test')
    session$setInputs(nbSelectInput = 'Train')

  })
