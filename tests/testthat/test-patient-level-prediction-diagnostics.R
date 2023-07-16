context("patient-level-prediction-diagnostics")

shiny::testServer(
  app = patientLevelPredictionDiagnosticsServer, 
  args = list(
    modelDesignId = shiny::reactiveVal(1),
    connectionHandler = connectionHandlerPlp,
    resultDatabaseSettings = resultDatabaseSettingsPlp
  ), 
  expr = {
    
    diag <- getPredictionDiagnostics(
      modelDesignId = modelDesignId(),
      connectionHandler = connectionHandler,
      resultDatabaseSettings = resultDatabaseSettings
    )
    
    expect_true(nrow(diag) >0 )
    
    # check plotting buttons dont break
    session$setInputs(show_predictors = list(index=1))
    session$setInputs(show_participants = list(index=1)) 
    session$setInputs(show_outcomes = list(index=1)) 
    
  })
