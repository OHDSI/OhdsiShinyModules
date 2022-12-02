context("prediction-diagnostics")

shiny::testServer(
  app = predictionDiagnosticsServer, 
  args = list(
    modelDesignId = shiny::reactiveVal(1),
    connectionHandler = connectionHandlerPlp,
    mySchema = resultDatabaseSettingsPlp$schema,
    myTableAppend = resultDatabaseSettingsPlp$tablePrefix,
    databaseTableAppend = resultDatabaseSettingsPlp$tablePrefix
  ), 
  expr = {
    
    diag <- getDiagnostics(
      modelDesignId = modelDesignId(),
      mySchema = mySchema, 
      connectionHandler = connectionHandler,
      myTableAppend = myTableAppend, 
      databaseTableAppend = databaseTableAppend
    )
    
    expect_true(nrow(diag) >0 )
    
    # check plotting buttons dont break
    session$setInputs(show_predictors = list(index=1))
    session$setInputs(show_participants = list(index=1)) 
    session$setInputs(show_outcomes = list(index=1)) 
    
  })