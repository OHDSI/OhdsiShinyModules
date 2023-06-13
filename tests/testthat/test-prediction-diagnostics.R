context("prediction-diagnostics")

shiny::testServer(
  app = predictionDiagnosticsServer, 
  args = list(
    modelDesignId = shiny::reactiveVal(1),
    connectionHandler = connectionHandlerPlp,
    schema = resultDatabaseSettingsPlp$schema,
    plpTablePrefix = resultDatabaseSettingsPlp$plpTablePrefix,
    databaseTablePrefix = resultDatabaseSettingsPlp$plpTablePrefix
  ), 
  expr = {
    
    diag <- getDiagnostics(
      modelDesignId = modelDesignId(),
      schema = schema, 
      connectionHandler = connectionHandler,
      plpTablePrefix = plpTablePrefix, 
      databaseTablePrefix = databaseTablePrefix
    )
    
    expect_true(nrow(diag) >0 )
    
    # check plotting buttons dont break
    session$setInputs(show_predictors = list(index=1))
    session$setInputs(show_participants = list(index=1)) 
    session$setInputs(show_outcomes = list(index=1)) 
    
  })
