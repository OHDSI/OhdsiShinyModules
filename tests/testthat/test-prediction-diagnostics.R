context("prediction-diagnostics")

shiny::testServer(
  app = predictionDiagnosticsServer, 
  args = list(
    modelDesignId = shiny::reactiveVal(1),
    con = connectionPlp,
    mySchema = schemaTest,
    targetDialect = dbmsTest,
    myTableAppend = tablePrefixTest, 
    databaseTableAppend = ''
  ), 
  expr = {
    
    diag <- getDiagnostics(
      modelDesignId = modelDesignId(),
      mySchema = schemaTest, 
      con = connectionPlp,
      myTableAppend = tablePrefixTest, 
      targetDialect = dbmsTest, 
      databaseTableAppend = databaseTableAppend
    )
    
    expect_true(nrow(diag) >0 )
    
    # check plotting buttons dont break
    session$setInputs(show_predictors = list(index=1))
    session$setInputs(show_participants = list(index=1)) 
    session$setInputs(show_outcomes = list(index=1)) 
    
  })
