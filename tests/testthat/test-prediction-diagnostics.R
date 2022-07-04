context("prediction-diagnostics")

shiny::testServer(
  app = predictionDiagnosticsServer, 
  args = list(
    modelDesignId = shiny::reactiveVal(1),
    con = connection,
    mySchema = mySchemaTest,
    targetDialect = targetDialectTest,
    myTableAppend = myTableAppendTest
  ), 
  expr = {
    
    diag <- getDiagnostics(
      modelDesignId = modelDesignId(),
      mySchema = mySchemaTest, 
      con = connection,
      myTableAppend = myTableAppendTest, 
      targetDialect = targetDialectTest 
    )
    
    expect_true(nrow(diag) >0 )
    
    # check plotting buttons dont break
    session$setInputs(show_predictors = list(index=1))
    session$setInputs(show_participants = list(index=1)) 
    session$setInputs(show_outcomes = list(index=1)) 
    
  })