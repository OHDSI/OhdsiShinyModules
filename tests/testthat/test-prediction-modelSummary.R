context("prediction-modelSummary")

shiny::testServer(
  app = predictionModelSummaryServer, 
  args = list(
    con = connection,
    mySchema = mySchemaTest,
    targetDialect = targetDialectTest,
    myTableAppend = myTableAppendTest,
    modelDesignId = shiny::reactiveVal(1)
  ), 
  expr = {
    
    # check reactives are null untill input set
    expect_true(is.null(performanceId()))
    expect_true(is.null(developmentDatabaseId()))
    
    session$setInputs(view_details = list(index = 1))
    expect_true(!is.null(performanceId()))
    expect_true(!is.null(developmentDatabaseId()))
    
  })