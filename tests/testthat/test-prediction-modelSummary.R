context("prediction-modelSummary")

shiny::testServer(
  app = predictionModelSummaryServer, 
  args = list(
    con = connectionPlp,
    mySchema = schemaTest,
    targetDialect = dbmsTest,
    myTableAppend = tablePrefixTest,
    modelDesignId = shiny::reactiveVal(1),
    databaseTableAppend = ''
  ), 
  expr = {
    
    expect_true(nrow(resultTable())>0)
    # check reactives are null untill input set
    expect_true(is.null(performanceId()))
    expect_true(is.null(developmentDatabaseId()))
    
    session$setInputs(view_details = list(index = 1))
    expect_true(!is.null(performanceId()))
    expect_true(!is.null(developmentDatabaseId()))
    
  })