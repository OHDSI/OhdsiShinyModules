context("prediction-modelSummary")

shiny::testServer(
  app = predictionModelSummaryServer, 
  args = list(
    connectionHandler = connectionHandlerPlp,
    modelDesignId = shiny::reactiveVal(1),
    schema = resultDatabaseSettingsPlp$schema,
    plpTablePrefix = resultDatabaseSettingsPlp$plpTablePrefix,
    databaseTablePrefix = resultDatabaseSettingsPlp$plpTablePrefix
  ), 
  expr = {
    
    expect_true(nrow(resultTable())>0)
    # check reactives are null untill input set
    expect_true(is.null(performanceId()))
    expect_true(is.null(developmentDatabaseId()))
    
    #session$setInputs(view_details = list(index = 1))
    #expect_true(!is.null(performanceId()))
    #expect_true(!is.null(developmentDatabaseId()))
    
  })
