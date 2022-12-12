context("prediction-main")

shiny::testServer(
  app = predictionServer, 
  args = list(
    resultDatabaseSettings = list(
      dbms = dbmsTest,
      server = serverPlp,
      user = NULL,
      password = NULL,
      port = NULL,
      tablePrefix = tablePrefixTest,
      cohortTablePrefix = tablePrefixTest,
      databaseTablePrefix = tablePrefixTest,
      schema = schemaTest
    )
  ), 
  expr = {
    
    expect_true(is.null(modelDesignId()))
    designSummary$modelDesignId(1)
    ##expect_true(!is.null(modelDesignId()))
    
    performance$performanceId(1)
    # check performanceId() and developmentDatabaseId()
    ##expect_true(!is.null(performanceId()))
    ##expect_true(!is.null(developmentDatabaseId()))
    
    designSummary$reportId(NULL)
    designSummary$reportId(1)
    ##expect_true(file.exists(file.path(tempdir(), 'main.html')))
    
    designSummary$diagnosticId(1)
    
    session$setInputs(allView = 'Model Designs Summary')
    
    
  })
