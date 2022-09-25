context("cohortgenerator-main")

shiny::testServer(
  app = cohortGeneratorServer, 
  args = list(
    resultDatabaseSettings = list(
      dbms = dbmsTest,
      server = "../resources/cgDatabase/databaseFile.sqlite",
      user = NULL,
      password = NULL,
      port = NULL,
      tablePrefix = cohortTablePrefix,
      cohortTablePrefix = cohortTablePrefix,
      databaseTable = databaseTable,
      schema = schemaTest,
      tempEmulationSchema = NULL
    )
  ), 
  expr = {
    
    testthat::expect_true(class(connection) == 'DatabaseConnectorDbiConnection')
    
    
    testthat::expect_true(!is.null(output$cohortCounts))
    testthat::expect_true(!is.null(output$cohortGeneration))
    testthat::expect_true(nrow(inclusionStats)>0)
    #testthat::expect_true(!is.null(output$inclusionsStats))
    
    
  })