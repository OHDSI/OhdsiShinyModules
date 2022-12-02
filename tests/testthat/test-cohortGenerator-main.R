context("cohortgenerator-main")

shiny::testServer(
  app = cohortGeneratorServer, 
  args = list(
    connectionHandler = connectionHandlerCG,
    resultDatabaseSettings = list(
      dbms = 'sqlite',
      tablePrefix = 'cg_',
      cohortTablePrefix = 'cg_',
      databaseTable = 'DATABASE_META_DATA',
      schema = 'main',
      tempEmulationSchema = NULL
    )
  ), 
  expr = {
    
    testthat::expect_true(inherits(connectionHandler,"ConnectionHandler"))
    
    
    testthat::expect_true(!is.null(output$cohortCounts))
    testthat::expect_true(!is.null(output$cohortGeneration))
    testthat::expect_true(nrow(inclusionStats)>0)
    #testthat::expect_true(!is.null(output$inclusionsStats))
    
    
  })