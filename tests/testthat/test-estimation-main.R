context("estimation-main")

shiny::testServer(
  app = estimationServer, 
  args = list(
    resultDatabaseSettings = list(
      dbms = dbmsTest,
      server = "../resources/estDatabase/databaseFile.sqlite",
      user = NULL,
      password = NULL,
      port = NULL,
      tablePrefix = estTablePrefix,
      cohortTablePrefix = cohortTablePrefix,
      databaseTable = databaseTable,
      schema = schemaTest,
      tempEmulationSchema = NULL
    )
  ), 
  expr = {
    
 testthat::expect_true(class(connection) == 'DatabaseConnectorDbiConnection')
    
    
    testthat::expect_true(!is.null(output$targetWidget))
    testthat::expect_true(!is.null(output$comparatorWidget))
    testthat::expect_true(!is.null(output$outcomeWidget))
    testthat::expect_true(!is.null(output$databaseWidget))
    testthat::expect_true(!is.null(output$analysisWidget))
    
    # check setting target updates inputParams
    session$setInputs(target = '1')
    testthat::expect_true(inputParams()$target == '1')

    
  })