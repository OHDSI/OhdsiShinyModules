context("estimation-main")

shiny::testServer(
  app = estimationServer, 
  args = list(
    connectionHandler = connectionHandlerEst,
    resultDatabaseSettings = resultDatabaseSettingsEst
  ), 
  expr = {
    
    testthat::expect_true(inherits(connectionHandler,"ConnectionHandler"))
    
    
    testthat::expect_true(!is.null(output$targetWidget))
    testthat::expect_true(!is.null(output$comparatorWidget))
    testthat::expect_true(!is.null(output$outcomeWidget))
    testthat::expect_true(!is.null(output$databaseWidget))
    testthat::expect_true(!is.null(output$analysisWidget))
    
    # check setting target updates inputParams
    session$setInputs(target = '1')
    testthat::expect_true(inputParams()$target == '1')

    
  })