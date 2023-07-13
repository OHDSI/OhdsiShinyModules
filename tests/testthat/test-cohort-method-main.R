context("cohort-method-main")

shiny::testServer(
  app = cohortMethodServer, 
  args = list(
    connectionHandler = connectionHandlerCm,
    resultDatabaseSettings = resultDatabaseSettingsCm
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


test_that("Test estimation ui", {
  # Test ui
  ui <- cohortMethodViewer('test')
  checkmate::expect_list(ui)
})