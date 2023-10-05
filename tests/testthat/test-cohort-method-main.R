context("cohort-method-main")

shiny::testServer(
  app = cohortMethodServer, 
  args = list(
    connectionHandler = connectionHandlerCm,
    resultDatabaseSettings = resultDatabaseSettingsCm
  ), 
  expr = {
    
    testthat::expect_true(inherits(connectionHandler,"ConnectionHandler"))
    
    testthat::expect_true(!is.null(targetIds))
    
  })


test_that("Test estimation ui", {
  # Test ui
  ui <- cohortMethodViewer('test')
  checkmate::expect_list(ui)
})
