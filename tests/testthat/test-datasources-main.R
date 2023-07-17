context("datasources-main")

shiny::testServer(datasourcesServer, args = list(
  id = "datasourcesServer",
  connectionHandler = connectionHandlerPV,
  resultDatabaseSettings = resultDatabaseSettingsPlp
), {

  testthat::expect_is(datasourcesData, 'reactive')
  testthat::expect_true(!is.null(datasourcesColList))
  
})


test_that("Test datasources ui", {
  # Test ui
  ui <- datasourcesViewer("datasources")
  checkmate::expect_list(ui)
})
