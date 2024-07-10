context("estimation-sccs-diagnostics")

test_that("Test sccs diagnostic ui", {
  # Test ui
  ui <- estimationSccsDiagnosticViewer()
  checkmate::expect_list(ui)
})


shiny::testServer(estimationSccsDiagnosticServer, args = list(
  id = "testEstimationSccsDiagnosticServer",
  connectionHandler = connectionHandlerEstimation,
  resultDatabaseSettings = resultDatabaseSettingsEstimation,
  targetIds = shiny::reactiveVal(c(1)),
  outcomeId = shiny::reactiveVal(c(3))
), {
  
  testthat::expect_true(inherits(sccsDiagnostics(), 'data.frame'))
  testthat::expect_true(nrow(sccsDiagnostics()) == 2)
  
})

test_that("Test estimationGetSccsDiagnosticColDefs()", {
  result <- estimationGetSccsDiagnosticColDefs()
  testthat::expect_true(inherits(result, 'list'))
})