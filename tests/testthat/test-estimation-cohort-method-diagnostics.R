context("estimation-cohort-method-diagnostics")

shiny::testServer(
  app = estimationCmDiagnosticServer, 
  args = list(
    connectionHandler = connectionHandlerEstimation, 
    resultDatabaseSettings = resultDatabaseSettingsEstimation,
    targetIds = shiny::reactiveVal(1),
    outcomeId = shiny::reactiveVal(3)
  ), 
  expr = {
    
    # should start null
    testthat::expect_true(!is.null(cmDiagnostics()))
    
  })

# estimationGetCmDiagnosticColDefs
test_that("estimationGetCmDiagnosticColDefs", {
colDefs <- estimationGetCmDiagnosticColDefs()
testthat::expect_is(colDefs, 'list')
})


