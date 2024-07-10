context("estimation-cohort-method-diagnostics")

shiny::testServer(
  app = estimationCmDiagnosticServer, 
  args = list(
    connectionHandler = connectionHandlerEstimation, 
    resultDatabaseSettings = resultDatabaseSettingsEstimation,
    targetIds = shiny::reactiveVal(1),
    comparatorIds = shiny::reactiveVal(c(2,4)),
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

test_that("estimationGetCmDiagnostics", {
  
diag <- estimationGetCmDiagnostics(
    connectionHandler = connectionHandlerEstimation,
    resultDatabaseSettings = resultDatabaseSettingsEstimation,
    targetIds = function(){1},
    comparatorIds = function(){c(2,4)},
    outcomeId = function(){3}
)

testthat::expect_true(nrow(diag) > 0)

})

