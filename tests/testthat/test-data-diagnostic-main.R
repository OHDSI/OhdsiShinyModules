context("data-diagnostic-main")

shiny::testServer(
  app = dataDiagnosticServer, 
  args = list(
    connectionHandler = connectionHandlerDataDiag,
    resultDatabaseSettings = resultDatabaseSettingsDataDiag
  ), 
  expr = {
    
    testthat::expect_true(inherits(connectionHandler,"ConnectionHandler"))
    
    # check tabs dont cause errors
    session$setInputs(main_tabs = 'Drill-Down')
    
    
  })

test_that("Test data-diag ui", {
  # Test ui
  ui <- dataDiagnosticViewer()
  checkmate::expect_list(ui)
})
