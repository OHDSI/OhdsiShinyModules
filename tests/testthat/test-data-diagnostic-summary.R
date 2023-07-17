context("data-diagnostic-summary")

shiny::testServer(
  app = dataDiagnosticSummaryServer, 
  args = list(
    connectionHandler = connectionHandlerDataDiag,
    resultDatabaseSettings = resultDatabaseSettingsDataDiag
  ), 
  expr = {
    
    # expect the resultTable reactiveVal to exist
    testthat::expect_true(!is.null(resultTable()))
    
  })
