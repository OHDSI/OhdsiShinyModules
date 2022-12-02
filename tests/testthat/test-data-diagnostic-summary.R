context("data-diagnostic-summary")

shiny::testServer(
  app = dataDiagnosticSummaryServer, 
  args = list(
    connectionHandler = connectionHandlerDataDiag,
    mySchema = resultDatabaseSettingsDataDiag$schema,
    myTableAppend = resultDatabaseSettingsDataDiag$tablePrefix 
  ), 
  expr = {
    
    # expect the resultTable reactiveVal to exist
    testthat::expect_true(!is.null(resultTable()))
    
  })