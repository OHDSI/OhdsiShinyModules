context("data-diagnostic-summary")

shiny::testServer(
  app = dataDiagnosticSummaryServer, 
  args = list(
    con = connectionDataDiag,
    mySchema = schemaTest,
    targetDialect = dbmsTest,
    myTableAppend = ""
  ), 
  expr = {
    
    # expect the resultTable reactiveVal to exist
    testthat::expect_true(!is.null(resultTable()))
    
  })