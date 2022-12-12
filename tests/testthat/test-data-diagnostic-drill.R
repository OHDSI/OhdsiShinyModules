context("data-diagnostic-drill")

shiny::testServer(
  app = dataDiagnosticDrillServer, 
  args = list(
    con = connectionDataDiag,
    mySchema = schemaTest,
    targetDialect = dbmsTest,
    myTableAppend = ""
  ), 
  expr = {
    
    # expect the analyses to exist
    testthat::expect_true(!is.null(analyses))
    testthat::expect_true(!is.null(databases))
    
    # check setting analysisSelected  (maybe also databasesSelected?)
    session$setInputs(analysisSelected = 2) 
    
    # check show_details
    session$setInputs(show_details = list(index = 1)) 
    
  })
