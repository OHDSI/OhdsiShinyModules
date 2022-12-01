context("data-diagnostic-main")

shiny::testServer(
  app = dataDiagnosticServer, 
  args = list(
    resultDatabaseSettings = list(
      dbms = dbmsTest,
      server = '../resources/datadiagDatabase/databaseFile.sqlite',
      user = NULL,
      password = NULL,
      port = NULL,
      tablePrefix = "",
      schema = schemaTest
    )
  ), 
  expr = {
    
    testthat::expect_true(class(con) == 'DatabaseConnectorDbiConnection')
    
    # check tabs dont cause errors
    session$setInputs(main_tabs = 'Drill-Down')
    
    
  })