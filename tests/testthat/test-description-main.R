context("description-main")

shiny::testServer(
  app = descriptionServer, 
  args = list(
    resultDatabaseSettings = list(
      dbms = dbmsTest,
      server = serverDesc,
      user = NULL,
      password = NULL,
      port = NULL,
      tablePrefix = descTablePrefix,
      cohortTablePrefix = cohortTablePrefix,
      databaseTable = databaseTable,
      schema = schemaTest,
      incidenceTablePrefix = incidenceTablePrefix,
      tempEmulationSchema = NULL
    )
  ), 
  expr = {
    
 testthat::expect_true(class(con) == 'DatabaseConnectorDbiConnection')
    
    session$setInputs(mainPanel = 'testing')
    testthat::expect_true(mainPanelTab() == 'testing')
    
  })
