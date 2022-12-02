context("description-main")

shiny::testServer(
  app = descriptionServer, 
  args = list(
    connectionHandler = connectionHandlerDesc ,
    resultDatabaseSettings = resultDatabaseSettingsDesc 
  ), 
  expr = {
    
    testthat::expect_true(inherits(connectionHandler,"ConnectionHandler"))
    
    session$setInputs(mainPanel = 'testing')
    testthat::expect_true(mainPanelTab() == 'testing')
    
  })