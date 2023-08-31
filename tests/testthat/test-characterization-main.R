context("characterization-main")

shiny::testServer(
  app = characterizationServer, 
  args = list(
    connectionHandler = connectionHandlerCharacterization,
    resultDatabaseSettings = resultDatabaseSettingsCharacterization 
  ), 
  expr = {
    
    testthat::expect_true(inherits(connectionHandler,"ConnectionHandler"))
    
    session$setInputs(mainPanel = 'testing')
    testthat::expect_true(mainPanelTab() == 'testing')
    
  })


test_that("Test characterization ui", {
  # Test ui
  ui <- characterizationViewer(id = 'viewer')
  checkmate::expect_list(ui)
})
