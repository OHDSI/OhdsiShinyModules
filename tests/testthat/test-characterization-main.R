context("characterization-main")

shiny::testServer(
  app = characterizationServer, 
  args = list(
    connectionHandler = connectionHandlerCharacterization,
    resultDatabaseSettings = resultDatabaseSettingsCharacterization 
  ), 
  expr = {
    
    testthat::expect_true(inherits(connectionHandler,"ConnectionHandler"))
    
    session$setInputs(mainPanel = 'Target Viewer')
    testthat::expect_true(input$mainPanel == 'Target Viewer')
    
    testthat::expect_true(previouslyLoaded() == c('Target Viewer'))
    
    session$setInputs(mainPanel = 'Outcome Stratified')
    testthat::expect_true(input$mainPanel == 'Outcome Stratified')
    
    testthat::expect_true(sum(previouslyLoaded() %in% c('Target Viewer','Outcome Stratified')) == 2)
    
    
    session$setInputs(mainPanel = 'Incidence Rate')
    testthat::expect_true(input$mainPanel == 'Incidence Rate')
    
    session$setInputs(mainPanel = 'Time To Event')
    testthat::expect_true(input$mainPanel == 'Time To Event')
    
    session$setInputs(mainPanel = 'Dechallenge Rechallenge')
    testthat::expect_true(input$mainPanel == 'Dechallenge Rechallenge')
    
    
  })


test_that("Test characterization ui", {
  # Test ui
  ui <- characterizationViewer(id = 'viewer')
  checkmate::expect_list(ui)
})

test_that("getCharacterizationTypes", {

  types <- getCharacterizationTypes(
    connectionHandler = connectionHandlerCharacterization,
    resultDatabaseSettings = resultDatabaseSettingsCharacterization
  )
  
  testthat::expect_is(types, 'character')
  
})
