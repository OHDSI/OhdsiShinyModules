context("characterization-TimeToEvent")

shiny::testServer(
  app = characterizationTimeToEventServer, 
  args = list(
    connectionHandler = connectionHandlerCharacterization,
    resultDatabaseSettings = resultDatabaseSettingsCharacterization
  ), 
  expr = {
    
    # make sure bothIds returns a list
    testthat::expect_true(class(bothIds) == 'list')
    testthat::expect_true(!is.null(bothIds$outcomeIds) )

    
    # checl targetId does not crash app
    session$setInputs(targetId = names(bothIds$outcomeIds)[1])
    
    # check input$generate does not crash app
    session$setInputs(outcomeId = 3)
    session$setInputs(generate = T)
    
    
  })


test_that("Test characterizationTimeToEvent ui", {
  # Test ui
  ui <- characterizationTimeToEventViewer(id = 'viewer')
  checkmate::expect_list(ui)
})
