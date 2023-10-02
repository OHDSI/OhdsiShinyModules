context("characterization-incidence")

shiny::testServer(
  app = characterizationIncidenceServer, 
  args = list(
    connectionHandler = connectionHandlerCharacterization,
    mainPanelTab = shiny::reactiveVal("Feature Comparison"),
    resultDatabaseSettings = resultDatabaseSettingsCharacterization
  ), 
  expr = {
    
    # make sure options is a list
    testthat::expect_true(class(options) == 'list')
    testthat::expect_true(!is.null(options$targetIds))
    testthat::expect_true(!is.null(options$outcomeIds))
    
    # check input$generate does not crash app
    # need to test generate in ns("input-selection")
    session$setInputs(`input-selection_generate` = 1)
    
    idata <- getIncidenceData(
      targetIds = options$targetIds[1],
      outcomeIds = options$outcomeIds[1],
      connectionHandler = connectionHandler,
      resultDatabaseSettings = resultDatabaseSettings
    )
    testthat::expect_is(idata, 'data.frame')
    
  })



test_that("Test characterizationIncidence ui", {
  # Test ui
  ui <- characterizationIncidenceViewer(id = 'viewer')
  checkmate::expect_list(ui)
})

