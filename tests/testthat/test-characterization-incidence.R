context("characterization-incidence")

shiny::testServer(
  app = characterizationIncidenceServer, 
  args = list(
    connectionHandler = connectionHandlerCharacterization,
    mainPanelTab = shiny::reactiveVal("Feature Comparison"),
    resultDatabaseSettings = resultDatabaseSettingsCharacterization
  ), 
  expr = {
    
    # make sure cohorts is a data.frame
    testthat::expect_true(class(cohorts) == 'list')
    testthat::expect_true(!is.null(cohorts$targetIds))
    testthat::expect_true(!is.null(cohorts$outcomeIds))
    
    # check input$generate does not crash app
    session$setInputs(`input-selection_targetId` = 1)
    session$setInputs(`input-selection_outcomeId` = 3)
    session$setInputs(`input-selection_generate` = T)
    
  })



test_that("Test characterizationIncidence ui", {
  # Test ui
  ui <- characterizationIncidenceViewer(id = 'viewer')
  checkmate::expect_list(ui)
})

