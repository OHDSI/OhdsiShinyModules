context("characterization-DechallengeRechallenge")

shiny::testServer(
  app = characterizationDechallengeRechallengeServer, 
  args = list(
    connectionHandler = connectionHandlerCharacterization,
    mainPanelTab = shiny::reactiveVal("Feature Comparison"),
    resultDatabaseSettings = resultDatabaseSettingsCharacterization
  ), 
  expr = {
    
    # make sure bothIds returns a list
    testthat::expect_true(class(bothIds) == 'list')
    testthat::expect_true(!is.null(bothIds$outcomeIds) )

    # checl targetId does not crash app
    ##session$setInputs(targetId = names(bothIds$outcomeIds)[1])
    
    # test setting inputs - not sure this works
    session$setInputs(`input-selection_targetId` = names(bothIds$targetIds)[1])
    session$setInputs(`input-selection_outcomeId` = names(bothIds$outcomeIds)[1])
    session$setInputs(`input-selection_generate` = TRUE)
    
    # check allData sets these reactices
    ##testthat::expect_true(!is.null(allData()))
    
  })

test_that("Test characterizationDechallengeRechallenge ui", {
  # Test ui
  ui <- characterizationDechallengeRechallengeViewer(id = 'viewer')
  checkmate::expect_list(ui)
})