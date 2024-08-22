context("characterization-DechallengeRechallenge")

shiny::testServer(
  app = characterizationDechallengeRechallengeServer, 
  args = list(
    connectionHandler = connectionHandlerCharacterization,
    resultDatabaseSettings = resultDatabaseSettingsCharacterization,
    targetId = shiny::reactive(1),
    outcomeId = shiny::reactive(3)
  ), 
  expr = {
    
    # options() is a list
    testthat::expect_true(inherits(options(), 'list'))
    
    # allData nrows > 0 
    testthat::expect_true(nrow(allData()) > 0 )
    
    # targetUniquePeople(), outcomeUniquePeople() numbers
    testthat::expect_true(inherits(targetUniquePeople(), "logical"))
    testthat::expect_true(inherits(outcomeUniquePeople(), "logical"))
    
    # characteriationDechalRechalColDefs is a list
    testthat::expect_true(inherits(characteriationDechalRechalColDefs(), 'list'))
    
    # failData NULL
    testthat::expect_true(is.null(failData()))
    
    # tableOutputs$actionCount()
    # failData not NULL
    
    
    # add tests for functions with progress bar
    
    
    
  })

test_that("Test characterizationDechallengeRechallenge ui", {
  # Test ui
  ui <- characterizationDechallengeRechallengeViewer(id = 'viewer')
  checkmate::expect_list(ui)
})
