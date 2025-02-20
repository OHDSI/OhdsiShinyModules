context("home-main")

test_that("Test home ui", {
  # Test ui
  ui <- homeViewer()
  checkmate::expect_list(ui)
})

test_that('home helper file works',{
  help <- homeHelperFile()
  testthat::expect_true(help != '')
})

# check the home module server
shiny::testServer(
  app = homeServer, 
  args = list(
    connectionHandler = connectionHandlerCharacterization ,
    resultDatabaseSettings = resultDatabaseSettingsCharacterization
  ), 
  expr = {
    testthat::expect_true(!is.null(output$tabs))
  })

