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
    
    # create a html file in a folder
    # add the folder location to environmental var shiny_report_folder
    tempDir <- file.path(tempdir(),'reports')
    if(!dir.exists(tempDir)){
      dir.create(tempDir, recursive = T)
    }
    write.table(x = c(a=1, b=2), file = file.path(tempDir, 'Prediction.html'))
    Sys.setenv(shiny_report_folder = tempDir)
    shiny::addResourcePath('www-reports', tempDir)
    
    testthat::expect_true(!is.null(output$tabs))
  })

