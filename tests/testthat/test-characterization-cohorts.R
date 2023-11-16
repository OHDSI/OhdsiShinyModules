context("characterization-cohorts")

shiny::testServer(
  app = characterizationTableServer, 
  args = list(
    connectionHandler = connectionHandlerCharacterization,
    resultDatabaseSettings = resultDatabaseSettingsCharacterization
    ), 
  expr = {
    
    
    # make sure options returns a data.frame
    testthat::expect_true(class(inputVals) == 'list')
    testthat::expect_true(!is.null(inputVals$cohortIds) )
    testthat::expect_true(!is.null(inputVals$databaseIds) )
    
    # checl generate does not crash app
    session$setInputs(targetIds = c(1,3))
    session$setInputs(databaseId = 'eunomia')
    session$setInputs(generate = T)
    
    # check input$columnSelect works without error
    session$setInputs(columnSelect = 'countValue')
    session$setInputs(columnSelect = 'averageValue')
    session$setInputs(columnSelect = c('averageValue','countValue'))

    
  })

test_that("Test characterizationTable ui", {
  # Test ui
  ui <- characterizationTableViewer(id = 'viewer')
  checkmate::expect_list(ui)
})
