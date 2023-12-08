context("characterization-aggregateFeatures")

shiny::testServer(
  app = characterizationAggregateFeaturesServer, 
  args = list(
    connectionHandler = connectionHandlerCharacterization ,
    resultDatabaseSettings = resultDatabaseSettingsCharacterization
    ), 
  expr = {
    
    # make sure options returns a list
    testthat::expect_true(class(options) == 'list')
    testthat::expect_true(length(options) >0 )
    
    # check setting and generating works
    session$setInputs(`input-selection_tarIds` = options$tars[1]) 
    session$setInputs(`input-selection_targetIds` = options$targets[1]) 
    session$setInputs(`input-selection_outcomeIds` = options$outcomes[1]) 
    session$setInputs(`input-selection_database` = 'eunomia')
    session$setInputs(`input-selection_index` = 'T')
    session$setInputs(`input-selection_firstO` = T)
    session$setInputs(`input-selection_generate` = TRUE)
    
    testthat::expect_true(!is.null(allData))
    
  })


test_that("Test characterizationAggregateFeatures ui", {
  # Test ui
  ui <- characterizationAggregateFeaturesViewer(id = 'viewer')
  checkmate::expect_list(ui)
})
