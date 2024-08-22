context("characterization-caseSeries")

shiny::testServer(
  app = characterizationCaseSeriesServer, 
  args = list(
    connectionHandler = connectionHandlerCharacterization ,
    resultDatabaseSettings = resultDatabaseSettingsCharacterization,
    targetId = shiny::reactive(1), #reactive 
    outcomeId = shiny::reactive(3)
    ), 
  expr = {
    
    # make sure options returns a list
    testthat::expect_true(class(options()) == 'list')
    testthat::expect_true(length(options()$databaseIds) > 0 )
    testthat::expect_true(length(options()$tarInds) > 0 )
    
    testthat::expect_true(inherits(selected(), 'NULL'))
    
    # check setting and generating works
    session$setInputs(tarInd = options()$tarInds[1]) 
    session$setInputs(databaseId = options()$databaseIds[1]) 
    session$setInputs(generate = TRUE)
    
    testthat::expect_true(inherits(selected(), 'data.frame'))
    
    data <- characterizationGetCaseSeriesData(
      connectionHandler = connectionHandlerCharacterization ,
      resultDatabaseSettings = resultDatabaseSettingsCharacterization,
      targetId = 1,
      outcomeId = 3,
      databaseId = options()$databaseIds[1],
      tar = list(
        riskWindowStart = 1,
        riskWindowEnd = 365,
        startAnchor = 'cohort start',
        endAnchor = 'cohort start'
      )
    )
    
    testthat::expect_true(inherits(data, 'list'))
    testthat::expect_true(nrow(data$binary) > 0 )
    testthat::expect_true(nrow(data$continuous) > 0 )
    
  })


test_that("Test characterizationCaseSeriesViewer ui", {
  # Test ui
  ui <- characterizationCaseSeriesViewer(id = 'viewer')
  checkmate::expect_list(ui)
})

test_that("Test characterizationGetCaseSeriesOptions", {
options <- characterizationGetCaseSeriesOptions(
  connectionHandler = connectionHandlerCharacterization ,
  resultDatabaseSettings = resultDatabaseSettingsCharacterization,
  targetId = 2,
  outcomeId = 3
)

testthat::expect_true(inherits(options, 'list'))

})

test_that("Test colDefsBinary", {
  colDefs <- colDefsBinary('test')
  testthat::expect_true(inherits( colDefs, 'list'))
  testthat::expect_true(length(colDefs) > 0 )
})

test_that("Test colDefsContinuous", {
  colDefs <- colDefsContinuous('test')
  testthat::expect_true(inherits( colDefs, 'list'))
  testthat::expect_true(length(colDefs) > 0 )
})
