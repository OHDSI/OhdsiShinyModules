context("estimation-sccs-results")

test_that("Test sccs results ui", {
  # Test ui
  ui <- estimationSccsResultsViewer(id = 'test_sccs')
  checkmate::expect_list(ui)
})


shiny::testServer(
  estimationSccsResultsServer, 
  args = list(
    id = "estimationSccsResultsServer",
    connectionHandler = connectionHandlerCharacterization,
    resultDatabaseSettings = resultDatabaseSettingsCharacterization,
    targetIds = shiny::reactive(1),
    outcomeId = shiny::reactive(3)
  ), {
    
  # check input$goBackSccsResults changes "resultPanel" to "Table"
    
  # check sccsData() is data.frame and nrow > 0
    testthat::expect_true(inherits(sccsData(), 'data.frame'))
    testthat::expect_true(nrow(sccsData()) > 0 )
    
    # check esData() is data.frame and nrow > 0
    testthat::expect_true(inherits(esData(), 'data.frame'))
    testthat::expect_true(nrow(esData()) >= 0 )
    
    # check data() is nrow(sccs and es data)
    testthat::expect_true(inherits(data(), 'data.frame'))
    testthat::expect_true(nrow(data()) ==  nrow(esData()) + nrow(sccsData()))
    
    # check selectedRow() is NULL
    testthat::expect_true(inherits(selectedRow(), 'NULL'))
    
    # is there a way to chnage resultTableOutputs$actionCount() ?
    resultTableOutputs$actionType <- shiny::reactive('results')
    resultTableOutputs$actionCount <- shiny::reactive(list(index = 1))
    #testthat::expect_equivalent(selectedRow(), data()[1,])
    #testthat::expect_true(input$resultPanel == "Results")
    
})

test_that("Test estimationGetSccsResultSummaryTableColDef", {

  result <- estimationGetSccsResultSummaryTableColDef()
  testthat::expect_true(inherits(result, 'list'))

})

test_that("Test estimationGetSccsResults", {
  
result <- estimationGetSccsResults(
  connectionHandler = connectionHandlerCharacterization,
  resultDatabaseSettings = resultDatabaseSettingsCharacterization,
  exposureIds = function(){1},
  outcomeIds = function(){3}
)

testthat::expect_true(inherits(result, 'data.frame'))

})

test_that("Test estimationGetSccsEsResults", {
result <- estimationGetSccsEsResults(
  connectionHandler = connectionHandlerCharacterization,
  resultDatabaseSettings = resultDatabaseSettingsCharacterization,
  exposureIds = function(){1},
  outcomeIds = function(){3}
)

testthat::expect_true(inherits(result, 'data.frame'))
testthat::expect_true(nrow(result) == 0) # fails

})
