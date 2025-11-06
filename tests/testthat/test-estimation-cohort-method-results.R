context("estimation-cohort-method-results")

test_that("Test sccs results ui", {
  # Test ui
  ui <- estimationCmResultsViewer(id = 'test_cm')
  checkmate::expect_list(ui)
})


shiny::testServer(
  estimationCmResultsServer, 
  args = list(
    id = "estimationCmResultsServer",
    connectionHandler = connectionHandlerCharacterization,
    resultDatabaseSettings = resultDatabaseSettingsCharacterization,
    targetIds = shiny::reactiveVal(1003),
    outcomeId = shiny::reactiveVal(3)
  ), {
    
  # check input$goBackCmResults changes "resultPanel" to "Table"
    
  # check cmData() is data.frame and nrow > 0
    testthat::expect_true(inherits(cmData(), 'data.frame'))
    testthat::expect_true(nrow(cmData()) > 0 )
    
    # check esData() is data.frame and nrow > 0
    testthat::expect_true(inherits(esData(), 'data.frame'))
    testthat::expect_true(nrow(esData()) >= 0 )
    
    # check data() is nrow(sccs and es data)
    testthat::expect_true(inherits(data(), 'data.frame'))
    testthat::expect_true(nrow(data()) ==  nrow(esData()) + nrow(cmData()))
    
    # check selectedRow() is NULL
    testthat::expect_true(inherits(selectedRow(), 'NULL'))
    
    # is there a way to chnage resultTableOutputs$actionCount() ?
    resultTableOutputs$actionType <- shiny::reactive('results')
    resultTableOutputs$actionCount <- shiny::reactive(list(index = 1))
    #testthat::expect_equivalent(selectedRow(), data()[1,])
    #testthat::expect_true(input$resultPanel == "Results")
    
})

test_that("Test estimationGetCmResultSummaryTableColDef", {

  result <- estimationGetCmResultSummaryTableColDef()
  testthat::expect_true(inherits(result, 'list'))

})

