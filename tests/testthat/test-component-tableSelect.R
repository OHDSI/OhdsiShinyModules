context("component-tableSelect")

shiny::testServer(
  app = tableSelectionServer, 
  args = list(
    table = shiny::reactive(
      data.frame(
        madeUp = 1:5,
        id = 2:6
      )
    ),
    selectedRowId = shiny::reactiveVal(0),
    selectButtonText = 'abc',
    helpText = 'help'
  ), 
  expr = {
    
    testthat::expect_true(selectedRowId() == 0)
    testthat::expect_true(selection == 'single')
    testthat::expect_true(selectButtonText() == 'abc')
    testthat::expect_true(helpTextReactive() == 'help')
    
    # select row
    reactable::updateReactable(outputId = 'inputTable', selected = 1)
    
    # input$confirmInput
    session$setInputs(confirmInput = TRUE)
    
  })

test_that("Test tableSelectionViewer ui", {
  # Test ui
  ui <- tableSelectionViewer(id = 'tab-select')
  checkmate::expect_list(ui)
})
