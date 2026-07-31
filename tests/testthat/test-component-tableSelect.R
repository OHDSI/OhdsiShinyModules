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
    helpText = 'help',
    inputColumns = list(
      madeUp = reactable::colDef(name = 'Made Up'),
      id = reactable::colDef(name = 'ID')
    )
  ), 
  expr = {
    
    testthat::expect_true(selectedRowId() == 0)
    testthat::expect_true(grepl('abc', output$selectionInput$html, fixed = TRUE))
    testthat::expect_true(grepl('help', output$selectionInput$html, fixed = TRUE))
    testthat::expect_true(grepl('No selection yet', output$selectionInput$html, fixed = TRUE))

    selectedRowId(1)
    session$flushReact()

    testthat::expect_true(selectedRowId() == 1)
    testthat::expect_true(grepl('Change abc', output$selectionInput$html, fixed = TRUE))
    testthat::expect_false(grepl('help', output$selectionInput$html, fixed = TRUE))
    
  })

test_that("Test tableSelectionViewer ui", {
  # Test ui
  ui <- tableSelectionViewer(id = 'tab-select')
  checkmate::expect_list(ui)
})
