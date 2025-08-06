context("components-data-viewer")

test_that("components-data-viewer server works", {
  
shiny::testServer(
  app = resultTableServer, 
  args = list(
    df = shiny::reactive({
      data.frame(
        a = 1:5,
        b= 2:6
      )
    })
  ), 
  expr = {
    
    testthat::expect_true(nrow(dfWithActions()) == 5)
    testthat::expect_true(is.null(onClick))
    
    testthat::expect_true(actionCount() == 0 )
    testthat::expect_true(actionIndex() == 0 )
    testthat::expect_true(actionType() == 'none' )
    
    session$setInputs(dataCols = columnsToSelectOptions()[1])
    
})
  
})


test_that("Test result table ui", {
  # Test ui
  ui <- resultTableViewer()
  checkmate::expect_list(ui)
})


# extractColumnRelations
