context("sccs-results")

shiny::testServer(sccsResultsServer, args = list(
  id = "testSccsResultsServer",
  connectionHandler = connectionHandlerSccs,
  resultDatabaseSettings = resultDatabaseSettingsSccs,
  inputSelected = shiny::reactiveVal(NULL)
), {
  
  testthat::expect_is(selectedRow(), "NULL") 

  inputSelected(
    list(
      analysis = 13,
      outcome = 11123,
      exposure = "1 1"
    )
  )
  
  testthat::expect_true(!is.null(data())) 

})

test_that("Test sccs results ui", {
  # Test ui
  ui <- sccsResultsViewer('testing')
  checkmate::expect_list(ui)
})

