context("sccs-main")

shiny::testServer(sccsServer, args = list(
  id = "testSccsServer",
  connectionHandler = connectionHandlerSccs,
  resultDatabaseSettings = resultDatabaseSettingsSccs
), {
  session$setInputs(
    exposuresOutcome = "[EPI_1024] canagliflozin exposures w 0d prior obsv, 30d gap, male - [EPI_1024] Prostatitis Syndrome events (remove testicular lesions, bladder neoplasm and hernia)",
    database = "1038356333",
    analysis = 1
  )
  checkmate::expect_data_frame(resultSubset())

  checkmate::expect_class(output$mainTable, "json")

  expect_null(selectedRow())

  # Dependency injection to mimic selection of a row in the table

  session$setInputs(
    mainTableRowInput = 1
  )
  
  testthat::expect_equal(selectedRow(),1)
  # End of testing that can be done without data
  # The following will be filled in when test data are available
  # checkmate::expect_data_frame(selectedRow())
  # output$powerTable
  # output$timeTrendPlot
  # output$modelTable
  # output$attritionPlot
  # output$timeTrendPlot
  # output$timeToEventPlot
  # output$ageSplinePlot
  # output$seasonSplinePlot
  # output$controlEstimatesPlot
  # output$diagnosticsSummary

})

test_that("Test cd ui", {
  # Test ui
  ui <- sccsView()
  checkmate::expect_list(ui)
  checkmate::expect_file_exists(sccsHelperFile())
})