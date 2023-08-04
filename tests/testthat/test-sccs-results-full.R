context("sccs-results-full")

shiny::testServer(sccsFullResultServer, args = list(
  id = "testSccsResultFullServer",
  connectionHandler = connectionHandlerSccs,
  resultDatabaseSettings = resultDatabaseSettingsSccs,
  selectedRow = shiny::reactiveVal(NULL)
), {
  
  testthat::expect_is(selectedRow(), "NULL") 
  
  selectedRow(
    data.frame(
      outcomeId = 11123,
      databaseId = 1,
      analysisId = 13,
      covariateId = 1,
      covariateName = 'test',
      description = 'madeup',
      databaseName = 'db',
      analysis = 13,
      outcome = 11123,
      exposure = 1,
      unblind = 1,
      outcomeEvents = 10,
      outcomeSubjects = 100,
      observedDays = 100,
      covariateSubjects = 1000,
      covariateDays = 1000,
      covariateOutcomes = 20,
      mdrr = 2
    )
  )

})

test_that("Test sccs full results ui", {
  # Test ui
  ui <- sccsFullResultViewer('fullview')
  checkmate::expect_list(ui)
})

