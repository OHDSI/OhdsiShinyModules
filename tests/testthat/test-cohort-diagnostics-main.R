context("cohort-diagnostics-main")

shiny::testServer(cohortDiagnosticsServer, args = list(
  id = "testCdServer",
  connectionHandler = connectionHandlerCohortDiag,
  resultDatabaseSettings = resultDatabaseSettingsCohortDiag,
  dataSource = dataSourceCd
), {
  ## input tests will go here
  session$setInputs(
    tabs = "cohortCounts",
    database = "Eunomia"
  )

  checkmate::expect_list(selectedCohorts())
})

test_that("Test cd ui", {
  # Test ui
  ui <- cohortDiagnosticsView()
  checkmate::expect_list(ui)
  checkmate::expect_file_exists(cohortDiagnosticsHelperFile())
})