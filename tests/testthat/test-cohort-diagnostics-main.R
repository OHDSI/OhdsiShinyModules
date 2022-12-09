context("cohort-diagnostics-main")

shiny::testServer(cohortDiagnosticsSever, args = list(
  id = "testCdServer",
  connectionHandler = connectionHandlerCohortDiag,
  resultsDatabaseSettings = resultDatabaseSettingsCohortDiag,
  vocabularyDatabaseSchema = "main"
), {
  ## input tests will go here
  session$setInputs(
    tabs = "cohortCounts",
    database = "Eunomia"
  )
  expect_null(inputCohortIds())
})