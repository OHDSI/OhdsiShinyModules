context("cohort-diagnostics-counts")

shiny::testServer(cohortCountsModule, args = list(
  id = "testcohortcounts", #Any string is ok?
  dataSource = dataSourceCd,
  cohortTable = dataSourceCd$cohortTable,
  databaseTable = dataSourceCd$dbTable,
  selectedCohorts = shiny::reactive("Any String"),
  selectedDatabaseIds = shiny::reactive("Eunomia"),
  cohortIds = shiny::reactive({ c(18350, 18347) })
), {
  # Checking to see if a dataframe is returned and all the elements are of the
  # correct datatype
  checkmate::expect_data_frame(getResults())
  checkmate::expect_numeric(getResults()$cohortId)
  checkmate::expect_numeric(getResults()$cohortEntries)
  checkmate::expect_numeric(getResults()$cohortSubjects)
  checkmate::expect_character(getResults()$databaseId)
  checkmate::expect_character(getResults()$cohortName)

  session$setInputs(
    cohortCountsTableColumnFilter = "Persons"
  )
  checkmate::expect_class(output$cohortCountsTable, "json")

  session$setInputs(
    cohortCountsTableColumnFilter = "Records"
  )
  checkmate::expect_class(output$cohortCountsTable, "json")
  expect_error(getInclusionRulesTable(dataSourceCd, 18347, "Eunomia",  c("Meet", "Gain", "Remain"), 0, TRUE))
})

