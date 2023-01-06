context("cohort-diagnostics-inclusion")

shiny::testServer(inclusionRulesModule, args = list(
  id = "testIncidenceRates", #Any string is ok?
  dataSource = dataSourceCd,
  selectedCohort = shiny::reactive("Any String"),
  selectedDatabaseIds = shiny::reactive("Eunomia"),
  targetCohortId = shiny::reactive({ c(14906) })
), {
  checkmate::expect_list(output$selectedCohort)
  expect_error(output$inclusionRuleTable)
})


shiny::testServer(inclusionRulesModule, args = list(
  id = "testIncidenceRates", #Any string is ok?
  dataSource = dataSourceCd,
  selectedCohort = shiny::reactive("Any String"),
  selectedDatabaseIds = shiny::reactive("Eunomia"),
  targetCohortId = shiny::reactive({ c(18350) })
), {
  ## input tests will go here
  session$setInputs(
    inclusionRuleTableFilters = "All",
    inclusionRulesShowAsPercent = FALSE
  )
  checkmate::expect_class(output$inclusionRuleTable, "json")
})
