context("cohort-diagnostics-incidence")

shiny::testServer(inclusionRulesModule, args = list(
  id = "testIncidenceRates", #Any string is ok?
  dataSource = dataSourceCd,
  selectedCohort = shiny::reactive("Any String"),
  selectedDatabaseIds = shiny::reactive("Eunomia"),
  targetCohortId = shiny::reactive({ c(14906) })
), {
  checkmate::expect_list(output$selectedCohort)
  expect_error(output$inclusionRulesTable)
})


shiny::testServer(inclusionRulesModule, args = list(
  id = "testIncidenceRates", #Any string is ok?
  dataSource = dataSourceCd,
  selectedCohort = shiny::reactive("Any String"),
  selectedDatabaseIds = shiny::reactive("Eunomia"),
  targetCohortId = shiny::reactive({ c(17492) })
), {
  ## input tests will go here
  session$setInputs(
    inclusionRuleTableFilters = "All",
    inclusionRulesShowAsPercent = FALSE
  )

  expect_error(output$inclusionRulesTable)
})
