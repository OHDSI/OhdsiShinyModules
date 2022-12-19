context("cohort-diagnostics-incidence")

shiny::testServer(incidenceRatesModule, args = list(
  id = "testIncidenceRates", #Any string is ok?
  dataSource = dataSourceCd,
  cohortTable = getCohortTable(dataSourceCd),
  selectedCohorts = shiny::reactive("Any String"),
  selectedDatabaseIds = shiny::reactive("Eunomia"),
  cohortIds = shiny::reactive({ c(14906, 14907) })
), {
  ## input tests will go here
  session$setInputs(
    irStratification = c("Age", "Calendar Year", "Sex"),
    minPersonYear = 0,
    minSubjectCount = 0
  )

  checkmate::expect_data_frame(incidenceRateData())

})
