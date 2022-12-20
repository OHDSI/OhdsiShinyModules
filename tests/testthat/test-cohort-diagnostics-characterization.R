context("cohort-diagnostics-characterization")

shiny::testServer(characterizationModule, args = list(
  id = "characterization",
  dataSource = dataSourceCd
), {

  session$setInputs(
    targetCohort = 14906,
    targetDatabase = "Eunomia"
  )

  checkmate::expect_double(selectedTimeIds())

})