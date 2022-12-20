context("cohort-diagnostics-compateCharacterization")

shiny::testServer(compareCohortCharacterizationModule, args = list(
  id = "compareCharacterization",
  dataSource = dataSourceCd
), {

  session$setInputs(
    targetCohort = 14906,
    comparatorCohort = 14907,
    targetDatabase = "Eunomia",
    comparatorDatabase = "Eunomia"
  )

  selectionsOutput()
  compareCohortCharacterizationDataFiltered()

  checkmate::expect_double(selectedTimeIds())
  checkmate::expect_double(selectedTimeIdsSingle())

})