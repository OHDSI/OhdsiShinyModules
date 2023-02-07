context("cohort-diagnostics-compare characterization")

shiny::testServer(compareCohortCharacterizationModule, args = list(
  id = "compareCharacterization",
  dataSource = dataSourceCd
), {

  expect_error(compareCohortCharacterizationBalanceData())

  tidOpts <- getResultsTemporalTimeRef(dataSource = dataSource)
  session$setInputs(
    targetCohort = 14906,
    comparatorCohort = 14907,
    targetDatabase = "Eunomia",
    comparatorDatabase = "Eunomia",
    analysisNameFilter = analysisNameOptions,
    domainIdFilter = domainIdOptions,
    timeIdChoices = tidOpts$temporalChoices,
    timeIdChoicesSingle = tidOpts$temporalChoices,
  )

  selectionsOutput()
  compareCohortCharacterizationDataFiltered()

  checkmate::expect_double(selectedTimeIds())
  checkmate::expect_double(selectedTimeIdsSingle())

  checkmate::expect_data_frame(compareCohortCharacterizationBalanceData())

  session$setInputs(
    targetCohort = 18347,
    comparatorCohort = 18348,
    targetDatabase = "Eunomia",
    comparatorDatabase = "Eunomia"
  )

  checkmate::expect_data_frame(compareCohortCharacterizationBalanceData())

})