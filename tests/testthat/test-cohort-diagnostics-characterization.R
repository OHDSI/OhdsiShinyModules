context("cohort-diagnostics-characterization")

shiny::testServer(characterizationModule, args = list(
  id = "characterization",
  dataSource = dataSourceCd
), {
  expect_null(getCohortConceptSets())

  tids <- getResultsTemporalTimeRef(dataSource = dataSource)
  session$setInputs(
    targetCohort = 18347,
    targetDatabase = "Eunomia",
    selectedConceptSet = "",
    proportionOrContinuous = "Proportion",
    generateRaw = 1,
    generateReport = 1,
    characterizationDomainIdFilter = domainIdOptions,
    characterizationAnalysisNameFilter = analysisNameOptions,
    timeIdChoices =  tids$temporalChoices,
    characterizationColumnFilters = "Mean"
  )
  checkmate::expect_double(selectedTimeIds())

  checkmate::expect_data_frame(getCohortConceptSets())
  checkmate::expect_data_frame(getResolvedConcepts())
  expect_null(getMappedConcepts())
  expect_error(getFilteredConceptIds())

  checkmate::expect_list(selectionsPanel())

  rawTableTimeIdReactable()
  rawTableReactable()

  session$setInputs(
    targetCohort = 18347,
    targetDatabase = "Eunomia",
    selectedConceptSet = "",
    proportionOrContinuous = "Continuous",
    characterizationColumnFilters = "Mean and Standard Deviation"
  )
  rawTableTimeIdReactable()
  rawTableReactable()
  cohortCharacterizationPrettyTable()
})