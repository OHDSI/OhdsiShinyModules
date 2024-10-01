context("cohort-diagnostics-concepts in data source")

shiny::testServer(conceptsInDataSourceModule, args = list(
  id = "conceptsInDataSourceModule",
  dataSource = dataSourceCd,
  selectedCohort = shiny::reactive({ 14096 }),
  selectedDatabaseIds = shiny::reactive({"Eunomia"}),
  targetCohortId = shiny::reactive({14096}),
  selectedConceptSets = shiny::reactive({ NULL }),
  conceptSetIds = shiny::reactive({ NULL })
), {

  conceptsInDataSourceReactive()
  getResolvedConcepts()
  getMappedConcepts()

  checkmate::expect_data_frame(getDisplayTableHeaderCount(dataSourceCd, 14096, "Eunomia"))
  expect_error(output$conceptsInDataSourceTable)
})
