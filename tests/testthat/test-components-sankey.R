context("component-sankey")

pathways <- OhdsiReportGenerator::getTreatmentPathways(
  connectionHandler = connectionHandlerTreatmentPatterns,
  schema = resultDatabaseSettingsTreatmentPatterns$schema,
  tpTablePrefix = resultDatabaseSettingsTreatmentPatterns$tpTablePrefix
)

fake_widget <- htmlwidgets::createWidget(
  name = "mockSankey",
  x = list(message = "mock sankey"),
  package = "htmlwidgets"
)


testthat::local_mocked_bindings(
  createSankeyDiagram = fake_createSankey,
  .package = "TreatmentPatterns"
)

shiny::testServer(
  app = sankeyPlotServer,
  args = list(
    pathwayTable = pathways,
    sankeyList = c(1, 2),
    filterColumn = "analysisId"
  ),
  expr = {
    session$flushReact()

    testthat::expect_equal(length(widgetList()), 2)

    testthat::expect_setequal(names(widgetList()), c("widget_1", "widget_2"))
  }
)
