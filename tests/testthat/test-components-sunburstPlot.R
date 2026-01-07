context("components-sunburstPlot")

pathways <- OhdsiReportGenerator::getTreatmentPathways(
  connectionHandler = connectionHandlerTreatmentPatterns,
  schema = resultDatabaseSettingsTreatmentPatterns$schema,
  tpTablePrefix = resultDatabaseSettingsTreatmentPatterns$tpTablePrefix
)


shiny::testServer(
  app = sunburstPlotServer,
  args = list(
    pathwayTable = pathways,
    plotWidth = 800,
    plotHeight = 400,
    filterColumn = "analysisId",
    sunburstList = c(1, 2),
    filenamePrefix = "sunburst"
  ),
  expr = {
    session$flushReact()

    testthat::expect_equal(length(widgetList()), 2)

    testthat::expect_setequal(names(widgetList()), c("widget_1", "widget_2"))

    all_htmlwidgets <- all(vapply(widgetList(), function(w) inherits(w, "htmlwidget"), logical(1)))
    testthat::expect_true(all_htmlwidgets)
  }
)

test_that("Test sunburst ui", {
  # Test ui
  ui <- sunburstPlotViewer(id = "viewer")
  checkmate::expect_list(ui)
})