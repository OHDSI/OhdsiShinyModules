context("treatment-patterns-sankey")

targetCohort <- OhdsiReportGenerator::getAnalysisCohorts(
  connectionHandler = connectionHandlerTreatmentPatterns,
  schema = resultDatabaseSettingsTreatmentPatterns$schema,
  tpTablePrefix = resultDatabaseSettingsTreatmentPatterns$tpTablePrefix
)

shiny::testServer(
  app = treatmentPatternsSankeyServer,
  arg = list(
    connectionHandler = connectionHandlerTreatmentPatterns,
    resultDatabaseSettings = resultDatabaseSettingsTreatmentPatterns,
    reactiveTargetRow = shiny::reactive(targetCohort[1:2, ])
  ),
  expr = {
    # Simulate Selections
    testthat::expect_true(length(databaseNames()) > 0)

    session$setInputs(databaseNames = c("Synthea"))

    testthat::expect_true(length(input$databaseNames) > 0)
    testthat::expect_equal(showSankey(), 0)

    session$setInputs(generate = 1)

    # Test pathway fetching
    testthat::expect_true(length(pathwayTable()) > 0)

    expectedSelection <- data.frame(
      Analyses = "1, 2",
      Targets = "ViralSinusitis",
      Databases = "Synthea",
      stringsAsFactors = FALSE,
      check.names = FALSE
    )


    testthat::expect_equal(
      selected(),
      expectedSelection,
      check.attributes = FALSE
    )
  }
)

test_that("Test treatmentPatternsSankey ui", {
  # Test ui
  ui <- treatmentPatternsSankeyViewer(id = "viewer")
  checkmate::expect_list(ui)
})
