context("treatment-patterns-main")

shiny::testServer(
  app = treatmentPatternsServer,
  args = list(
    connectionHandler = connectionHandlerTreatmentPatterns,
    resultDatabaseSettings = resultDatabaseSettingsTreatmentPatterns
  ),
  exp = {
    testthat::expect_true(inherits(connectionHandler, "ConnectionHandler"))

    # test initial states
    testthat::expect_true(nrow(targetTable) > 0)
    testthat::expect_null(reactiveTargetRowId())
    testthat::expect_null(reactiveTargetRow())

    session$flushReact()

    # simulate selecting
    reactiveTargetRowId(c(1, 2))
    testthat::expect_true(is.numeric(reactiveTargetRowId()))
    testthat::expect_equal(length(reactiveTargetRowId()), 2)

    session$flushReact()


    testthat::expect_true(is.data.frame(reactiveTargetRow()))
    testthat::expect_equal(nrow(reactiveTargetRow()), 2)
  }
)


test_that("Test treatmentPatternsMain ui", {
  # Test ui
  ui <- treatmentPatternsViewer(id = "viewer")
  checkmate::expect_list(ui)
})
