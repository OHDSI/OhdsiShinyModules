test_that("the self controlled cohort helper file exists", {
  expect_true(file.exists(selfControlledCohortHelperFile()))
})

test_that("the self controlled cohort module viewer can be created", {
  ui <- selfControlledCohortViewer(id = "testScc")
  expect_true(inherits(ui, "shiny.tag") || inherits(ui, "shiny.tag.list"))
})

test_that("selfControlledCohortCombineResults combines database and meta results", {
  conDet <- OhdsiReportGenerator::getExampleConnectionDetails()
  connectionHandler <- ResultModelManager::ConnectionHandler$new(
    conDet,
    loadConnection = FALSE
  )
  settings <- list(
    schema = "main",
    sccTablePrefix = "scc_",
    cgTablePrefix = "cg_",
    esTablePrefix = "es_",
    databaseTable = "database_meta_data"
  )
  est <- OhdsiReportGenerator::getSccEstimation(
    connectionHandler = connectionHandler,
    schema = settings$schema,
    sccTablePrefix = settings$sccTablePrefix,
    cgTablePrefix = settings$cgTablePrefix,
    databaseTable = settings$databaseTable,
    targetIds = 9,
    outcomeIds = 11
  )
  meta <- OhdsiReportGenerator::getSccMetaEstimation(
    connectionHandler = connectionHandler,
    schema = settings$schema,
    sccTablePrefix = settings$sccTablePrefix,
    cgTablePrefix = settings$cgTablePrefix,
    esTablePrefix = settings$esTablePrefix,
    targetIds = 9,
    outcomeIds = 11
  )
  combined <- selfControlledCohortCombineResults(est, meta)
  expect_equal(nrow(combined), nrow(est) + nrow(meta))
  expect_true(all(c("meta", "calibratedRr", "calibratedLb95",
                    "calibratedUb95", "calibratedPValue") %in% colnames(combined)))
  # uncalibrated estimates should not be returned
  expect_false(any(c("rr", "lb95", "ub95", "pValue", "seLogRr") %in% colnames(combined)))
  expect_true(sum(combined$meta) == nrow(meta))
  expect_null(connectionHandler$closeConnection())
})

test_that("the self controlled cohort col defs can be created", {
  colDefs <- selfControlledCohortDetailedColDef()
  expect_true(all(c("calibratedRr", "description", "databaseName") %in% names(colDefs)))
  # uncalibrated estimate columns should not be shown
  expect_false(any(c("rr", "lb95", "ub95", "pValue") %in% names(colDefs)))
  signalsColDefs <- selfControlledCohortSignalsColDef()
  expect_true(all(c("benefitCount", "riskCount", "metaRr") %in% names(signalsColDefs)))
})
