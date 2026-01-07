context("treatment-patterns-tables")


targetCohort <- OhdsiReportGenerator::getAnalysisCohorts(
  connectionHandler = connectionHandlerTreatmentPatterns,
  schema = resultDatabaseSettingsTreatmentPatterns$schema,
  tpTablePrefix = resultDatabaseSettingsTreatmentPatterns$tpTablePrefix
)

shiny::testServer(
  app = treatmentPatternsTabularServer,
  args = list(
    connectionHandler = connectionHandlerTreatmentPatterns,
    resultDatabaseSettings = resultDatabaseSettingsTreatmentPatterns,
    reactiveTargetRow = shiny::reactive(targetCohort[1:2, ])
  ),
  expr = {
    testthat::expect_true(length(databaseNames()) > 0)
    testthat::expect_true(length(reactiveTargetRow()) > 0)
    testthat::expect_equal(showTables(), 0)
    # set inputs
    session$setInputs(
      databaseName = databaseNames()[1],
      age = "all",
      indexYear = "all",
      sex = "all",
      generate = 1
    )

    testthat::expect_equal(input$age, "all")
    testthat::expect_equal(input$sex, "all")
    testthat::expect_equal(input$indexYear, "all")

    testthat::expect_equal(showTables(), 1)

    testthat::expect_true(length(pathwayTable()) > 0)
    testthat::expect_true(length(summaryTable()) > 0)
    testthat::expect_true(inherits(selected(), "data.frame"))

    testthat::expect_equal(length(pathwayGroups()), 2)
  }
)

pathways <- OhdsiReportGenerator::getTreatmentPathways(
  connectionHandler = connectionHandlerTreatmentPatterns,
  schema = resultDatabaseSettingsTreatmentPatterns$schema,
  tpTablePrefix = resultDatabaseSettingsTreatmentPatterns$tpTablePrefix
) %>%
  dplyr::slice_head(n = 20) %>%
  dplyr::select(-targetCohortId, -age, -sex, -indexYear)


test_that("test createPathwaysTable ", {
  pathwayDf <- createPathwaysTable(pathways, 774)

  expect_true(inherits(pathwayDf, "data.frame"))

  # max pathway length was 4
  expect_equal(ncol(pathwayDf), 10)
})

test_that("test createPathwaysTable ", {
  pathwayDf <- createPathwaysTable(pathways, 746)

  expect_true(inherits(pathwayDf, "data.frame"))

  # max pathway length was 4
  expect_equal(ncol(pathwayDf), 10)
})

test_that("test createEventCountTable", {
  eventDf <- createEventCountTable(pathways, 746) %>%
    dplyr::select(event, freq)

  expect_true(inherits(eventDf, "data.frame"))

  # test an event cnt
  expectedCnt <- pathways %>%
    dplyr::filter(stringr::str_detect(pathway, stringr::fixed("Amoxicillin+Clavulanate"))) %>%
    dplyr::summarize(n = sum(freq, na.rm = TRUE)) %>%
    dplyr::pull(n)


  cnt <- eventDf %>%
    dplyr::filter(event == "Amoxicillin+Clavulanate") %>%
    dplyr::pull(freq)


  expect_equal(expectedCnt, cnt)
})

test_that("test, createEventRankTable", {
  rankDf <- createEventRankTable(pathways, 746)

  expect_true(inherits(eventDf, "data.frame"))

  expectedCnt <- pathways %>%
    dplyr::filter(stringr::str_starts(pathway, stringr::fixed("Amoxicillin+Clavulanate"))) %>%
    dplyr::summarize(n = sum(freq, na.rm = TRUE)) %>%
    dplyr::pull(n)

  cnt <- rankDf %>%
    dplyr::filter(event == "Amoxicillin+Clavulanate", rank == 1) %>%
    dplyr::pull(freq)

  expect_equal(expectedCnt, cnt)
})
