context("patient-level-prediction-main")

shiny::testServer(
  app = patientLevelPredictionServer,
  args = list(
    connectionHandler = connectionHandlerCharacterization,
    resultDatabaseSettings = resultDatabaseSettingsCharacterization
  ),
  expr = {

    expect_true(performanceRowId() == 0)
    testthat::expect_is(performances, "data.frame")
    expect_true(nrow(performances) > 0)
    expect_true("algorithmName" %in% colnames(performances))
    expect_false(any(is.na(performances$algorithmName)))
    expect_false(any(trimws(performances$algorithmName) == ""))

    # set the performanceRowId()
    performanceRowId(1)

    # check each view loads selects tab
    session$setInputs(tabView = "View Models")
    session$flushReact()

    # add check to see whether tab changed
    #testthat::expect_true(input$resultTab == 'View Models')

    session$setInputs(tabView = "Generate Plots")
    #testthat::expect_true(input$resultTab == 'Generate Plots')

    session$setInputs(tabView = "View Threshold Performances")
    #testthat::expect_true(input$resultTab == 'View Threshold Performances')

    session$setInputs(tabView = "View Diagnostics")
    #testthat::expect_true(input$resultTab == 'View Diagnostics')


  })


test_that("normalizePredictionAlgorithmName fills missing names", {
  input <- data.frame(
    modelType = c("binary", "survival"),
    stringsAsFactors = FALSE
  )
  result <- normalizePredictionAlgorithmName(input)

  expect_true("algorithmName" %in% colnames(result))
  expect_identical(result$algorithmName, input$modelType)
})

test_that("normalizePredictionAlgorithmName fills blank algorithm names", {
  input <- data.frame(
    modelType = c("binary", "survival", "binary"),
    algorithmName = c(NA, "", "xgboost"),
    stringsAsFactors = FALSE
  )
  result <- normalizePredictionAlgorithmName(input)

  expect_identical(result$algorithmName, c("binary", "survival", "xgboost"))
})


test_that("Test prediction ui", {
  # Test ui
  ui <- patientLevelPredictionViewer()
  checkmate::expect_list(ui)
})
