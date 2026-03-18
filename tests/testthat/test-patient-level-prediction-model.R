context("patient-level-prediction-model")

# extract performances
performances <- OhdsiReportGenerator::getFullPredictionPerformances(
  connectionHandler = connectionHandlerCharacterization,
  schema = resultDatabaseSettingsCharacterization$schema,
  plpTablePrefix = resultDatabaseSettingsCharacterization$plpTablePrefix,
  cgTablePrefix = resultDatabaseSettingsCharacterization$cgTablePrefix,
  databaseTable = resultDatabaseSettingsCharacterization$databaseTable,
  databaseTablePrefix = resultDatabaseSettingsCharacterization$databaseTablePrefix
) %>%
  normalizePredictionAlgorithmName() %>%
  dplyr::relocate("developmentTargetName") %>%
  dplyr::relocate("developmentOutcomeName", .after = "developmentTargetName") %>%
  dplyr::relocate("developmentTimeAtRisk", .after = "developmentOutcomeName") %>%
  dplyr::relocate("modelDesignId", .after = "developmentTimeAtRisk") %>%
  dplyr::arrange(.data$developmentTargetName, .data$developmentOutcomeName, .data$developmentTimeAtRisk)


shiny::testServer(
  app = patientLevelPredictionModelServer,
  args = list(
    performances = shiny::reactive({performances}),
    performanceRowIds = shiny::reactiveVal(1),
    connectionHandler = connectionHandlerCharacterization,
    resultDatabaseSettings = resultDatabaseSettingsCharacterization
  ),
  expr = {

    # check generate works for all model views
    i <- 0
    for (viewOption in c("Model Variable Importance", "Univariate Variable Importance", "Model Design", "Hyperparameters")) {
      i <- i + 1
      session$setInputs(
        view = viewOption,
        select = i
      )
      session$flushReact()
    }

    expect_false(is.null(hyperparameterSettings()))
    expect_true("algorithmName" %in% colnames(hyperparameterSettings()))
    expect_true(grepl("\\(.+\\)", hyperparameterSettings()$name[[1]]))

  })


test_that("hyperparameter selector falls back to modelType", {
  fallbackPerformances <- performances
  fallbackPerformances$algorithmName <- NA_character_

  shiny::testServer(
    app = patientLevelPredictionModelServer,
    args = list(
      performances = shiny::reactive({fallbackPerformances}),
      performanceRowIds = shiny::reactiveVal(1),
      connectionHandler = connectionHandlerCharacterization,
      resultDatabaseSettings = resultDatabaseSettingsCharacterization
    ),
    expr = {
      session$setInputs(
        view = "Hyperparameters",
        select = 1
      )
      session$flushReact()

      expect_false(is.null(hyperparameterSettings()))
      expect_identical(
        hyperparameterSettings()$algorithmName[[1]],
        hyperparameterSettings()$modelType[[1]]
      )
    }
  )
})


test_that("Test prediction model ui", {
  # Test ui
  ui <- patientLevelPredictionModelViewer(id = "models")
  checkmate::expect_list(ui)
})

test_that("predictionModelColumns", {
  # Test ui
  cols <- predictionModelColumns()
  testthat::expect_is(cols, "list")
})


# TODO add test for addValidationName
