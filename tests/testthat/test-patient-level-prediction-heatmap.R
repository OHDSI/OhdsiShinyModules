context("patient-level-prediction-heatmap")

# extract performances
performances <- OhdsiReportGenerator::getFullPredictionPerformances(
  connectionHandler = connectionHandlerCharacterization, 
  schema = resultDatabaseSettingsCharacterization$schema,
  plpTablePrefix = resultDatabaseSettingsCharacterization$plpTablePrefix, 
  cgTablePrefix = resultDatabaseSettingsCharacterization$cgTablePrefix,
  databaseTable = resultDatabaseSettingsCharacterization$databaseTable, 
  databaseTablePrefix = resultDatabaseSettingsCharacterization$databaseTablePrefix
) %>%
  dplyr::relocate("developmentTargetName") %>%
  dplyr::relocate("developmentOutcomeName", .after = "developmentTargetName") %>%
  dplyr::relocate("developmentTimeAtRisk", .after = "developmentOutcomeName") %>%
  dplyr::relocate("modelDesignId", .after = "developmentTimeAtRisk") %>%
  dplyr::arrange(.data$developmentTargetName, .data$developmentOutcomeName, .data$developmentTimeAtRisk)


shiny::testServer(
  app = patientLevelPredictionHeatmapServer, 
  args = list(
    performances = shiny::reactive({performances}),
    performanceRowIds = shiny::reactiveVal(1),
    connectionHandler = connectionHandlerCharacterization,
    resultDatabaseSettings = resultDatabaseSettingsCharacterization
  ), 
  expr = {
    
    session$setInputs(
      xAxis = 'developmentTargetName',
      yAxis = 'evaluation',
      metric = 'AUROC'
      )
    session$setInputs(generateHeatmap = 1)
    
    testthat::expect_true(!is.null(output$heatmap))

  })


test_that("Test heatmap prediction plot ui", {
  # Test ui
  ui <- patientLevelPredictionHeatmapViewer(id = 'plots')
  checkmate::expect_list(ui)
})