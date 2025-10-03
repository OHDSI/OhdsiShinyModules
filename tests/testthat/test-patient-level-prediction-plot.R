context("patient-level-prediction-plot")

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
  app = patientLevelPredictionPlotServer, 
  args = list(
    performances = shiny::reactive({performances}),
    performanceRowIds = shiny::reactiveVal(1),
    connectionHandler = connectionHandlerCharacterization,
    resultDatabaseSettings = resultDatabaseSettingsCharacterization
  ), 
  expr = {
    
    # check generate works for all plots
    for(i in 1:length(plotList)){
      session$setInputs(
        plotType = plotList[i],
        generatePlot = i
      )
      expect_true(!is.null(output$plot))
    }

  })


test_that("Test prediction plot ui", {
  # Test ui
  ui <- patientLevelPredictionPlotViewer(id = 'plots')
  checkmate::expect_list(ui)
})