context("patient-level-prediction-cutoff")

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
  app = patientLevelPredictionCutoffServer, 
  args = list(
    performances = shiny::reactive({performances}),
    performanceRowIds = shiny::reactiveVal(0),
    connectionHandler = connectionHandlerCharacterization,
    resultDatabaseSettings = resultDatabaseSettingsCharacterization
  ), 
  expr = {
    
    # check initial settings
    testthat::expect_null(thresholdSummary())
    testthat::expect_null(probabilities())
    
    # check this triggers observe event
    performanceRowIds(1)
    session$flushReact()
    
    #check results
    res <- OhdsiReportGenerator::getPredictionPerformanceTable(
      connectionHandler = connectionHandler, 
      schema = resultDatabaseSettings$schema, 
      plpTablePrefix = resultDatabaseSettings$plpTablePrefix, 
      databaseTable = resultDatabaseSettings$databaseTable, 
      table = 'threshold_summary', 
      performanceIds = performances()$performanceId[performanceRowIds()]
    )
    
    if(!is.null(res)){
      # check values update when performance is set
      testthat::expect_is(thresholdSummary(), 'data.frame')
      testthat::expect_true(nrow(thresholdSummary()) > 0 )
    }
  })


test_that("Test prediction Cutoff ui", {
  # Test ui
  ui <- patientLevelPredictionCutoffViewer(id = 'Cutoff')
  checkmate::expect_list(ui)
})



# TODO add test for processedThresholds(data,probability,performances)

