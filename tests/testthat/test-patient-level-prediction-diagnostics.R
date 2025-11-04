context("patient-level-prediction-diagnostics")

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
  app = patientLevelPredictionDiagnosticsServer, 
  args = list(
    performances = shiny::reactive({performances}),
    performanceRowIds = shiny::reactiveVal(0),
    connectionHandler = connectionHandlerCharacterization,
    resultDatabaseSettings = resultDatabaseSettingsCharacterization
  ), 
  expr = {
    
    # check initial settings
    testthat::expect_null(diagnosticTable())
    testthat::expect_null(colDef())
    
    # check this triggers observe event
    performanceRowIds(1)
    session$flushReact()
    
    # set the inputs
    session$setInputs(selectedModels  = unique(performances()$modelDesignId))
    session$setInputs(generate  = 1)
    
    #check results
    diag <- OhdsiReportGenerator::getPredictionDiagnostics(
      connectionHandler = connectionHandler, 
      schema = resultDatabaseSettings$schema, 
      plpTablePrefix = resultDatabaseSettings$plpTablePrefix, 
      cgTablePrefix = resultDatabaseSettings$cgTablePrefix, 
      databaseTable = resultDatabaseSettings$databaseTable, 
      modelDesignIds = unique(performances()$modelDesignId[performanceRowIds()])
    )
    
    if(!is.null(diag)){
      # check values update when performance is set
      testthat::expect_is(diagnosticTable(), 'data.frame')
      testthat::expect_true(nrow(diagnosticTable()) > 0 )
      testthat::expect_true(length(colDef()) > 0)
    }
  })


test_that("Test prediction diagnostics ui", {
  # Test ui
  ui <- patientLevelPredictionDiagnosticsViewer(id = 'diagnostics')
  checkmate::expect_list(ui)
})