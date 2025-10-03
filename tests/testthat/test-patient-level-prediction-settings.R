context("patient-level-prediction-settings")


modelDesign <- OhdsiReportGenerator::getPredictionModelDesigns(
  connectionHandler = connectionHandlerCharacterization, 
  schema = resultDatabaseSettingsCharacterization$schema,
  plpTablePrefix = resultDatabaseSettingsCharacterization$plpTablePrefix, 
  cgTablePrefix = resultDatabaseSettingsCharacterization$cgTablePrefix,
  modelDesignIds = 1
  )
  
shiny::testServer(
  app = patientLevelPredictionSettingsServer, 
  args = list(
    modelDesign = shiny::reactive(modelDesign)
  ), 
  expr = {
    
    session$setInputs(showCohort  = T) 
    expect_true(!is.null(output$cohort))
    session$setInputs(showOutcome  = T) 
    session$setInputs(showRestrictPlpData  = T) 
    session$setInputs(showPopulation  = T) 
    session$setInputs(showCovariates  = T) 
    session$setInputs(showModel = T) 
    session$setInputs(showFeatureEngineering = T) 
    session$setInputs(showPreprocess = T) 
    session$setInputs(showSplit = T) 
    session$setInputs(showSample = T) 
    
  })


test_that("Test prediction Settings ui", {
  # Test ui
  ui <- patientLevelPredictionSettingsViewer(id = 'Settings')
  checkmate::expect_list(ui)
})
