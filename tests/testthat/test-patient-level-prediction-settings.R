context("patient-level-prediction-settings")

shiny::testServer(
  app = patientLevelPredictionSettingsServer, 
  args = list(
    modelDesignId = shiny::reactiveVal(NULL),
    developmentDatabaseId = shiny::reactiveVal(1),
    performanceId = shiny::reactiveVal(1),
    connectionHandler = connectionHandlerPlp,
    inputSingleView = shiny::reactiveVal('Design Settings'), # only works with this
    resultDatabaseSettings = resultDatabaseSettingsPlp
  ), 
  expr = {
    
    modelDesignId(1)
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
    session$setInputs(showHyperparameters = T) 
    
    design <- getPredictionModelDesign(
      inputSingleView = inputSingleView,
      modelDesignId = modelDesignId,
      connectionHandler = connectionHandler,
      resultDatabaseSettings = resultDatabaseSettings
    )
    expect_true(class(design) == 'list')
    expect_true(!is.null(design$RestrictPlpData))
    
    # check reactive?
    
  })
