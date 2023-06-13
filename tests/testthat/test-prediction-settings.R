context("prediction-settings")

shiny::testServer(
  app = predictionSettingsServer, 
  args = list(
    modelDesignId = shiny::reactiveVal(NULL),
    developmentDatabaseId = shiny::reactiveVal(1),
    performanceId = shiny::reactiveVal(1),
    connectionHandler = connectionHandlerPlp,
    inputSingleView = shiny::reactiveVal('Design Settings'), # only works with this
    schema = resultDatabaseSettingsPlp$schema,
    plpTablePrefix = resultDatabaseSettingsPlp$plpTablePrefix
  ), 
  expr = {
    
    modelDesignId(1)
    session$setInputs(showAttrition  = T) 
    expect_true(!is.null(output$attrition))
    session$setInputs(showCohort  = T) 
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
    
    design <- getModelDesign(
      inputSingleView = inputSingleView,
      modelDesignId = modelDesignId,
      schema = schema, 
      connectionHandler = connectionHandler,
      plpTablePrefix = plpTablePrefix, 
      cohortTablePrefix = ''  # add as input?
    )
    expect_true(class(design) == 'list')
    expect_true(!is.null(design$RestrictPlpData))
    
    # check reactive?
    
  })
