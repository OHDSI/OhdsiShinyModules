context("prediction-settings")

shiny::testServer(
  app = predictionSettingsServer, 
  args = list(
    modelDesignId = shiny::reactiveVal(NULL),
    developmentDatabaseId = shiny::reactiveVal(1),
    performanceId = shiny::reactiveVal(1),
    connectionHandler = connectionHandlerPlp,
    inputSingleView = shiny::reactiveVal('Design Settings'), # only works with this
    mySchema = resultDatabaseSettingsPlp$schema,
    myTableAppend = resultDatabaseSettingsPlp$tablePrefix
  ), 
  expr = {
    
    modelDesignId(1)
    session$setInputs(showAttrition  = T) 
    expect_true(!is.null(output$attrition))
    
    design <- getModelDesign(
      modelDesignId = modelDesignId,
      mySchema = mySchema, 
      connectionHandler = connectionHandler,
      myTableAppend = myTableAppend, 
      cohortTableAppend = ''  # add as input?
    )
    expect_true(class(design) == 'list')
    expect_true(!is.null(design$RestrictPlpData))
    
    # check reactive?
    
  })