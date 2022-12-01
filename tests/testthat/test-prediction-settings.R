context("prediction-settings")

shiny::testServer(
  app = predictionSettingsServer, 
  args = list(
    modelDesignId = shiny::reactiveVal(NULL),
    developmentDatabaseId = shiny::reactiveVal(1),
    performanceId = shiny::reactiveVal(1),
    con = connectionPlp,
    inputSingleView = shiny::reactiveVal('Design Settings'), # only works with this
    mySchema = schemaTest,
    targetDialect = dbmsTest,
    myTableAppend = tablePrefixTest
  ), 
  expr = {
    
    modelDesignId(1)
    session$setInputs(showAttrition  = T) 
    expect_true(!is.null(output$attrition))
    
    design <- getModelDesign(
      modelDesignId = modelDesignId,
      mySchema = schemaTest, 
      con = connectionPlp,
      myTableAppend = tablePrefixTest, 
      targetDialect = dbmsTest,
      cohortTableAppend = ''
    )
    expect_true(class(design) == 'list')
    expect_true(!is.null(design$RestrictPlpData))
    
    # check reactive?
    
  })