context("prediction-settings")

shiny::testServer(
  app = predictionSetingsServer, 
  args = list(
    modelDesignId = shiny::reactiveVal(1),
    developmentDatabaseId = shiny::reactiveVal(1),
    performanceId = shiny::reactiveVal(1),
    con = connection,
    inputSingleView = shiny::reactiveVal('Design Settings'), # only works with this
    mySchema = mySchemaTest,
    targetDialect = targetDialectTest,
    myTableAppend = myTableAppendTest
  ), 
  expr = {
    
    session$setInputs(showAttrition  = T) 
    expect_true(!is.null(output$attrition))
    
    design <- getModelDesign(
      modelDesignId = modelDesignId,
      mySchema = mySchemaTest, 
      con = connection,
      myTableAppend = myTableAppendTest, 
      targetDialect = targetDialectTest  
    )
    expect_true(class(design) == 'list')
    expect_true(!is.null(design$RestrictPlpData))
    
    # check reactive?
    
  })