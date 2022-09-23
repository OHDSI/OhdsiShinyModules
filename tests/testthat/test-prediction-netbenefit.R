context("prediction-netbenefit")

shiny::testServer(
  app = predictionNbServer, 
  args = list(
    performanceId = shiny::reactiveVal(NULL),
    con = connectionPlp,
    inputSingleView = shiny::reactiveVal("Discrimination"),
    mySchema = schemaTest,
    targetDialect = dbmsTest,
    myTableAppend = tablePrefixTest
  ), 
  expr = {
    
    expect_true(is.null(thresholdSummary()))
    
    performanceId(1)
    inputSingleView('Net Benefit')
    expect_true(!is.null(thresholdSummary()))
    
    # check no errors
    session$setInputs(nbSelectInput = 'CV')
    session$setInputs(nbSelectInput = 'Test')
    session$setInputs(nbSelectInput = 'Train')

  })