context("prediction-valdiation")

shiny::testServer(
  app = predictionValidationServer, 
  args = list(
    modelDesignId = shiny::reactiveVal(1),
    developmentDatabaseId = shiny::reactiveVal(1),
    performanceId = shiny::reactiveVal(1),
    con = connection,
    inputSingleView = shiny::reactiveVal('Validation'),
    mySchema = mySchemaTest,
    targetDialect = targetDialectTest,
    myTableAppend = myTableAppendTest
    ), 
  expr = {
  
  expect_true(nrow(validationTable()) > 0)
    
  # no rows selected so valResult() should empty list
  expect_true(length(valResult()$thresholdSummaryList) == 0) 
    
  session$setInputs(validationTable_rows_selected = 1)  
  expect_true(length(valResult()$thresholdSummaryList) > 0)  
    
})