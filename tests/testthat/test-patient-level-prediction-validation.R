context("patient-level-prediction-valdiation")

shiny::testServer(
  app = patientLevelPredictionValidationServer, 
  args = list(
    modelDesignId = shiny::reactiveVal(1),
    developmentDatabaseId = shiny::reactiveVal(1),
    performanceId = shiny::reactiveVal(1),
    connectionHandler = connectionHandlerPlp,
    inputSingleView = shiny::reactiveVal('No Validation'),
    resultDatabaseSettings = resultDatabaseSettingsPlp
    ), 
  expr = {
    
  expect_true(is.null(validationTable()))
    
    # after setting the inputSingleView the table should load
  inputSingleView('Validation')
  
  expect_true(!is.null(validationTable()))
  expect_true(nrow(validationTable()) > 0)
    
  # no rows selected so valResult() should empty list
  expect_true(length(valResult()$thresholdSummaryList) == 0) 
    
  session$setInputs(validationTable_rows_selected = 1)  
  expect_true(length(valResult()$thresholdSummaryList) > 0)  
    
})
