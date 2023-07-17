context("patient-level-prediction-modelSummary")

shiny::testServer(
  app = patientLevelPredictionModelSummaryServer, 
  args = list(
    connectionHandler = connectionHandlerPlp,
    modelDesignId = shiny::reactiveVal(1),
    resultDatabaseSettings = resultDatabaseSettingsPlp
  ), 
  expr = {
    
    expect_true(nrow(resultTable())>0)
    # check reactives are null untill input set
    expect_true(is.null(performanceId()))
    expect_true(is.null(developmentDatabaseId()))
    
    #session$setInputs(view_details = list(index = 1))
    #expect_true(!is.null(performanceId()))
    #expect_true(!is.null(developmentDatabaseId()))
    
  })
