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
    
    designInfo <- getModelDesignInfo(
      connectionHandler = connectionHandler, 
      resultDatabaseSettings = resultDatabaseSettings,
      modelDesignId = modelDesignId
    )
    expect_true(inherits(designInfo, 'data.frame'))
    
    performanceSum <- getModelDesignPerformanceSummary(
      connectionHandler = connectionHandler, 
      resultDatabaseSettings = resultDatabaseSettings,
      modelDesignId = modelDesignId
    )
    expect_true(inherits(performanceSum, 'data.frame'))
    
    attr <- getAttrition(
      performanceId = 1,
      connectionHandler = connectionHandler,
      resultDatabaseSettings = resultDatabaseSettings
    )
    expect_true(inherits(attr, 'data.frame'))
    
  })
