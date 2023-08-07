context("cohort-method-Power")

shiny::testServer(
  app = cohortMethodPowerServer, 
  args = list(
    selectedRow = shiny::reactiveVal(NULL), 
    connectionHandler = connectionHandlerCm, 
    resultDatabaseSettings = resultDatabaseSettingsCm
  ), 
  expr = {
    
    #testthat::expect_true(is.null(output$powerTable))
    
    followUp <- getCmFollowUpDist(
      connectionHandler = connectionHandlerCm,
      resultDatabaseSettings = resultDatabaseSettingsCm,
      targetId = 1,
      comparatorId = 2,
      outcomeId = 3,
      databaseId = '1',
      analysisId = 2
    )
    testthat::expect_true(nrow(followUp)>0)
    
    tablet <- prepareCohortMethodFollowUpDistTable(followUp)
    testthat::expect_true(nrow(tablet)>0)
    
    # make sure this runs if we pick the first row
    selectedRow(
      data.frame(
        databaseId = '1', 
        cdmSourceAbbreviation = 'Eunomia', 
        description  = 'madeup',
        target = 'test target',
        targetId = 1,
        comparatorId = 2, 
        comparator = 'test comparator',
        outcomeId = 3,
        outcome = 'test outcome',
        psStrategy = '',
        analysisId = 2, 
        psStrategy = '', 
        unblind = F,
        targetSubjects = 100, 
        comparatorSubjects = 100,
        targetOutcomes = 10, 
        comparatorOutcomes = 5,
        targetDays = 1000, 
        comparatorDays = 1000
      )
    )
    testthat::expect_true(!is.null(output$powerTable))
    testthat::expect_true(!is.null(output$powerTableCaption))
    testthat::expect_true(!is.null(output$timeAtRiskTableCaption))
    testthat::expect_true(!is.null(output$timeAtRiskTable))
    
    
  })
