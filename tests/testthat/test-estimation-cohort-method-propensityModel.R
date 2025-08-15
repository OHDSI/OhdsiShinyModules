context("cohort-method-propensityModel")

shiny::testServer(
  app = cohortMethodPropensityModelServer, 
  args = list(
    selectedRow = shiny::reactiveVal(NULL), 
    connectionHandler = connectionHandlerEstimation, 
    resultDatabaseSettings = resultDatabaseSettingsEstimation
  ), 
  expr = {
    
    #testthat::expect_true(is.null(output$powerTable))
    
    # make sure this runs if we pick the first row
    selectedRow(
      list(
        databaseId = 'eunomia', 
        databaseName = 'Eunomia', 
        description  = 'madeup',
        targetName = 'test target',
        targetId = 1,
        comparatorId = 2, 
        comparatorName = 'test comparator',
        outcomeId = 3,
        outcomeName = 'test outcome',
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
    testthat::expect_true(!is.null(data()))
    
    
  })
