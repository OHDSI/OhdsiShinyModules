context("cohort-method-PropensityScoreDist")

shiny::testServer(
  app = cohortMethodPropensityScoreDistServer, 
  args = list(
    selectedRow = shiny::reactiveVal(NULL), 
    connectionHandler = connectionHandlerEstimation, 
    resultDatabaseSettings = resultDatabaseSettingsEstimation
  ), 
  expr = {
    
    testthat::expect_true(is.null(psDistPlot()))
    
    ps <- getCohortMethodPs(
      connectionHandler = connectionHandlerEstimation, 
      resultDatabaseSettings = resultDatabaseSettingsEstimation,
      targetId = 1, 
      comparatorId = 2, 
      analysisId = 2, 
      databaseId = 'eunomia'
    )
    
    testthat::expect_true('preferenceScore' %in% colnames(ps))
    testthat::expect_true('targetDensity' %in% colnames(ps))
    testthat::expect_true('comparatorDensity' %in% colnames(ps))
    
    # make sure this runs if we pick the first row
    selectedRow(
      list(
        databaseId = 'eunomia', 
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
        unblind = 0,
        targetSubjects = 100, 
        comparatorSubjects = 100,
        targetOutcomes = 10, 
        comparatorOutcomes = 5,
        targetDays = 1000, 
        comparatorDays = 1000
      )
    )
    
    testthat::expect_true(!is.null(psDistPlot()))
    
    
  })
