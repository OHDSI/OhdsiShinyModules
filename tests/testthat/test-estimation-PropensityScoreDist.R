context("estimation-PropensityScoreDist")

shiny::testServer(
  app = estimationPropensityScoreDistServer, 
  args = list(
    selectedRow = shiny::reactiveVal(NULL
      #data.frame(databaseId = '1', analysisId = 2, psStrategy = '', unblind = F,
      #           targetSubjects = 100, comparatorSubjects = 100,
      #           targetOutcomes = 10, comparatorOutcomes = 5,
      #           targetDays = 1000, comparatorDays = 1000)
    ), 
    inputParams = shiny::reactiveVal(list(
      target = 1,
      comparator = 2, 
      outcome = 3
      )), 
    connectionHandler = connectionHandlerEst, 
    resultsSchema = 'main', 
    tablePrefix = 'cm_',
    cohortTablePrefix = resultDatabaseSettingsEst$cohortTablePrefix, 
    #databaseTable = databaseTable,
    metaAnalysisDbIds = NULL
  ), 
  expr = {
    
    testthat::expect_true(is.null(psDistPlot()))
    
    # make sure this runs if we pick the first row
    selectedRow(
      
      data.frame(databaseId = '1', analysisId = 2, psStrategy = '', unblind = F,
                 targetSubjects = 100, comparatorSubjects = 100,
                 targetOutcomes = 10, comparatorOutcomes = 5,
                 targetDays = 1000, comparatorDays = 1000)
      
    )
    
    testthat::expect_true(!is.null(psDistPlot()))
    
    
  })