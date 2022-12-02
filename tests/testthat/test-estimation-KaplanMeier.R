context("estimation-KaplanMeier")

shiny::testServer(
  app = estimationKaplanMeierServer, 
  args = list(
    selectedRow = shiny::reactiveVal(NULL), 
    inputParams = shiny::reactiveVal(list(
      target = 1,
      comparator = 2, 
      outcome = 3
      )), 
    connectionHandler = connectionHandlerEst, 
    resultsSchema = 'main', 
    tablePrefix = 'cm_',
    cohortTablePrefix = resultDatabaseSettingsEst$cohortTablePrefix, 
    databaseTable = resultDatabaseSettingsEst$databaseTable,
    metaAnalysisDbIds = '1'
  ), 
  expr = {
    
    # should start null
    testthat::expect_true(is.null(kaplanMeierPlot()))
    
    # make sure this runs if we pick the first row
    selectedRow(list(databaseId = '1', analysisId = 2, psStrategy = ''))
    testthat::expect_true(!is.null(kaplanMeierPlot()))
    
    testthat::expect_true(!is.null(output$kaplanMeierPlotPlotCaption))
    
  })