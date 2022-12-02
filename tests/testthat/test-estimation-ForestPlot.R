context("estimation-ForestPlot")

shiny::testServer(
  app = estimationForestPlotServer, 
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
    metaAnalysisDbIds = NULL,
    databaseTable = resultDatabaseSettingsEst$databaseTable
  ), 
  expr = {
    
    # doesnt seem to be currently used?
    testthat::expect_true(is.null(forestPlot()))
    
  })