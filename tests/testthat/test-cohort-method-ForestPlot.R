context("cohort-method-ForestPlot")

shiny::testServer(
  app = cohortMethodForestPlotServer, 
  args = list(
    selectedRow = shiny::reactiveVal(NULL), 
    inputParams = shiny::reactiveVal(list(
      target = 1,
      comparator = 2, 
      outcome = 3
      )), 
    connectionHandler = connectionHandlerCm, 
    resultsSchema = 'main', 
    tablePrefix = 'cm_',
    metaAnalysisDbIds = NULL,
    databaseTable = resultDatabaseSettingsCm$databaseTable
  ), 
  expr = {
    
    # doesnt seem to be currently used?
    testthat::expect_true(is.null(forestPlot()))
    
  })
