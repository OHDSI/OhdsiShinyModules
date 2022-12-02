context("estimation-attrition")

shiny::testServer(
  app = estimationAttritionServer, 
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
    databaseTable = resultDatabaseSettingsEst$databaseTable
  ), 
  expr = {
    
    # should start null
    testthat::expect_true(is.null(attritionPlot()))
    
    # make sure this runs if we pick the first row
    selectedRow(list(databaseId = 'Eunomia', analysisId = 1))
    testthat::expect_true(!is.null(attritionPlot()))
    
  })