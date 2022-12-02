context("estimation-PopulationCharacteristics")

shiny::testServer(
  app = estimationPopulationCharacteristicsServer, 
  args = list(
    selectedRow = shiny::reactiveVal(NULL), 
    inputParams = shiny::reactiveVal(list(
      target = 1,
      comparator = 2, 
      outcome = 3
      )), 
    connectionHandler = connectionHandlerEst, 
    resultsSchema = 'main', 
    tablePrefix = 'cm_'
    #cohortTablePrefix = cohortTablePrefix, 
    #databaseTable = databaseTable,
    #metaAnalysisDbIds = '1'
  ), 
  expr = {
    
    # make sure this runs if we pick the first row
    selectedRow(list(databaseId = '1', analysisId = 2, psStrategy = ''))
    testthat::expect_true(!is.null(output$table1Table))
    
    testthat::expect_true(!is.null(output$table1Caption))
    
  })