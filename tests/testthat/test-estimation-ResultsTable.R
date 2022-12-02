context("estimation-ResultsTable")

shiny::testServer(
  app = estimationResultsTableServer, 
  args = list(
    #selectedRow = shiny::reactiveVal(NULL
      #data.frame(databaseId = '1', analysisId = 2, psStrategy = '', unblind = F,
      #           targetSubjects = 100, comparatorSubjects = 100,
      #           targetOutcomes = 10, comparatorOutcomes = 5,
      #           targetDays = 1000, comparatorDays = 1000)
    #), 
    inputParams = shiny::reactiveVal(list(
      target = 1,
      comparator = 2, 
      outcome = 3,
      database = 1,
      analysis = 1
      )), 
    connectionHandler = connectionHandlerEst, 
    resultsSchema = 'main', 
    tablePrefix = 'cm_',
    #cohortTablePrefix = cohortTablePrefix, 
    databaseTable = resultDatabaseSettingsEst$databaseTable
    #metaAnalysisDbIds = NULL
  ), 
  expr = {
    
    # check result table loads
    testthat::expect_true(!is.null(resultSubset()))
    
    # select first row 
    session$setInputs(mainTable_rows_selected = 1)
    testthat::expect_true(!is.null(selectedRow())) # could check columns
    
    
  })