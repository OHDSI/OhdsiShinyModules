context("cohort-method-ResultsTable")

shiny::testServer(
  app = cohortMethodResultsTableServer, 
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
    connectionHandler = connectionHandlerCm, 
    resultDatabaseSettings = resultDatabaseSettingsCm
  ), 
  expr = {
    
    # check result table loads
    testthat::expect_true(!is.null(resultSubset()))
    
    # select first row 
    testthat::expect_true(is.null(selectedRow()))
    #reactable::updateReactable(
    #  outputId = "mainTable", 
    #  selected = 1, 
    #  session = session
    #  )
    session$setInputs(mainTable__reactable__selected = 1)
    #session$setInputs(mainTable_rows_selected = 1)
    testthat::expect_true(!is.null(selectedRow())) # could check columns
    
    
  })
