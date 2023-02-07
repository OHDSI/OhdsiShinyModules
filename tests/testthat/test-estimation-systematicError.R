context("estimation-systematicError")

# issues with this modules!

shiny::testServer(
  app = estimationSystematicErrorServer, 
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
      outcome = 3,
      database = 1,
      analysis = 1
      )), 
    connectionHandler = connectionHandlerEst, 
    resultsSchema = 'main', 
    tablePrefix = 'cm_',
    #cohortTablePrefix = cohortTablePrefix, 
    #databaseTable = databaseTable
    metaAnalysisDbIds = 1
  ), 
  expr = {
    
    # check result table loads
    testthat::expect_true(is.null(systematicErrorPlot()))
    
    # select first row 
    selectedRow(
      data.frame(databaseId = '1', analysisId = 2, psStrategy = '', unblind = F,
                            targetSubjects = 100, comparatorSubjects = 100,
                            targetOutcomes = 10, comparatorOutcomes = 5,
                            targetDays = 1000, comparatorDays = 1000)
    )
    # setting selectedRow() activates the following
    ##testthat::expect_true(!is.null(systematicErrorPlot()))
    ##testthat::expect_true(!is.null(output$systematicErrorPlot))
    ##testthat::expect_true(!is.null(systematicErrorSummaryPlot()))
    ##testthat::expect_true(!is.null(output$systematicErrorSummaryPlot))
    
  })
