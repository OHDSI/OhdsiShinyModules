context("estimation-CovariateBalance")

shiny::testServer(
  app = estimationCovariateBalanceServer, 
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
    metaAnalysisDbIds = '1'
  ), 
  expr = {
    
    # should start null
    testthat::expect_true(is.null(balance()))
    
    # make sure this runs if we pick the first row
    selectedRow(list(databaseId = '1', analysisId = 2, psStrategy = ''))
    testthat::expect_true(!is.null(balance()))
    testthat::expect_true(nrow(balance())>0)
    
    testthat::expect_true(!is.null(balancePlot()))
    
    testthat::expect_true(!is.null(output$balancePlotCaption))
    #session$setInputs(plotHoverBalanceScatter = list(
    #  x = balance()$absBeforeMatchingStdDiff[1], 
    #  y = balance()$absAfterMatchingStdDiff[1],
    #  domain = list(left = 0.9, right = 1, top = 3, bottom = 0),
    #  range = list(left = 3, right = 5, top = 3, bottom = 0)
    #  ) 
    #)
    #testthat::expect_true(!is.null(output$hoverInfoBalanceScatter))
    ##testthat::expect_true(!is.null(balanceSummaryPlot())) - doesnt work
    testthat::expect_true(!is.null(output$balanceSummaryPlotCaption))
    
    
    
  })