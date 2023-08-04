context("cohort-method-CovariateBalance")

shiny::testServer(
  app = cohortMethodCovariateBalanceServer, 
  args = list(
    selectedRow = shiny::reactiveVal(NULL), 
    connectionHandler = connectionHandlerCm, 
    resultDatabaseSettings = resultDatabaseSettingsCm
  ), 
  expr = {
    
    # should start null
    testthat::expect_true(is.null(balance()))
    
    # make sure this runs if we pick the first row
    selectedRow(
      list(
        databaseId = '1', 
        cdmSourceAbbreviation = 'Eunomia', 
        analysisId = 2,
        description  = 'madeup',
        target = 'test target',
        targetId = 1,
        comparatorId = 2, 
        comparator = 'test comparator',
        outcomeId = 3,
        outcome = 'test outcome',
        psStrategy = ''
      )
    )
    
    testthat::expect_true(!is.null(output$balanceSummaryPlot))
    testthat::expect_true(!is.null(balance()))
    testthat::expect_true(nrow(balance())>0)
    
    testthat::expect_true(!is.null(output$balancePlotCaption))
    testthat::expect_true(!is.null(output$balanceSummaryPlotCaption))
    
    # check textsearch 
    textSearchCohortMethod('heart')
    
    
    balance <- getCohortMethodCovariateBalanceShared(
      connectionHandler = connectionHandlerCm,
      resultDatabaseSettings = resultDatabaseSettingsCm,
      targetId = 1,
      comparatorId = 2,
      databaseId = '1',
      analysisId = 2)
    
    testthat::expect_true(!is.null(balance))
    testthat::expect_true(nrow(balance)>0)
    
    plot <- plotCohortMethodCovariateBalanceScatterPlotNew(
      balance = balance,
      beforeLabel = "Before propensity score adjustment",
      afterLabel = "After propensity score adjustment"
    )
    
    testthat::expect_is(object = plot, class = 'plotly')
    
    
  })



test_that("plotCohortMethodCovariateBalanceSummary", {
  
  # not the output of getEstimationCovariateBalance - where does it come from??
  balance <- data.frame(
    databaseId = rep(1,2), 
    #covariateId = 1,
    #covariateName = '1',              
    #analysisId = 1,
    #beforeMatchingMeanTreated = 1,
    #beforeMatchingMeanComparator = 1,
    #beforeMatchingStdDiff = 0,
    #afterMatchingMeanTreated = 1,
    #afterMatchingMeanComparator = 1,
    #afterMatchingStdDiff = 0,
    absBeforeMatchingStdDiff = c(0.1,0.4),
    absAfterMatchingStdDiff = c(0.1,0.4),
    x = rep(1,2),
    ymin = rep(1,2),
    lower = rep(1,2),
    median = rep(1,2),
    upper = rep(1,2),
    ymax = rep(1,2),
    covariateCount = rep(1,2),
    type = c("Before matching","After matching")
  )
  
  # added test for this in covariatebal
  #resP <- plotEstimationCovariateBalanceScatterPlotNew(
  #  balance = balance,
  #  beforeLabel = "Before matching",
  #  afterLabel = "After matching",
  #  textsearch = shiny::reactiveVal(NULL)
  #)
  #testthat::expect_true(inherits(resP, 'plotly'))
  
  balanceSummary <- data.frame(
    databaseId = rep(1,2), 
    #covariateId = 1,
    #covariateName = '1',              
    #analysisId = 1,
    #beforeMatchingMeanTreated = 1,
    #beforeMatchingMeanComparator = 1,
    #beforeMatchingStdDiff = 0,
    #afterMatchingMeanTreated = 1,
    #afterMatchingMeanComparator = 1,
    #afterMatchingStdDiff = 0,
    x = rep(1,2),
    ymin = rep(1,2),
    lower = rep(1,2),
    median = rep(1,2),
    upper = rep(1,2),
    ymax = rep(1,2),
    covariateCount = rep(1,2),
    type = c("Before matching","After matching")
  )
  
  resP <- plotCohortMethodCovariateBalanceSummary(
    balanceSummary = balanceSummary,
    threshold = 0,
    beforeLabel = "Before matching",
    afterLabel = "After matching"
  )
  
  testthat::expect_true(inherits(resP, 'gtable'))
  
})