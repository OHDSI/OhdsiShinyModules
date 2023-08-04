context("cohort-method-systematicError")

# issues with this modules!

shiny::testServer(
  app = cohortMethodSystematicErrorServer, 
  args = list(
    selectedRow = shiny::reactiveVal(NULL), 
    connectionHandler = connectionHandlerCm, 
    resultDatabaseSettings = resultDatabaseSettingsCm
  ), 
  expr = {
    
    # check result table loads
    testthat::expect_true(is.null(systematicErrorPlot()))
    
    # select first row 
    selectedRow(
      list(
        databaseId = '1', 
        cdmSourceAbbreviation = 'Eunomia', 
        description  = 'madeup',
        target = 'test target',
        targetId = 1,
        comparatorId = 2, 
        comparator = 'test comparator',
        outcomeId = 3,
        outcome = 'test outcome',
        psStrategy = '',
        analysisId = 2, 
        psStrategy = '', 
        unblind = 0,
        targetSubjects = 100, 
        comparatorSubjects = 100,
        targetOutcomes = 10, 
        comparatorOutcomes = 5,
        targetDays = 1000, 
        comparatorDays = 1000
      )
    )
    # setting selectedRow() activates the following
    ##testthat::expect_true(!is.null(systematicErrorPlot()))
    testthat::expect_true(!is.null(output$systematicErrorPlot))
    ##testthat::expect_true(!is.null(systematicErrorSummaryPlot()))
    ##testthat::expect_true(!is.null(output$systematicErrorSummaryPlot))
    
  })



test_that("plotCohortMethodScatter", {
  
  
  controlResults <- data.frame(
    databaseId = 1:10,
    seLogRr = runif(10),
    logRr = runif(10),
    ci95Lb = runif(10),
    ci95Ub = runif(10),
    effectSize = runif(10),
    calibratedLogRr = runif(10),
    calibratedSeLogRr = runif(10),
    calibratedCi95Lb = runif(10),
    calibratedCi95Ub = runif(10),
    trueRr = rep(1,10)
    
  )
  resP <- plotCohortMethodScatter(controlResults)
  testthat::expect_true(inherits(resP, 'ggplot'))  
  
})
