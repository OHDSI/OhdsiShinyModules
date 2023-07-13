context('tests-helpers-cohort-methodPlotsAndTables')

test_that("Subgroup stuff", {
  
subgroupRes <- getCohortMethodSubgroupResults(
  connectionHandler = connectionHandlerCm, 
  targetIds = 1, 
  comparatorIds = 2, 
  outcomeIds = 3, 
  databaseIds = 1, 
  analysisIds = 1, 
  subgroupIds = 372328212,
  cmInteractionResult = data.frame(
    targetId = 1, 
    comparatorId = 2, 
    outcomeId = 3,
    databaseId = 1,
    analysisId = 1,
    interactionCovariateId = 372328212,
    targetSubjects = 10,
    comparatorSubjects =10,
    rrr = 1,
    ci95Lb = 1,
    ci95Ub = 1,
    p = 1,
    calibratedP = 1
    ),
  covariate = list(
    covariateId = 372328212,
    covariateName = 'test',
    databaseId = 1
  )
  )

testthat::expect_true(nrow(subgroupRes) > 0)

res <- prepareCohortMethodSubgroupTable(subgroupResults = subgroupRes, output = "latex")
testthat::expect_true(nrow(res) > 0)

})


test_that("CovariateBalance stuff", {
  
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

test_that("nonZeroCohortMethodHazardRatio", {
  
  testthat::expect_equal(
    nonZeroCohortMethodHazardRatio(hrLower = 0.5, hrUpper = 0.5, terms = 'test'),
    'test'
  )
  testthat::expect_equal(
    nonZeroCohortMethodHazardRatio(hrLower = 1.1, hrUpper = 1.1, terms = c('test','sec')),
    'sec'
  )
  testthat::expect_equal(
    nonZeroCohortMethodHazardRatio(hrLower = 0.9, hrUpper = 1.1, terms = c('test','sec','3')),
    '3'
  )

})

test_that("goodCohortMethodPropensityScore", {
  testthat::expect_equal(goodCohortMethodPropensityScore(0.5), F)
  testthat::expect_equal(goodCohortMethodPropensityScore(1), F)
  testthat::expect_equal(goodCohortMethodPropensityScore(1.01), T)
})

test_that("goodCohortMethodSystematicBias", {
  testthat::expect_equal(goodCohortMethodSystematicBias(0.5), F)
  testthat::expect_equal(goodCohortMethodSystematicBias(1), F)
  testthat::expect_equal(goodCohortMethodSystematicBias(1.01), T)
})


test_that("plotCohortMethodForest", {
  
  results <- data.frame(
    databaseId = 1:10,
    seLogRr = runif(10),
    logRr = runif(10),
    ci95Lb = runif(10),
    ci95Ub = runif(10),
    calibratedLogRr = runif(10),
    calibratedCi95Lb = runif(10),
    calibratedCi95Ub = runif(10),
    i2 = runif(10)
  )
resP <- plotCohortMethodForest(results, limits = c(0.1, 10), metaAnalysisDbIds = 2)
testthat::expect_true(inherits(resP, 'gtable'))  

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

## Functions remaining to add tests:
# getCohortMethodTcoChoice
# getCohortMethodTargetChoices
# getCohortMethodComparatorChoices
# getCohortMethodOutcomeChoices
# getCohortMethodDatabaseChoices 
# getCmAnalysisOptions
# getAllCohortMethodResults
# getCohortMethodControlResults
# getCohortMethodStudyPeriod 
# getCohortMethodNegativeControlEstimates 
