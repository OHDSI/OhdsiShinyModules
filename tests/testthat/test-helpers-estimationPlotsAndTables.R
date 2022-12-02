context('tests-helpers-estimationPlotsAndTables')

test_that("Subgroup stuff", {
  
subgroupRes <- getEstimationSubgroupResults(
  connectionHandler = connectionHandlerEst, 
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

res <- prepareEstimationSubgroupTable(subgroupResults = subgroupRes, output = "latex")
testthat::expect_true(nrow(res) > 0)

})


test_that("CovariateBalance stuff", {
  
  balanceSummary <- getEstimationCovariateBalance(
    connectionHandler = connectionHandlerEst,
    resultsSchema = 'main',
    tablePrefix = resultDatabaseSettingsEst$tablePrefix,
    targetId = 1,
    comparatorId = 2,
    analysisId = 1,
    databaseId = '1',
    outcomeId = 3
  )
    
    testthat::expect_true(inherits(balanceSummary, 'data.frame'))
  
    # not the output of getEstimationCovariateBalance - where does it come from??
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
    
  resP <- plotEstimationCovariateBalanceSummary(
    balanceSummary = balanceSummary,
    threshold = 0,
    beforeLabel = "Before matching",
    afterLabel = "After matching"
  )
  
  testthat::expect_true(inherits(resP, 'gtable'))
  
})

test_that("nonZeroEstimationHazardRatio", {
  
  testthat::expect_equal(
    nonZeroEstimationHazardRatio(hrLower = 0.5, hrUpper = 0.5, terms = 'test'),
    'test'
  )
  testthat::expect_equal(
    nonZeroEstimationHazardRatio(hrLower = 1.1, hrUpper = 1.1, terms = c('test','sec')),
    'sec'
  )
  testthat::expect_equal(
    nonZeroEstimationHazardRatio(hrLower = 0.9, hrUpper = 1.1, terms = c('test','sec','3')),
    '3'
  )

})

test_that("goodEstimationPropensityScore", {
  testthat::expect_equal(goodEstimationPropensityScore(0.5), F)
  testthat::expect_equal(goodEstimationPropensityScore(1), F)
  testthat::expect_equal(goodEstimationPropensityScore(1.01), T)
})

test_that("goodEstimationSystematicBias", {
  testthat::expect_equal(goodEstimationSystematicBias(0.5), F)
  testthat::expect_equal(goodEstimationSystematicBias(1), F)
  testthat::expect_equal(goodEstimationSystematicBias(1.01), T)
})


test_that("plotEstimationForest", {
  
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
resP <- plotEstimationForest(results, limits = c(0.1, 10), metaAnalysisDbIds = 2)
testthat::expect_true(inherits(resP, 'gtable'))  

})

test_that("plotEstimationScatter", {
  
  
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
  resP <- plotEstimationScatter(controlResults)
  testthat::expect_true(inherits(resP, 'ggplot'))  
  
})

## Functions remaining to add tests:
# getEstimationTcoChoice
# getEstimationTargetChoices
# getEstimationComparatorChoices
# getEstimationOutcomeChoices
# getEstimationDatabaseChoices 
# getCmAnalysisOptions
# getAllEstimationResults
# getEstimationControlResults
# getEstimationStudyPeriod 
# getEstimationNegativeControlEstimates 
# getDiagnosticsData
