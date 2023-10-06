context("helpers-sccsPlots")

test_that("convert to dates", {
  testthat::expect_equal(as.character(convertToStartDate(2020,3)), "2020-03-01")
  testthat::expect_equal(as.character(convertToEndDate(2020,11)), "2020-11-30")
  testthat::expect_equal(as.character(convertToEndDate(2020,12)), "2020-12-31")
})

# Note - this is the old plot
test_that("plotTimeTrendStability", {
  df <- data.frame(
    calendarYear = c(2011,2012),
    calendarMonth = c(1,1),
    outcomeRate = runif(2),
    observedSubjects = rep(100,2),
    adjustedRate = runif(2),
    stable = rep(1,2)
  )
  res <- plotTimeTrendStability(df)
  testthat::expect_is(res, "ggplot")
})
# New plot
test_that("plotTimeTrend", {
  df <- data.frame(
    calendarYear = c(2011,2012),
    calendarMonth = c(1,1),
    ratio = runif(2),
    observedSubjects = rep(100,2),
    adjustedRatio = runif(2)
  )
  res <- plotTimeTrend(df)
  testthat::expect_is(res, "ggplot")
})



test_that("plotTimeToEventSccs", {
  
  df <- data.frame(
    week = rep(1:10),
    outcomes = sample(100,10),
    observedSubjects = rep(1000,10)
  )

 res <- plotTimeToEventSccs(timeToEvent = df)
 testthat::expect_is(res, "ggplot")
})


test_that("drawAttritionDiagram", {
  
  df <- data.frame(
    description = paste('example',1:5),
    outcomeSubjects = c(1000,900,800,700,500),
    outcomeEvents = c(100,80,70,50,20)
  )
  
  res <- drawAttritionDiagram(attrition = df)
  testthat::expect_is(res, "ggplot")
})


test_that("plotEventDepObservation", {
  
  df <- data.frame(
    monthsToEnd = sample(12, 5),
    outcomes = sample(100,5),
    censored = rep(0,5)
  )
  
  res <- plotEventDepObservation(df, maxMonths = 12)
  testthat::expect_is(res, "ggplot")
})

test_that("plotSpanning", {
  
  df <- data.frame(
    ageMonth = sample(12,5),
    calendarYear = rep(2010,5),
    calendarMonth = sample(12,5),
    coverBeforeAfterSubjects = rep(1000,5)
  )
  
  res <- plotSpanning(df, type = 'age')
  testthat::expect_is(res, "ggplot")
  
  res <- plotSpanning(df, type = 'noage')
  testthat::expect_is(res, "ggplot")
})


test_that("plotAgeSpline", {
  
  df <- data.frame(
    rr = 0.1+runif(100),
    ageMonth = sample(12,100, replace = T)
  )
  
  res <- suppressWarnings(plotAgeSpline(df))
  testthat::expect_is(res, "ggplot")
})


test_that("plotSeasonSpline", {
  
  df <- data.frame(
    rr = 0.1+runif(100),
    knotMonth = sample(12,100, replace = T)
  )
  
  res <- plotSeasonSpline(df)
  testthat::expect_is(res, "ggplot")
})

#plotCalendarTimeSpline - missing fucntion?
# cyclicSplineDesign - what knots?

test_that("plotControlEstimates", {
  
  df <- data.frame(
    logRr = 0.1+runif(100),
    seLogRr = rep(0.01, 100),
    ci95Lb = rep(0,100),
    ci95Ub = rep(10,100),
    trueEffectSize = rep(1,100),
    calibratedLogRr = 0.1+runif(100),
    calibratedSeLogRr = rep(0.01, 100),
    calibratedCi95Lb = rep(0,100),
    calibratedCi95Ub = rep(10,100),
    trueEffectSize = rep(1,100)
  )
  
  res <- plotControlEstimates(df)
  testthat::expect_is(res, "ggplot")
})

test_that("renderDiagnosticsSummary", {
  
  df <- data.frame(
    mdrr = rep(1,3),
    timeTrendP = rep(1,3),
    preExposureP = rep(1,3),
    ease = rep(1,3),
    mdrrDiagnostic = rep('PASS',3),
    timeTrendDiagnostic = rep('PASS',3),
    preExposureDiagnostic = rep('PASS',3),
    easeDiagnostic = rep('PASS',3)
  )
  
  res <- renderDiagnosticsSummary(df[1,])
  testthat::expect_is(res, "data.frame")
})
