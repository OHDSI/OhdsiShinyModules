context("evidence-synth-main")

shiny::testServer(evidenceSynthesisServer, args = list(
  id = "testEvidenceSynthesisServer",
  connectionHandler = connectionHandlerES,
  resultDatabaseSettings = resultDatabaseSettingsES
), {
  
  expect_true(length(targetIds) > 0)
  expect_true(length(outcomeIds) > 0)
  
  #session$setInputs(
  # `input-selection-targetId` = 1, 
  # `input-selection-outcomeId` = 3, 
  # `input-selection-generate` = 1
  #)
  
  inputSelected(list(targetId = targetIds[1], outcomeId = 3))
  testthat::expect_true( nrow(unique(rbind(data(),data2()))) >0 )
  testthat::expect_equal(as.double(inputSelected()$outcomeId), 3)
  
})

test_that("Test es ui", {
  # Test ui
  ui <- evidenceSynthesisViewer()
  checkmate::expect_list(ui)
  checkmate::expect_file_exists(evidenceSynthesisHelperFile())
})

test_that("getCMEstimation", {
  
  res <- getCMEstimation(
    connectionHandler = connectionHandlerES,
    resultDatabaseSettings = resultDatabaseSettingsES,
    targetId = 1,
    outcomeId = 3
  )
  
  testthat::expect_true(nrow(res)==0) # no results as calibrated rr is NA

})

test_that("getMetaEstimation", {
  
  res <- getMetaEstimation(
    connectionHandler = connectionHandlerES,
    resultDatabaseSettings = resultDatabaseSettingsES,
    targetId = 1,
    outcomeId = 3
  )
  
  testthat::expect_true(nrow(res)==1) 
  
})

test_that("createPlotForAnalysis", {
  
  data <- data.frame(
    database = 'database 1',
    description = 'test plot',
    outcome = 'sdsd',
    comparator = 'test',
    calibratedRr = 1,
    calibratedCi95Lb = 0.5,
    calibratedCi95Ub = 1.5
  )
  
  res <- createPlotForAnalysis(data)
  testthat::expect_is(res, "ggplot")

})


test_that("computeTraditionalP", {
  
  p1 <- computeTraditionalP(
    logRr = 1,
    seLogRr = 0.5,
    twoSided = T,
    upper = T
  )
  
  p2 <- computeTraditionalP(
    logRr = 1.1,
    seLogRr = 0.5,
    twoSided = T,
    upper = T
  )
  
  testthat::expect_true(p2 < p1)
  
})
 
test_that("getSccsEstimation", {
  
res <- getSccsEstimation(
  connectionHandlerES,
  resultDatabaseSettings = resultDatabaseSettingsES,
  targetId = 1,
  outcomeId = 3
)
  testthat::expect_equal(nrow(res), 0)
  
})

test_that("createPlotForSccsAnalysis", {
  
  data <- data.frame(
    calibratedRr = 1,
    calibratedCi95Lb = 0.8,
    calibratedCi95Ub = 1.2,
    type = 'test',
    database = 'database 1',
    description = 'test',
    outcome = 'outcome 1'
  )

  res <- createPlotForSccsAnalysis(data)
  testthat::expect_is(res, 'ggplot')

})