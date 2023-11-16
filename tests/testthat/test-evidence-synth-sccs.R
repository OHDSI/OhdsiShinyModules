context("evidence-synth-sccs")

shiny::testServer(evidenceSynthesisSccsServer, args = list(
  id = "testEvidenceSynthesisSccsServer",
  connectionHandler = connectionHandlerES,
  resultDatabaseSettings = resultDatabaseSettingsES
), {
  
  expect_true(length(targetIds) > 0)
  expect_true(length(outcomeIds) > 0)
  
  inputSelected(
    list(
      targetId = targetIds[1], 
      targetIds = targetIds[1],
      target = 'test target',
      outcome = 'test outcome',
      outcomeId = 3,
      outcomeIds = 3
    )
    )
  
  #testthat::expect_is(output$esSccsPlot, 'list')
  
  testthat::expect_is( sccsData(), 'data.frame')
  testthat::expect_equal(as.double(inputSelected()$outcomeId), 3)
  
})

test_that("Test es ui", {
  # Test ui
  ui <- evidenceSynthesisSccsViewer()
  checkmate::expect_list(ui)
})

test_that("getSccsEstimation", {
  tarIds <- getSccsTargetIds(
    connectionHandler = connectionHandlerES,
    resultDatabaseSettings = resultDatabaseSettingsES
  )
  
  testthat::expect_true( length(tarIds) > 0 )
  
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
  testthat::expect_is(res, 'gtable')

})

# getEvidenceSynthSccsDiagnostics ?

