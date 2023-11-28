context("evidence-synth-cm")

shiny::testServer(evidenceSynthesisCmServer, args = list(
  id = "testEvidenceSynthesisCmServer",
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
      comparatorId = 2, 
      comparator = 'test comparator',
      outcome = 'test outcome',
      outcomeId = 3,
      outcomeIds = 3
    )
    )
  
  testthat::expect_is(output$esCohortMethodPlot, 'list')
  
  testthat::expect_true( nrow(unique(cmdata())) >0 )
  testthat::expect_equal(as.double(inputSelected()$outcomeId), 3)
  
})

test_that("Test es cm ui", {
  # Test ui
  ui <- evidenceSynthesisCmViewer()
  checkmate::expect_list(ui)
  })


test_that("getEsCmTargetIds", {
tarId <- getEsCmTargetIds(
  connectionHandler = connectionHandlerES,
  resultDatabaseSettings = resultDatabaseSettingsES
)

testthat::expect_true(length(tarId) > 0 )
  
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

# getEvidenceSynthCmDiagnostics ?

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


