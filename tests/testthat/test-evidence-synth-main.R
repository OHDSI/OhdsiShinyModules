context("evidence-synth-main")

shiny::testServer(evidenceSynthesisServer, args = list(
  id = "testEvidenceSynthesisServer",
  connectionHandler = connectionHandlerES,
  resultDatabaseSettings = resultDatabaseSettingsES
), {
  
  a <- 2
  testthat::expect_true(a == 2)
  
})

test_that("Test es ui", {
  # Test ui
  ui <- evidenceSynthesisViewer()
  checkmate::expect_list(ui)
  checkmate::expect_file_exists(evidenceSynthesisHelperFile())
})

test_that("getEsOutcomeIds", {
  
  outcomes <- getEsOutcomeIds(
    connectionHandler = connectionHandlerES,
    resultDatabaseSettings = resultDatabaseSettingsES
  )
  testthat::expect_true(length(outcomes)>0)
  
})

test_that("getColDefsESDiag", {
  
colDef <- getColDefsESDiag(
    connectionHandler = connectionHandlerES,
    resultDatabaseSettings = resultDatabaseSettingsES
)

testthat::expect_is(
  object = colDef, 
  class = 'list'
  )

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
 
