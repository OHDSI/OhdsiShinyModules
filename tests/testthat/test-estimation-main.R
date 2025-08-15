context("estimation-main")

shiny::testServer(
  app = estimationServer, 
  args = list(
    id = 'cm',
    connectionHandler = connectionHandlerEstimation,
    resultDatabaseSettings = resultDatabaseSettingsEstimation
  ), 
  expr = {
    
    testthat::expect_true(inherits(targetIds(), 'NULL'))
    testthat::expect_true(inherits(outcomeId(), 'NULL'))
    
    testthat::expect_true(inherits(estimationTypes,"character"))
    
    testthat::expect_true("Cohort Method" %in% estimationTypes)
    testthat::expect_true("SCCS" %in% estimationTypes)
    
    #testthat::expect_true(output$tabShow() == 0)
    #testthat::expect_true(output$outcomeShow() == 0)
    testthat::expect_true(is.null(outcomes()))
    
    # test observe input$targetId
    session$setInputs(
      targetId = 2
    )
    session$setInputs(
      targetSelect = 1
    )
    #testthat::expect_true(output$outcomeShow() == 1)
    #testthat::expect_true(!is.null(outcomes()))
    
    # check outcome selection
    session$setInputs(
      outcomeId = 3
    )
    session$setInputs(
      outcomeSelect = 1
    )
    #testthat::expect_true(output$tabShow() == 1)
    
    testthat::expect_true(!is.null(outcomeId()))
    testthat::expect_true(!is.null(targetIds()))

  })


test_that("Test estimation ui", {
  # Test ui
  ui <- estimationViewer('test')
  checkmate::expect_list(ui)
})


# 
test_that("Test getEstimationTypes", {
  
  types <- getEstimationTypes(
    connectionHandler = connectionHandlerEstimation,
    resultDatabaseSettings = resultDatabaseSettingsEstimation
  )  
  testthat::expect_true(inherits(types,"character"))
  testthat::expect_true("Cohort Method" %in% types)
  testthat::expect_true("SCCS" %in% types)
  testthat::expect_true("Evidence Synthesis" %in% types)
  

  typesCm <- getEstimationTypes(
    connectionHandler = connectionHandlerEstimation,
    resultDatabaseSettings = resultDatabaseSettingsEstimationCm
  )  
  testthat::expect_true(inherits(typesCm,"character"))
  testthat::expect_true("Cohort Method" %in% typesCm)
  testthat::expect_true(!"SCCS" %in% typesCm)
  testthat::expect_true(!"Evidence Synthesis" %in% typesCm)
  
  typesSccs <- getEstimationTypes(
    connectionHandler = connectionHandlerEstimation,
    resultDatabaseSettings = resultDatabaseSettingsEstimationSccs
  )  
  testthat::expect_true(inherits(typesSccs,"character"))
  testthat::expect_true(!"Cohort Method" %in% typesSccs)
  testthat::expect_true("SCCS" %in% typesSccs)
  testthat::expect_true(!"Evidence Synthesis" %in% typesSccs)
  
})
