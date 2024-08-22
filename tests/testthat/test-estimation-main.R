context("estimation-main")

shiny::testServer(
  app = estimationServer, 
  args = list(
    id = 'cm',
    connectionHandler = connectionHandlerEstimation,
    resultDatabaseSettings = resultDatabaseSettingsEstimation
  ), 
  expr = {
    
    testthat::expect_true(inherits(estimationTypes,"character"))
    testthat::expect_true(inherits(options,"list"))
    testthat::expect_true(length(options) > 0 )
    
    testthat::expect_true("Cohort Method" %in% estimationTypes)
    testthat::expect_true("SCCS" %in% estimationTypes)
    
    # test observe input$targetId
    testthat::expect_equal(outcomes(), options$tos[[1]])
    session$setInputs(
      targetId = 2
    )
    testthat::expect_true(input$targetId == 2)
    testthat::expect_true(length(outcomes()$outcomeId) > 0)
    
    
    # test targetSelect clicked
    testthat::expect_true(inherits(targetSelected(), 'NULL'))
    testthat::expect_true(inherits(comparatorIds(), 'NULL'))
    testthat::expect_true(inherits(targetIds(), 'NULL'))
    testthat::expect_true(inherits(outcomeId(), 'NULL'))
    session$setInputs(
      outcomeId = 3,
      targetId = 1
    )
    testthat::expect_true(is.null(input$targetSelect))
    session$setInputs(
      targetSelect = T
    )
    testthat::expect_true(input$targetSelect == T)
    testthat::expect_true(input$outcomeId == 3)
    testthat::expect_true(length(names(targets)[targets == input$targetId])>0)
    testthat::expect_true(length(outcomes()$outcomeName[outcomes()$outcomeId == input$outcomeId])>0)
  
    # check the reactiveVals updated when the observe event is clicked
    ##testthat::expect_true(!is.null(targetSelected()))
    ##testthat::expect_true(!is.null(comparatorIds()))
    ##testthat::expect_true(!is.null(targetIds()))
    ##testthat::expect_true(outcomeId() == 3)
    ##testthat::expect_true(!is.null(outcomeId()))
    
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
