context("characterization-database")

options <- characterizationGetOptions(
  connectionHandler = connectionHandlerCharacterization,
  resultDatabaseSettings = resultDatabaseSettingsCharacterization,
  includeAggregate = T,
  includeIncidence = T
)
parents <- characterizationGetParents(options)


shiny::testServer(
  app = characterizationDatabaseComparisonServer, 
  args = list(
    connectionHandler = connectionHandlerCharacterization,
    resultDatabaseSettings = resultDatabaseSettingsCharacterization,
    options = options,
    parents = parents,
    parentIndex = shiny::reactive(1), # reactive
    subTargetId = shiny::reactive(1) # reactive
    ), 
  expr = {
    
    testthat::expect_true(inherits(inputVals(), 'list'))
    testthat::expect_true(length(inputVals()$databaseIds) > 0)
    
    inputTests <- characterizationGetCohortsInputs(
      connectionHandler = connectionHandlerCharacterization,
      resultDatabaseSettings = resultDatabaseSettingsCharacterization,
      targetId = shiny::reactive(3)
    )
    testthat::expect_true(inherits(inputTests, 'list'))
    
    
    # set inputs
    session$setInputs(
      databaseIds = inputVals()$databaseIds,
      minThreshold = 0.02
    )

    session$setInputs(
      generate = T
    )
    
    testthat::expect_true(inherits(selected(),'data.frame'))
    
    resultTable <- characterizatonGetDatabaseComparisonData(
      connectionHandler = connectionHandler,
      resultDatabaseSettings = resultDatabaseSettings,
      targetIds = c(1),
      databaseIds = input$databaseIds,
      minThreshold = 0.02
    )
    testthat::expect_true(inherits(resultTable$table , 'data.frame'))
    testthat::expect_true(nrow(resultTable$table ) > 0)
    testthat::expect_true(inherits(resultTable$databaseNames , 'data.frame'))
    testthat::expect_true(nrow(resultTable$databaseNames ) > 0)
    
    countTable <- characterizatonGetCohortCounts(
      connectionHandler = connectionHandler,
      resultDatabaseSettings = resultDatabaseSettings,
      targetIds = c(1),
      databaseIds = input$databaseIds
    )
    testthat::expect_true(nrow(countTable) > 0)
    
    
    continuousTable <- characterizatonGetCohortComparisonDataContinuous(
      connectionHandler = connectionHandler,
      resultDatabaseSettings = resultDatabaseSettings,
      targetIds = c(1),
      databaseIds = input$databaseIds,
      pivot = F
    )
    testthat::expect_true(nrow(continuousTable) > 0)
    
  
  })

test_that("Test characterizationDatabaseComparison ui", {
  # Test ui
  ui <- characterizationDatabaseComparisonViewer(id = 'viewer')
  checkmate::expect_list(ui)
})


test_that("Test getMinCovaraiteThreshold", {

  minVal <- getMinCovaraiteThreshold(
    connectionHandler = connectionHandlerCharacterization,
    resultDatabaseSettings = resultDatabaseSettingsCharacterization
  )
  
  testthat::expect_true(minVal >= 0)
  
})

