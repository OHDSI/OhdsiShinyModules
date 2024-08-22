context("characterization-cohorts")

options <- characterizationGetOptions(
  connectionHandler = connectionHandlerCharacterization,
  resultDatabaseSettings = resultDatabaseSettingsCharacterization,
  includeAggregate = T,
  includeIncidence = T
)
parents <- characterizationGetParents(options)


shiny::testServer(
  app = characterizationCohortComparisonServer, 
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
      comparatorGroup = parents[2],
      comparatorId = comparatorOptions[1],
      databaseId = inputVals()$databaseIds[1]
    )
    testthat::expect_true(comparatorIndex() == 2)
    testthat::expect_true(comparatorGroups() == characterizationGetChildren(options, comparatorIndex()))
    
    session$setInputs(
      comparatorId = comparatorGroups()[1]
    )
    session$setInputs(
      generate = T
    )
    
    testthat::expect_true(inherits(selected(),'data.frame'))
    
    resultTable <- characterizatonGetCohortData(
      connectionHandler = connectionHandler,
      resultDatabaseSettings = resultDatabaseSettings,
      targetIds = c(1,2),
      databaseIds = input$databaseId,
      minThreshold = 0.01,
      addSMD = T
    )
    testthat::expect_true(nrow(resultTable) > 0)
    
    countTable <- characterizatonGetCohortCounts(
      connectionHandler = connectionHandler,
      resultDatabaseSettings = resultDatabaseSettings,
      targetIds = c(1,2),
      databaseIds = input$databaseId
    )
    testthat::expect_true(nrow(countTable) > 0)
    
    
    continuousTable <- characterizatonGetCohortComparisonDataContinuous(
      connectionHandler = connectionHandler,
      resultDatabaseSettings = resultDatabaseSettings,
      targetIds = c(1,2),
      databaseIds = input$databaseId
    )
    testthat::expect_true(nrow(continuousTable) > 0)
    
  
  })

test_that("Test characterizationTable ui", {
  # Test ui
  ui <- characterizationCohortComparisonViewer(id = 'viewer')
  checkmate::expect_list(ui)
})


test_that("Test characterizationCohortsColumns", {
  cols <- characterizationCohortsColumns()
  testthat::expect_true(inherits(cols, 'list'))
})

test_that("Test characteriationCountTableColDefs", {
  cols <- characteriationCountTableColDefs()
  testthat::expect_true(inherits(cols, 'list'))
})

test_that("Test characterizationCohortsColumnsContinuous", {
  cols <- characterizationCohortsColumnsContinuous()
  testthat::expect_true(inherits(cols, 'list'))
})

