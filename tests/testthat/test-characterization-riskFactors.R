context("characterization-riskFactors")

shiny::testServer(
  app = characterizationRiskFactorServer, 
  args = list(
    connectionHandler = connectionHandlerCharacterization ,
    resultDatabaseSettings = resultDatabaseSettingsCharacterization,
    targetId = shiny::reactive(1), #reactive 
    outcomeId = shiny::reactive(3)
    ), 
  expr = {
    
    # make sure options returns a list
    testthat::expect_true(class(options()) == 'list')
    testthat::expect_true(length(options()$databaseIds) >0 )
    testthat::expect_true(length(options()$tarInds) >0 )
    
    testthat::expect_true(inherits(selected(), 'NULL'))
    
    # check setting and generating works
    session$setInputs(tarInd = options()$tarInds[1]) 
    session$setInputs(databaseId = options()$databaseIds[1]) 
    session$setInputs(generate = TRUE)
    
    testthat::expect_true(inherits(selected(), 'data.frame'))
    
    #testthat::expect_true(inherits(allData, 'list'))
    #testthat::expect_true( nrow(allData$binary) > 0 )
    #testthat::expect_true( nrow( allData$continuous) > 0 )
    
    #Test characterizationGetRiskFactorData
    data <- characterizationGetRiskFactorData(
      connectionHandler = connectionHandlerCharacterization ,
      resultDatabaseSettings = resultDatabaseSettingsCharacterization,
      targetId = 1,
      outcomeId = 3,
      databaseId = 'eunomia',
      tar = list(
        riskWindowStart = 0,
        riskWindowEnd = 365,
        startAnchor = 'cohort start',
        endAnchor = 'cohort start'
      )
    )
    
    testthat::expect_true(inherits(data, 'list'))
    testthat::expect_true(nrow(data$binary) > 0 )
    testthat::expect_true(nrow(data$continuous) > 0 )
    
  })


test_that("Test characterizationRiskFactorViewer ui", {
  # Test ui
  ui <- characterizationRiskFactorViewer(id = 'viewer')
  checkmate::expect_list(ui)
})

test_that("Test characteriationRiskFactorColDefs", {
  colDefs <- characteriationRiskFactorColDefs()
  testthat::expect_true(inherits( colDefs, 'list'))
  testthat::expect_true(length(colDefs) > 0 )
})

test_that("Test characteriationRiskFactorContColDefs", {
  colDefs <- characteriationRiskFactorContColDefs()
  testthat::expect_true(inherits( colDefs, 'list'))
  testthat::expect_true(length(colDefs) > 0 )
})
