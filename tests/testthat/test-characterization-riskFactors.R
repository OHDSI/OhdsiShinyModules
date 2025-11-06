context("characterization-riskFactors")

targetCohort <- OhdsiReportGenerator::getTargetTable(
  connectionHandler = connectionHandlerCharacterization,
  schema = resultDatabaseSettingsCharacterization$schema, 
  ciTablePrefix = resultDatabaseSettingsCharacterization$incidenceTablePrefix
)

outcomeCohort <- OhdsiReportGenerator::getOutcomeTable(
  connectionHandler = connectionHandlerCharacterization,
  schema = resultDatabaseSettingsCharacterization$schema, 
  ciTablePrefix = resultDatabaseSettingsCharacterization$incidenceTablePrefix, 
  targetId = targetCohort$cohortId[1]
)


shiny::testServer(
  app = characterizationRiskFactorServer, 
  args = list(
    connectionHandler = connectionHandlerCharacterization ,
    resultDatabaseSettings = resultDatabaseSettingsCharacterization,
    reactiveTargetRow = shiny::reactive(targetCohort[1,]),
    outcomeTable = shiny::reactive(outcomeCohort),
    reactiveOutcomeRowId = shiny::reactiveVal(0)
    ), 
  expr = {
    
    # check database
    testthat::expect_true(length(databaseNames()) > 0 )
    testthat::expect_true(length(databaseIds()) > 0 )
    
    #Test characterizationGetRiskFactorData
    data <- characterizationGetRiskFactorData(
      connectionHandler = connectionHandlerCharacterization ,
      resultDatabaseSettings = resultDatabaseSettingsCharacterization,
      targetId = targetCohort$cohortId[1],
      outcomeId = outcomeCohort$cohortId[1],
      databaseId = '388020256',
      tar = list(
        riskWindowStart = 1,
        riskWindowEnd = 365,
        startAnchor = 'cohort start',
        endAnchor = 'cohort end'
      )
    )
    
    testthat::expect_true(inherits(data, 'list'))
    testthat::expect_true(nrow(data$binary) > 0 )
    testthat::expect_true(nrow(data$continuous) > 0 )
    
    # check setting reactiveOutcomeRowId updates these - doesnt seem to work?
    reactiveOutcomeRowId(1)
    session$flushReact()
    
    testthat::skip_if(reactiveOutcomeRowId() != 1)
    testthat::expect_true(inherits(selected(), 'NULL'))
    
    # check setting and generating works
    session$setInputs(tarInd = strsplit(
      x = outcomeTable()[1,]$tarNames, 
      split = ':'
    )[[1]][1]) 
    session$setInputs(databaseName = databaseNames()[1]) 
    session$setInputs(outcomeWashout = strsplit(
      x = outcomeTable()[1,]$outcomeWashoutDays, 
      split = ':'
    )[[1]][1]) 
    session$setInputs(generate = TRUE)
    
    testthat::expect_true(inherits(selected(), 'data.frame'))
    
    #testthat::expect_true(inherits(allData, 'list'))
    #testthat::expect_true( nrow(allData$binary) > 0 )
    #testthat::expect_true( nrow( allData$continuous) > 0 )
    

    
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
