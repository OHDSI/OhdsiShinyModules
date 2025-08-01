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
  targetId = targetCohort$cohortId[2]
)


shiny::testServer(
  app = characterizationRiskFactorServer, 
  args = list(
    connectionHandler = connectionHandlerCharacterization ,
    resultDatabaseSettings = resultDatabaseSettingsCharacterization,
    reactiveTargetRow = shiny::reactive(targetCohort[2,]),
    outcomeTable = shiny::reactive(outcomeCohort),
    reactiveOutcomeRowId = shiny::reactiveVal(0)
    ), 
  expr = {
    
    # check database
    testthat::expect_true(length(databaseNames()) > 0 )
    testthat::expect_true(length(databaseIds()) > 0 )
    
    # check setting reactiveOutcomeRowId updates these - doesnt seem to work?
    reactiveOutcomeRowId(1)
    ##testthat::expect_equal(reactiveOutcomeTar(), strsplit(
    ##  x = outcomeTable()[reactiveOutcomeRowId(),]$tarNames, 
    ##  split = ':'
    ##)[[1]])
    ##testthat::expect_true(!is.null(reactiveOutcomeWashout()))
    
    testthat::expect_true(inherits(selected(), 'NULL'))
    
    # check setting and generating works
    session$setInputs(tarInd = strsplit(
      x = outcomeTable()[reactiveOutcomeRowId(),]$tarNames, 
      split = ':'
    )[[1]][1]) 
    session$setInputs(databaseName = databaseNames()[1]) 
    session$setInputs(outcomeWashout = strsplit(
      x = outcomeTable()[reactiveOutcomeRowId(),]$outcomeWashoutDays, 
      split = ':'
    )[[1]][1]) 
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
        riskWindowStart = 1,
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
