context("characterization-caseSeries")

targetCohort <- OhdsiReportGenerator::getTargetTable(
  connectionHandler = connectionHandlerCharacterization,
  schema = resultDatabaseSettingsCharacterization$schema, 
  cTablePrefix = resultDatabaseSettingsCharacterization$cTablePrefix,
  ciTablePrefix = resultDatabaseSettingsCharacterization$incidenceTablePrefix
)

outcomeCohort <- OhdsiReportGenerator::getOutcomeTable(
  connectionHandler = connectionHandlerCharacterization,
  schema = resultDatabaseSettingsCharacterization$schema, 
  cTablePrefix = resultDatabaseSettingsCharacterization$cTablePrefix,
  ciTablePrefix = resultDatabaseSettingsCharacterization$incidenceTablePrefix, 
  targetId = targetCohort$cohortId[1], 
  getCharacterizationInclusion = TRUE
)

shiny::testServer(
  app = characterizationCaseSeriesServer, 
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
    testthat::expect_true(databaseNames()[1] == "Synthea")
    testthat::expect_true(databaseIds()[1] == "388020256")
    
    # check data fetches
    data <- characterizationGetCaseSeriesData(
      connectionHandler = connectionHandlerCharacterization ,
      resultDatabaseSettings = resultDatabaseSettingsCharacterization,
      targetId = targetCohort$cohortId[1],
      outcomeId = outcomeCohort$cohortId[1],
      databaseId = databaseIds()[1],
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
    
    
    counts <- characterizationGetCaseSeriesCounts(
      connectionHandler = connectionHandlerCharacterization ,
      resultDatabaseSettings = resultDatabaseSettingsCharacterization,
      targetId = targetCohort$cohortId[1],
      outcomeId = outcomeCohort$cohortId[1],
      databaseId = databaseIds()[1],
      tar = list(
        riskWindowStart = 1,
        riskWindowEnd = 365,
        startAnchor = 'cohort start',
        endAnchor = 'cohort end'
      )
    )
    testthat::expect_true(inherits(counts, 'data.frame'))
    
  
    # check values when reactiveOutcomeRowId is 0
    testthat::expect_true(inherits(reactiveOutcomeTar(), 'NULL'))
    testthat::expect_true(inherits(reactiveOutcomeTarValues(), 'NULL'))
    testthat::expect_true(inherits(reactiveOutcomeWashout(), 'NULL'))
    
    # check setting reactiveOutcomeRowId updates these - doesnt seem to work?
    ##reactiveOutcomeRowId(1)
    
    #reactable::updateReactable("characterization-case-series-outcome-table-select-case-input-table-resultData", selected = c(1))
    #reactable::updateReactable("outcome-table-select-case-input-table-resultData", selected = c(1))
    
    reactiveOutcomeRowId(1)
    session$flushReact()
    
    # that should set reactiveOutcomeTar - why not??
    testthat::skip_if(reactiveOutcomeRowId() != 1)
    testthat::expect_true(!is.null(reactiveOutcomeTar()))
    testthat::expect_true(!is.null(reactiveOutcomeTarValues()))
    testthat::expect_true(!is.null(reactiveOutcomeWashout()))
    
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
    session$setInputs(generate = 2)
    session$flushReact()
    
    testthat::expect_true(inherits(selected(), 'data.frame'))
    
    
  })


test_that("Test characterizationCaseSeriesViewer ui", {
  # Test ui
  ui <- characterizationCaseSeriesViewer(id = 'viewer')
  checkmate::expect_list(ui)
})


test_that("Test colDefsBinary", {
  colDefs <- colDefsBinary('test')
  testthat::expect_true(inherits( colDefs, 'list'))
  testthat::expect_true(length(colDefs) > 0 )
})

test_that("Test colDefsContinuous", {
  colDefs <- colDefsContinuous()
  testthat::expect_true(inherits( colDefs, 'list'))
  testthat::expect_true(length(colDefs) > 0 )
})
