context("characterization-DechallengeRechallenge")

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
  app = characterizationDechallengeRechallengeServer, 
  args = list(
    connectionHandler = connectionHandlerCharacterization,
    resultDatabaseSettings = resultDatabaseSettingsCharacterization,
    reactiveTargetRow = shiny::reactive(targetCohort[1,]),
    outcomeTable = shiny::reactive(outcomeCohort),
    reactiveOutcomeRowId = shiny::reactiveVal(0)
  ), 
  expr = {
    
    # allData null initially
    testthat::expect_true(is.null(allData()) )
    
    # select the first outcome
    reactiveOutcomeRowId(1) 
    session$setInputs(generate = TRUE) # not working!
    # check generate works
    ##testthat::expect_true(nrow(allData()) > 0 )
    
    # characteriationDechalRechalColDefs is a list
    testthat::expect_true(inherits(characteriationDechalRechalColDefs(), 'list'))
    
    # failData NULL
    testthat::expect_true(is.null(failData()))
    
    # tableOutputs$actionCount()
    # failData not NULL
    
    
    data <- getDechalRechalInputsData(
      targetId = reactiveTargetRow()$cohortId,
      outcomeId = outcomeTable()[1,]$cohortId[1],
      connectionHandler = connectionHandlerCharacterization,
      resultDatabaseSettings = resultDatabaseSettingsCharacterization
    )
    testthat::expect_true(nrow(data) > 0)
    
    # add tests for functions with progress bar
    
    fails <- getDechalRechalFailData(
      targetId = reactiveTargetRow()$cohortId,
      outcomeId = outcomeTable()[1,]$cohortId[1],
      databaseId = 'Synthea',
      dechallengeStopInterval = 30,
      dechallengeEvaluationWindow = 30,
      connectionHandler = connectionHandlerCharacterization,
      resultDatabaseSettings = resultDatabaseSettingsCharacterization
    )
    
    testthat::expect_true(inherits(fails, 'data.frame'))
    
    if(nrow(fails) > 0){
      plot <- plotDechalRechal(
        dechalRechalData = fails,
        i = 1
      )
      testthat::expect_true(inherits(plot, 'ggplot'))
    }
    
  })

test_that("Test characterizationDechallengeRechallenge ui", {
  # Test ui
  ui <- characterizationDechallengeRechallengeViewer(id = 'viewer')
  checkmate::expect_list(ui)
})


test_that("Test characteriationDechalRechalColDefs", {
  # Test ui
  colDef <- characteriationDechalRechalColDefs()
  testthat::expect_is(colDef, 'list')
})
