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
  targetId = targetCohort$cohortId[2]
)


shiny::testServer(
  app = characterizationDechallengeRechallengeServer, 
  args = list(
    connectionHandler = connectionHandlerCharacterization,
    resultDatabaseSettings = resultDatabaseSettingsCharacterization,
    reactiveTargetRow = shiny::reactive(targetCohort[2,]),
    reactiveOutcomeRow = shiny::reactive(outcomeCohort[1,])
  ), 
  expr = {
    
    # allData null initially
    testthat::expect_true(is.null(allData()) )
    
    session$setInputs(generate = TRUE)
    # check generate works
    testthat::expect_true(nrow(allData()) > 0 )
    
    # characteriationDechalRechalColDefs is a list
    testthat::expect_true(inherits(characteriationDechalRechalColDefs(), 'list'))
    
    # failData NULL
    testthat::expect_true(is.null(failData()))
    
    # tableOutputs$actionCount()
    # failData not NULL
    
    
    # add tests for functions with progress bar
    
    fails <- getDechalRechalFailData(
      targetId = reactiveTargetRow()$cohortId,
      outcomeId = reactiveOutcomeRow()$cohortId,
      databaseId = 'eunomia',
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
