context("characterization-TimeToEvent")

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
  app = characterizationTimeToEventServer, 
  args = list(
    connectionHandler = connectionHandlerCharacterization,
    resultDatabaseSettings = resultDatabaseSettingsCharacterization,
    reactiveTargetRow = shiny::reactive(targetCohort[2,]),
    outcomeTable = shiny::reactive(outcomeCohort),
    reactiveOutcomeRowId = shiny::reactiveVal(0)
  ), 
  expr = {
    
    testthat::expect_true( is.null(allData()) )
    
    testthat::expect_true(inherits(characterizationTimeToEventColDefs(), 'list'))
    
    # data extracted when generate is set
    reactiveOutcomeRowId(1)
    session$setInputs(generate = TRUE)
    testthat::expect_true( nrow(allData()) > 0 )
    
    # check plot works
    session$setInputs(
      databases = unique(allData()$databaseName)[1],
      times = unique(allData()$timeScale)[1],
      outcomeTypes = unique(allData()$outcomeType)[1],
      targetOutcomeTypes = unique(allData()$targetOutcomeType)[1]
      )
  
    
    data <- getTimeToEventData(
      targetId = reactiveTargetRow()$cohortId,
      outcomeId = outcomeTable()[reactiveOutcomeRowId(),]$cohortId,
      connectionHandler = connectionHandlerCharacterization,
      resultDatabaseSettings = resultDatabaseSettingsCharacterization
    )
    testthat::expect_true( nrow(data) > 0 )
    
    plot <- plotTimeToEvent(
      timeToEventData = shiny::reactive(data),
      databases = unique(allData()$databaseName)[1],
      times = unique(allData()$timeScale)[1],
      outcomeTypes = unique(allData()$outcomeType)[1],
      targetOutcomeTypes = unique(allData()$targetOutcomeType)[1]
    )
    testthat::expect_true( inherits(plot, "ggplot") )
    
  })


test_that("Test characterizationTimeToEvent ui", {
  # Test ui
  ui <- characterizationTimeToEventViewer(id = 'viewer')
  checkmate::expect_list(ui)
})
