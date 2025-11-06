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
  targetId = targetCohort$cohortId[1]
)


shiny::testServer(
  app = characterizationTimeToEventServer, 
  args = list(
    connectionHandler = connectionHandlerCharacterization,
    resultDatabaseSettings = resultDatabaseSettingsCharacterization,
    reactiveTargetRow = shiny::reactive(targetCohort[1,]),
    outcomeTable = shiny::reactive(outcomeCohort),
    reactiveOutcomeRowId = shiny::reactiveVal(0)
  ), 
  expr = {
    
    testthat::expect_true( is.null(allData()) )
    testthat::expect_true(inherits(characterizationTimeToEventColDefs(), 'list'))
    
    data <- getTimeToEventData(
      targetId = reactiveTargetRow()$cohortId,
      outcomeId = outcomeTable()[1,]$cohortId,
      connectionHandler = connectionHandlerCharacterization,
      resultDatabaseSettings = resultDatabaseSettingsCharacterization
    )
    testthat::expect_true( nrow(data) > 0 )
    
    plot <- plotTimeToEvent(
      timeToEventData = shiny::reactive(data),
      databases = unique(data$databaseName)[1],
      times = "per 365-day",
      outcomeTypes = unique(data$outcomeType)[1],
      targetOutcomeTypes = unique(data$targetOutcomeType)[1]
    )
    testthat::expect_is(plot, "ggplot") 
    
    
    # data extracted when generate is set
    reactiveOutcomeRowId(1)
    session$flushReact()
    
    testthat::skip_if(reactiveOutcomeRowId() != 1 )
    
    session$setInputs(generate = 1)
    session$flushReact()
    testthat::expect_true( nrow(allData()) > 0 )
    
    # check plot works
    session$setInputs(
      databases = unique(allData()$databaseName)[1],
      times = unique(allData()$timeScale)[1],
      outcomeTypes = unique(allData()$outcomeType)[1],
      targetOutcomeTypes = unique(allData()$targetOutcomeType)[1]
      )
    
  })


test_that("Test characterizationTimeToEvent ui", {
  # Test ui
  ui <- characterizationTimeToEventViewer(id = 'viewer')
  checkmate::expect_list(ui)
})
