context("characterization-TimeToEvent")

shiny::testServer(
  app = characterizationTimeToEventServer, 
  args = list(
    connectionHandler = connectionHandlerCharacterization,
    resultDatabaseSettings = resultDatabaseSettingsCharacterization,
    targetId = shiny::reactive(1),
    outcomeId = shiny::reactive(3)
  ), 
  expr = {
    
    testthat::expect_true(inherits(options(), 'list'))
    testthat::expect_true( nrow(allData()) > 0 )
    testthat::expect_true(inherits(characterizationTimeToEventColDefs(), 'list'))
    
    # check plot works
    session$setInputs(
      databases = unique(allData()$databaseName)[1],
      times = unique(allData()$timeScale)[1],
      outcomeTypes = unique(allData()$outcomeType)[1],
      targetOutcomeTypes = unique(allData()$targetOutcomeType)[1]
      )
    
    
    data <- getTimeToEventData(
      targetId = targetId(),
      outcomeId = outcomeId(),
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
