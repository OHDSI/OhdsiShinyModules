context("characterization-incidence")

shiny::testServer(
  app = characterizationIncidenceServer, 
  args = list(
    connectionHandler = connectionHandlerCharacterization,
    schema = resultDatabaseSettingsCharacterization$schema,
    mainPanelTab = shiny::reactiveVal("Feature Comparison"),
    incidenceTablePrefix = resultDatabaseSettingsCharacterization$incidenceTablePrefix,
    databaseTable = resultDatabaseSettingsCharacterization$databaseTable
  ), 
  expr = {
    
    # make sure cohorts is a data.frame
    testthat::expect_true(class(cohorts) == 'list')
    testthat::expect_true(!is.null(cohorts$targetIds))
    testthat::expect_true(!is.null(cohorts$outcomeIds))
    
    # check input$generate does not crash app
    session$setInputs(targetId = 1)
    session$setInputs(outcomeId = 3)
    session$setInputs(generate = T)
    
  })
