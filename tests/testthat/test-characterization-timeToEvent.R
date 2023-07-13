context("characterization-TimeToEvent")

shiny::testServer(
  app = characterizationTimeToEventServer, 
  args = list(
    connectionHandler = connectionHandlerCharacterization,
    schema = resultDatabaseSettingsCharacterization$schema,
    mainPanelTab = shiny::reactiveVal("Feature Comparison"),
    tablePrefix = resultDatabaseSettingsCharacterization$tablePrefix,
    cohortTablePrefix = resultDatabaseSettingsCharacterization$cohortTablePrefix,
    databaseTable = resultDatabaseSettingsCharacterization$databaseTable
  ), 
  expr = {
    
    # make sure bothIds returns a list
    testthat::expect_true(class(bothIds) == 'list')
    testthat::expect_true(!is.null(bothIds$outcomeIds) )

    
    # checl targetId does not crash app
    session$setInputs(targetId = names(bothIds$outcomeIds)[1])
    
    # check input$generate does not crash app
    session$setInputs(outcomeId = 3)
    session$setInputs(generate = T)
    
    
  })
