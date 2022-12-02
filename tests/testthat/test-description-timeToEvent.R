context("description-TimeToEvent")

shiny::testServer(
  app = descriptionTimeToEventServer, 
  args = list(
    connectionHandler = connectionHandlerDesc,
    schema = resultDatabaseSettingsDesc$schema,
    mainPanelTab = shiny::reactiveVal("Feature Comparison"),
    tablePrefix = resultDatabaseSettingsDesc$tablePrefix,
    cohortTablePrefix = resultDatabaseSettingsDesc$cohortTablePrefix,
    databaseTable = resultDatabaseSettingsDesc$databaseTable
  ), 
  expr = {
    
    # make sure bothIds returns a list
    testthat::expect_true(class(bothIds) == 'list')
    testthat::expect_true(!is.null(bothIds$outcomeIds) )

    
    # checl targetId does not crash app
    session$setInputs(targetId = names(bothIds$outcomeIds)[1])
    
    # check input$fetchData does not crash app
    session$setInputs(outcomeId = 3)
    session$setInputs(fetchData = T)
    
    
  })
