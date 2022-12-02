context("description-incidence")

shiny::testServer(
  app = descriptionIncidenceServer, 
  args = list(
    connectionHandler = connectionHandlerDesc ,
    schema = resultDatabaseSettingsDesc$schema,
    mainPanelTab = shiny::reactiveVal("Feature Comparison"),
    incidenceTablePrefix = resultDatabaseSettingsDesc$incidenceTablePrefix,
    databaseTable = resultDatabaseSettingsDesc$databaseTable
  ), 
  expr = {
    
    # make sure cohorts is a data.frame
    testthat::expect_true(class(cohorts) == 'list')
    testthat::expect_true(!is.null(cohorts$targetIds))
    testthat::expect_true(!is.null(cohorts$outcomeIds))
    
    # check input$fetchData does not crash app
    session$setInputs(targetId = 1)
    session$setInputs(outcomeId = 3)
    session$setInputs(fetchData = T)
    
    
  })
