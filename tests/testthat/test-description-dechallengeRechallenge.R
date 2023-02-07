context("description-DechallengeRechallenge")

shiny::testServer(
  app = descriptionDechallengeRechallengeServer, 
  args = list(
    connectionHandler = connectionHandlerDesc ,
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
    
    # check fetchData sets these reactices
    testthat::expect_true(length(databases())>0)
    testthat::expect_true(length(dechallengeStopInterval())>0)
    testthat::expect_true(length(dechallengeEvaluationWindow())>0)
    
    
    # check input$databaseRowId works without error
    session$setInputs(databaseRowId  = list(index = 1))
    
  })
