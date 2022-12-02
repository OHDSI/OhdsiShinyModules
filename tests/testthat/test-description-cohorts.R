context("description-cohorts")

shiny::testServer(
  app = descriptionTableServer, 
  args = list(
    connectionHandler = connectionHandlerDesc ,
    schema = resultDatabaseSettingsDesc$schema,
    mainPanelTab = shiny::reactiveVal("Feature Comparison"),
    tablePrefix = resultDatabaseSettingsDesc$tablePrefix,
    cohortTablePrefix = resultDatabaseSettingsDesc$cohortTablePrefix,
    databaseTable = resultDatabaseSettingsDesc$databaseTable
  ), 
  expr = {
    
    
    # make sure options returns a data.frame
    testthat::expect_true(class(inputVals) == 'list')
    testthat::expect_true(!is.null(inputVals$cohortIds) )
    testthat::expect_true(!is.null(inputVals$databaseIds) )
    
    # checl fetchData does not crash app
    session$setInputs(targetIds = c(1,3))
    session$setInputs(databaseId = 'eunomia')
    session$setInputs(fetchData = T)
    
    # check input$columnSelect works without error
    session$setInputs(columnSelect = 'countValue')
    session$setInputs(columnSelect = 'averageValue')
    session$setInputs(columnSelect = c('averageValue','countValue'))

    
  })
