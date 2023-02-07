context("description-aggregateFeatures")

shiny::testServer(
  app = descriptionAggregateFeaturesServer, 
  args = list(
    connectionHandler = connectionHandlerDesc ,
    schema = resultDatabaseSettingsDesc$schema,
    mainPanelTab = shiny::reactiveVal("Feature Comparison"),
    tablePrefix = resultDatabaseSettingsDesc$tablePrefix,
    cohortTablePrefix = resultDatabaseSettingsDesc$cohortTablePrefix,
    databaseTable = resultDatabaseSettingsDesc$databaseTable
  ), 
  expr = {
    
    # expect the values to be null untill button clicked
    testthat::expect_null(targetId())
    
    # make sure options returns a data.frame
    testthat::expect_true(class(options) == 'data.frame')
    testthat::expect_true(nrow(options) >0 )
    
    # set input$descAgSelect to list with index = 1
    session$setInputs(descAgSelect = list(index = 1))  
    testthat::expect_true(!is.null(targetId()))
    testthat::expect_true(!is.null(outcomeId()))
    
    # input$ag_plot with settings works
    session$setInputs(database1 = 'eunomia')
    session$setInputs(database2 = 'eunomia')
    session$setInputs(type1 = 'Target')
    session$setInputs(type2 = 'Outcome')
    session$setInputs(ag_plot = TRUE)
    
  })
