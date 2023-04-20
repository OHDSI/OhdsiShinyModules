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
    
    # expect the binaryData() to be the default
    testthat::expect_true(nrow(binaryData()) == 1)
    testthat::expect_true(binaryData()$covariateName[1] == '')
    
    # make sure options returns a list
    testthat::expect_true(class(options) == 'list')
    testthat::expect_true(length(options) >0 )
    
    # check setting and generating works
    session$setInputs(tar = options$tars[1]) 
    session$setInputs(target = options$targets[1]) 
    session$setInputs(outcome = options$outcomes[1]) 
    session$setInputs(database1 = 'eunomia')
    session$setInputs(database2 = 'eunomia')
    session$setInputs(type1 = 'Target')
    session$setInputs(type2 = 'Outcome')
    session$setInputs(generate = TRUE)
    
    testthat::expect_true(binaryData()$covariateName[1] != '')
    
  })
