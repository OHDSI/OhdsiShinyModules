context("characterization-aggregateFeatures")

shiny::testServer(
  app = characterizationAggregateFeaturesServer, 
  args = list(
    connectionHandler = connectionHandlerCharacterization ,
    schema = resultDatabaseSettingsCharacterization$schema,
    mainPanelTab = shiny::reactiveVal("Feature Comparison"),
    tablePrefix = resultDatabaseSettingsCharacterization$tablePrefix,
    cohortTablePrefix = resultDatabaseSettingsCharacterization$cohortTablePrefix,
    databaseTable = resultDatabaseSettingsCharacterization$databaseTable
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
    session$setInputs(type1 = types[1])
    session$setInputs(type2 = types[2])
    session$setInputs(generate = TRUE)
    
    testthat::expect_true(binaryData()$covariateName[1] != '')
    
  })
