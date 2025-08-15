context("cohort-method-PopulationCharacteristics")

shiny::testServer(
  app = cohortMethodPopulationCharacteristicsServer, 
  args = list(
    selectedRow = shiny::reactiveVal(NULL), 
    connectionHandler = connectionHandlerEstimation, 
    resultDatabaseSettings = resultDatabaseSettingsEstimation
  ), 
  expr = {
    
    # make sure this runs if we pick the first row
    selectedRow(
      list(
        databaseId = 'eunomia', 
        databaseName = 'Eunomia', 
        analysisId = 1,
        description  = 'madeup',
        targetName = 'test target',
        targetId = 1,
        comparatorId = 2, 
        comparatorName = 'test comparator',
        outcomeId = 3,
        outcomeName = 'test outcome',
        psStrategy = ''
      )
    )
    testthat::expect_true(!is.null(data()))
    
    testthat::expect_true(!is.null(output$table1Caption))
    
  })
