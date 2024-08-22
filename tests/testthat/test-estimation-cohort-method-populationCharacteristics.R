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
        cdmSourceAbbreviation = 'Eunomia', 
        analysisId = 1,
        description  = 'madeup',
        target = 'test target',
        targetId = 1,
        comparatorId = 2, 
        comparator = 'test comparator',
        outcomeId = 3,
        outcome = 'test outcome',
        psStrategy = ''
      )
    )
    testthat::expect_true(!is.null(data()))
    
    testthat::expect_true(!is.null(output$table1Caption))
    
  })
