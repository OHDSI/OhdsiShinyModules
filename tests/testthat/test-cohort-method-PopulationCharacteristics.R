context("cohort-method-PopulationCharacteristics")

shiny::testServer(
  app = cohortMethodPopulationCharacteristicsServer, 
  args = list(
    selectedRow = shiny::reactiveVal(NULL), 
    connectionHandler = connectionHandlerCm, 
    resultDatabaseSettings = resultDatabaseSettingsCm
  ), 
  expr = {
    
    # make sure this runs if we pick the first row
    selectedRow(
      list(
        databaseId = '1', 
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
    testthat::expect_true(!is.null(output$table1Table))
    
    testthat::expect_true(!is.null(output$table1Caption))
    
  })
