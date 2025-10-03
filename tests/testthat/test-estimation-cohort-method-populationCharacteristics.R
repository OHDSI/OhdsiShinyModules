context("cohort-method-PopulationCharacteristics")

shiny::testServer(
  app = cohortMethodPopulationCharacteristicsServer, 
  args = list(
    selectedRow = shiny::reactiveVal(NULL), 
    connectionHandler = connectionHandlerCharacterization, 
    resultDatabaseSettings = resultDatabaseSettingsCharacterization
  ), 
  expr = {
    
    # make sure this runs if we pick the first row
    selectedRow(
      list(
        databaseId = '388020256', 
        databaseName = 'Synthea', 
        analysisId = 1,
        description  = 'madeup',
        targetName = 'Celecoxib',
        targetId = 1003,
        comparatorId = 2003, 
        comparatorName = 'Diclofenac',
        outcomeId = 3,
        outcomeName = 'GI bleed',
        psStrategy = ''
      )
    )
    testthat::expect_true(!is.null(data()))
    
    testthat::expect_true(!is.null(output$table1Caption))
    
  })
