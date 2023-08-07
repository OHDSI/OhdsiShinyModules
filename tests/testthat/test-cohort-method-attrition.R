context("cohort-method-attrition")

shiny::testServer(
  app = cohortMethodAttritionServer, 
  args = list(
    selectedRow = shiny::reactiveVal(
      NULL
    ), 
    connectionHandler = connectionHandlerCm, 
    resultDatabaseSettings = resultDatabaseSettingsCm
  ), 
  expr = {
    
    # should start null
    testthat::expect_true(is.null(attritionPlot()))
    
    # make sure this runs if we pick the first row
    selectedRow(
      list(
        databaseId = '1', 
        cdmSourceAbbreviation = 'Eunomia', 
        analysisId = 2,
        description  = 'madeup',
        target = 'test target',
        targetId = 1,
        comparatorId = 2, 
        comparator = 'test comparator',
        outcomeId = 3,
        outcome = 'test outcome'
        )
      )
    testthat::expect_true(!is.null(attritionPlot()))
    
  })
