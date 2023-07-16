context("cohort-method-attrition")

shiny::testServer(
  app = cohortMethodAttritionServer, 
  args = list(
    selectedRow = shiny::reactiveVal(NULL), 
    inputParams = shiny::reactiveVal(list(
      target = 1,
      comparator = 2, 
      outcome = 3
      )), 
    connectionHandler = connectionHandlerCm, 
    resultDatabaseSettings = resultDatabaseSettingsCm
  ), 
  expr = {
    
    # should start null
    testthat::expect_true(is.null(attritionPlot()))
    
    # make sure this runs if we pick the first row
    selectedRow(list(databaseId = 'Eunomia', analysisId = 1))
    testthat::expect_true(!is.null(attritionPlot()))
    
  })
