context("estimation-cohort-method-KaplanMeier")

shiny::testServer(
  app = cohortMethodKaplanMeierServer, 
  args = list(
    selectedRow = shiny::reactiveVal(NULL), 
    connectionHandler = connectionHandlerEstimation, 
    resultDatabaseSettings = resultDatabaseSettingsEstimation
  ), 
  expr = {
    
    # should start null
    testthat::expect_true(is.null(kaplanMeierPlot()))
    
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
    testthat::expect_true(!is.null(kaplanMeierPlot()))
    
    testthat::expect_true(!is.null(output$kaplanMeierPlotPlotCaption))
    
  })
