context("cohort-method-KaplanMeier")

shiny::testServer(
  app = cohortMethodKaplanMeierServer, 
  args = list(
    selectedRow = shiny::reactiveVal(NULL), 
    connectionHandler = connectionHandlerCm, 
    resultDatabaseSettings = resultDatabaseSettingsCm
  ), 
  expr = {
    
    # should start null
    testthat::expect_true(is.null(kaplanMeierPlot()))
    
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
    testthat::expect_true(!is.null(kaplanMeierPlot()))
    
    testthat::expect_true(!is.null(output$kaplanMeierPlotPlotCaption))
    
  })
