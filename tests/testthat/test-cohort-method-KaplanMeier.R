context("cohort-method-KaplanMeier")

shiny::testServer(
  app = cohortMethodKaplanMeierServer, 
  args = list(
    selectedRow = shiny::reactiveVal(NULL), 
    inputParams = shiny::reactiveVal(list(
      target = 1,
      comparator = 2, 
      outcome = 3
      )), 
    connectionHandler = connectionHandlerCm, 
    resultDatabaseSettings = resultDatabaseSettingsCm,
    metaAnalysisDbIds = '1'
  ), 
  expr = {
    
    # should start null
    testthat::expect_true(is.null(kaplanMeierPlot()))
    
    # make sure this runs if we pick the first row
    selectedRow(list(databaseId = '1', analysisId = 2, psStrategy = ''))
    testthat::expect_true(!is.null(kaplanMeierPlot()))
    
    testthat::expect_true(!is.null(output$kaplanMeierPlotPlotCaption))
    
  })
