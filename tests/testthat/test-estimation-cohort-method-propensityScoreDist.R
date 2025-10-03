context("cohort-method-PropensityScoreDist")

test_that("Test cohortMethodPropensityScoreDistViewer ui", {
  # Test ui
  ui <- cohortMethodPropensityScoreDistViewer(id = 'cohortMethodPropensityScoreDistViewer')
  checkmate::expect_list(ui)
})


shiny::testServer(
  app = cohortMethodPropensityScoreDistServer, 
  args = list(
    selectedRow = shiny::reactiveVal(NULL), 
    connectionHandler = connectionHandlerCharacterization, 
    resultDatabaseSettings = resultDatabaseSettingsCharacterization
  ), 
  expr = {
    
    testthat::expect_true(is.null(psDistPlot()))
    
    ps <- getCohortMethodPs(
      connectionHandler = connectionHandlerCharacterization, 
      resultDatabaseSettings = resultDatabaseSettingsCharacterization,
      targetId = 1002, 
      comparatorId = 2002, 
      analysisId = 2, 
      databaseId = '388020256'
    )
    
    testthat::expect_true('preferenceScore' %in% colnames(ps))
    testthat::expect_true('targetDensity' %in% colnames(ps))
    testthat::expect_true('comparatorDensity' %in% colnames(ps))
    
    # make sure this runs if we pick the first row
    selectedRow(
      list(
        databaseId = '388020256', 
        databaseName = 'Synthea', 
        description  = 'madeup',
        targetName = 'Celecoxib',
        targetId = 1002,
        comparatorId = 2002, 
        comparatorName = 'Diclofenac',
        outcomeId = 3,
        outcomeName = 'GI bleed',
        psStrategy = '',
        analysisId = 2, 
        psStrategy = '', 
        unblind = 0,
        targetSubjects = 100, 
        comparatorSubjects = 100,
        targetOutcomes = 10, 
        comparatorOutcomes = 5,
        targetDays = 1000, 
        comparatorDays = 1000
      )
    )
    
    testthat::expect_true(!is.null(psDistPlot()))
    
    
  })
