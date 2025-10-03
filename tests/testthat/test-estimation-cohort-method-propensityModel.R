context("cohort-method-propensityModel")

test_that("Test cohortMethodPropensityModelViewer ui", {
  # Test ui
  ui <- cohortMethodPropensityModelViewer(id = 'cohortMethodPropensityModelViewer')
  checkmate::expect_list(ui)
})

shiny::testServer(
  app = cohortMethodPropensityModelServer, 
  args = list(
    selectedRow = shiny::reactiveVal(NULL), 
    connectionHandler = connectionHandlerCharacterization, 
    resultDatabaseSettings = resultDatabaseSettingsCharacterization
  ), 
  expr = {
    
    #testthat::expect_true(is.null(output$powerTable))
    
    # make sure this runs if we pick the first row
    selectedRow(
      list(
        databaseId = '388020256', 
        databaseName = 'Synthea', 
        analysisId = 2,
        description  = 'madeup',
        targetName = 'Celecoxib',
        targetId = 1003,
        comparatorId = 2003, 
        comparatorName = 'Diclofenac',
        outcomeId = 3,
        outcomeName = 'GI bleed',
        psStrategy = '',
        analysisId = 2, 
        psStrategy = '', 
        unblind = F,
        targetSubjects = 100, 
        comparatorSubjects = 100,
        targetOutcomes = 10, 
        comparatorOutcomes = 5,
        targetDays = 1000, 
        comparatorDays = 1000
      )
    )
    testthat::expect_true(!is.null(data()))
    
    
  })
