context("estimation-cohort-method-Power")

test_that("Test cohortMethodPowerViewer ui", {
  # Test ui
  ui <- cohortMethodPowerViewer(id = 'cohortMethodPowerViewer')
  checkmate::expect_list(ui)
})

shiny::testServer(
  app = cohortMethodPowerServer, 
  args = list(
    selectedRow = shiny::reactiveVal(NULL), 
    connectionHandler = connectionHandlerCharacterization, 
    resultDatabaseSettings = resultDatabaseSettingsCharacterization
  ), 
  expr = {
    
    #testthat::expect_true(is.null(output$powerTable))
    
    followUp <- getCmFollowUpDist(
      connectionHandler = connectionHandlerCharacterization,
      resultDatabaseSettings = resultDatabaseSettingsCharacterization,
      targetId = 1003,
      comparatorId = 2003,
      outcomeId = 3,
      databaseId = '388020256',
      analysisId = 2
    )
    testthat::expect_true(nrow(followUp)>0)
    
    tablet <- prepareCohortMethodFollowUpDistTable(followUp)
    testthat::expect_true(nrow(tablet)>0)
    
    # make sure this runs if we pick the first row
    selectedRow(
      data.frame(
        databaseId = '388020256', 
        databaseName = 'Synthea', 
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
    testthat::expect_true(!is.null(powerTable))
    testthat::expect_true(!is.null(output$powerTableCaption))
    testthat::expect_true(!is.null(output$timeAtRiskTableCaption))
    testthat::expect_true(!is.null(timeAtRiskTable))
    
    
  })
