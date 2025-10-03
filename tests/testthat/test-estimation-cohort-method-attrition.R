context("estimation-cohort-method-attrition")

shiny::testServer(
  app = cohortMethodAttritionServer, 
  args = list(
    selectedRow = shiny::reactiveVal(
      NULL
    ), 
    connectionHandler = connectionHandlerCharacterization, 
    resultDatabaseSettings = resultDatabaseSettingsCharacterization
  ), 
  expr = {
    
    # should start null
    testthat::expect_true(is.null(attritionPlot()))
    
    # make sure this runs if we pick the first row
    selectedRow(
      list(
        databaseId = '388020256', 
        cdmSourceAbbreviation = 'Synthea', 
        analysisId = 1,
        description  = 'madeup',
        target = 'Celecoxib',
        targetId = 1003,
        comparatorId = 2003, 
        comparator = 'Diclofenac',
        outcomeId = 3,
        outcome = 'GI bleed'
        )
      )
    testthat::expect_true(!is.null(attritionPlot()))
    
  })


test_that("Test cohortMethodAttritionViewer ui", {
  # Test ui
  ui <- cohortMethodAttritionViewer(id = 'cohortMethodAttritionViewer')
  checkmate::expect_list(ui)
})

# getCohortMethodAttrition
#drawCohortMethodAttritionDiagram(
#    attrition
#) 
