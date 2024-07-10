context("estimation-cohort-method-attrition")

shiny::testServer(
  app = cohortMethodAttritionServer, 
  args = list(
    selectedRow = shiny::reactiveVal(
      NULL
    ), 
    connectionHandler = connectionHandlerEstimation, 
    resultDatabaseSettings = resultDatabaseSettingsEstimation
  ), 
  expr = {
    
    # should start null
    testthat::expect_true(is.null(attritionPlot()))
    
    # make sure this runs if we pick the first row
    selectedRow(
      list(
        databaseId = 'eunomia', 
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


test_that("Test cohortMethodAttritionViewer ui", {
  # Test ui
  ui <- cohortMethodAttritionViewer(id = 'cohortMethodAttritionViewer')
  checkmate::expect_list(ui)
})

# getCohortMethodAttrition
#drawCohortMethodAttritionDiagram(
#    attrition
#) 
