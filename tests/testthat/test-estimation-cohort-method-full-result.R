context("estimation-cohort-method-full-result")

shiny::testServer(
  app = estimationCmFullResultServer, 
  args = list(
    connectionHandler = connectionHandlerEstimation,
    resultDatabaseSettings = resultDatabaseSettingsEstimation,
    selectedRow = shiny::reactiveVal(NULL),
    actionCount = shiny::reactiveVal(NULL)
  ), 
  expr = {
    
    selectedRow(
      data.frame(
        databaseId = 'eunomia', 
        databaseName = 'Eunomia', 
        analysisId = 2,
        description  = 'madeup',
        targetName = 'test target',
        targetId = 1,
        comparatorId = 2, 
        comparatorName = 'test comparator',
        outcomeId = 3,
        outcomeName = 'test outcome'
      )
    )
    
    testthat::expect_true(nrow(modifiedRow()) > 0)

  })


test_that("Test full ui", {
  # Test ui
  ui <- estimationCmFullResultViewer('test')
  checkmate::expect_list(ui)
})
