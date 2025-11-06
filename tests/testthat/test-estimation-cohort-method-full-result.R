context("estimation-cohort-method-full-result")

shiny::testServer(
  app = estimationCmFullResultServer, 
  args = list(
    connectionHandler = connectionHandlerCharacterization,
    resultDatabaseSettings = resultDatabaseSettingsCharacterization,
    selectedRow = shiny::reactiveVal(NULL),
    actionCount = shiny::reactiveVal(NULL)
  ), 
  expr = {
    
    selectedRow(
      data.frame(
        databaseId = '388020256', 
        databaseName = 'Synthea', 
        analysisId = 2,
        description  = 'madeup',
        targetName = 'Celecoxib',
        targetId = 1003,
        comparatorId = 2003, 
        comparatorName = 'Diclofenac',
        outcomeId = 3,
        outcomeName = 'GI bleed'
      )
    )
    
    testthat::expect_true(nrow(modifiedRow()) > 0)

  })


test_that("Test full ui", {
  # Test ui
  ui <- estimationCmFullResultViewer('test')
  checkmate::expect_list(ui)
})
