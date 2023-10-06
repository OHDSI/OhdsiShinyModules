context("cohort-method-full-result")

shiny::testServer(
  app = cohortMethodFullResultServer, 
  args = list(
    connectionHandler = connectionHandlerCm,
    resultDatabaseSettings = resultDatabaseSettingsCm,
    selectedRow = shiny::reactiveVal(NULL)
  ), 
  expr = {
    
    selectedRow(
      list(
        databaseId = '1', 
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

  })


test_that("Test full ui", {
  # Test ui
  ui <- cohortMethodFullResultViewer('test')
  checkmate::expect_list(ui)
})
