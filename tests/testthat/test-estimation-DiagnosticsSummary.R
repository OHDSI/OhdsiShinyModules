context("estimation-DiagnosticsSummary")

shiny::testServer(
  app = estimationDiagnosticsSummaryServer, 
  args = list(
    connectionHandler = connectionHandlerEst, 
    resultsSchema = 'main', 
    tablePrefix = 'cm_',
    cohortTablePrefix = resultDatabaseSettingsEst$cohortTablePrefix
  ), 
  expr = {
    
    # should start null
    testthat::expect_true(!is.null(data))
    
  })