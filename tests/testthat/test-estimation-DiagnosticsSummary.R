context("estimation-DiagnosticsSummary")

shiny::testServer(
  app = estimationDiagnosticsSummaryServer, 
  args = list(
    connection = connectionEst, 
    resultsSchema = 'main', 
    tablePrefix = 'cm_',
    cohortTablePrefix = cohortTablePrefix
  ), 
  expr = {
    
    # should start null
    testthat::expect_true(!is.null(data))
    
  })
