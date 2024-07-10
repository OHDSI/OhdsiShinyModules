context("report-main")

shiny::testServer(reportServer, args = list(
  id = "testReportServer",
  connectionHandler = connectionHandlerEstimation,
  resultDatabaseSettings = resultDatabaseSettingsEstimation#,
  #server = Sys.getenv("RESULTS_SERVER"), 
  #username = Sys.getenv("RESULTS_USER"), 
  #password = Sys.getenv("RESULTS_PASSWORD"), 
  #dbms = "postgresql"
), {
  
  # input$cmTargetNext
  # input$cmTargetPrevious
  # input$comparatorNext
  # input$comparatorPrevious
  # input$outcomeNext
  # input$outcomePrevious
  # input$generatePrevious
  
  session$setInputs(cmTargetNext = TRUE)
  session$setInputs(cmTargetPrevious = TRUE)
  
})

test_that("Test report ui", {
  # Test ui
  ui <- reportViewer()
  checkmate::expect_list(ui)
  checkmate::expect_file_exists(reportHelperFile())
})
