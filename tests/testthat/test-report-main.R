context("report-main")

shiny::testServer(reportServer, args = list(
  id = "testReportServer",
  connectionHandler = connectionHandlerCharacterization,
  resultDatabaseSettings = resultDatabaseSettingsCharacterization#,
  #server = Sys.getenv("RESULTS_SERVER"), 
  #username = Sys.getenv("RESULTS_USER"), 
  #password = Sys.getenv("RESULTS_PASSWORD"), 
  #dbms = "postgresql"
), {
  
  session$setInputs(reportToShow = "Full Report")
  
  session$setInputs(reportToShow = "Prediction Report")
  
})

test_that("Test report ui", {
  # Test ui
  ui <- reportViewer()
  checkmate::expect_list(ui)
  checkmate::expect_file_exists(reportHelperFile())
})
