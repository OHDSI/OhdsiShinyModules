context("estimation-TitlePanel")

# issues with this modules!

shiny::testServer(
  app = estimationTitlePanelServer, 
  args = list(), 
  expr = {
    
    # check result table loads
    testthat::expect_true(!is.null(output$titleText))
  
  })

