context("about")

test_that("about server works", {
  
  shiny::testServer(
    app = aboutServer, 
    args = list(
      connectionHandler = NULL,
      resultDatabaseSettings = NULL,
      config = list(
        shinyModules = list(
          list(
            id = 'about',
          tabName = "About",
          tabText = "About",
          shinyModulePackage = "OhdsiShinyModules",
          uiFunction = "aboutViewer",
          serverFunction = "aboutServer"
        ), 
        list(
          tabName = "DataSources"
        ),
        list(
          tabName = "Cohorts"
        ),
        list(
          tabName = "Characterization"
        ),
        
        list(
          tabName = "CohortDiagnostics"
        ),
        list(
          tabName = "Estimation"
        ),
        list(
          tabName = "Prediction"
        ),
        list(
          tabName = "SCCS"
        ),
        list(
          tabName = "Meta"
        ),
        list(
          tabName = "Report"
        )
      ))
    ), 
    expr = {
      
      testthat::expect_true(sum(tab_names %in% c(
        "About", "DataSources", "Cohorts", "Characterization",
        "CohortDiagnostics", "Estimation", "Prediction", "SCCS",
        "Meta", "Report")) == length(tab_names))

    })
  
})


test_that("about server works no modules", {
  
  shiny::testServer(
    app = aboutServer, 
    args = list(
      connectionHandler = NULL,
      resultDatabaseSettings = NULL,
      config = list(
        shinyModules = list(
          list(
            id = 'about',
            tabName = "About",
            tabText = "About",
            shinyModulePackage = "OhdsiShinyModules",
            uiFunction = "aboutViewer",
            serverFunction = "aboutServer"
          )
        ))
    ), 
    expr = {
      
      testthat::expect_true(tab_names == c("About"))
      
    })
  
})

test_that("Test about ui", {
  # Test ui
  ui <- aboutViewer()
  checkmate::expect_list(ui)
})
