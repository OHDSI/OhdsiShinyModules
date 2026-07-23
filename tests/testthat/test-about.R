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
      moduleCardsHtml <- output$moduleCards$html
      testthat::expect_true(grepl("Data Sources", moduleCardsHtml, fixed = TRUE))
      testthat::expect_true(grepl("Cohorts", moduleCardsHtml, fixed = TRUE))
      testthat::expect_true(grepl("Characterization", moduleCardsHtml, fixed = TRUE))
      testthat::expect_true(grepl("Cohort Diagnostics", moduleCardsHtml, fixed = TRUE))
      testthat::expect_true(grepl("Estimation", moduleCardsHtml, fixed = TRUE))
      testthat::expect_true(grepl("Prediction", moduleCardsHtml, fixed = TRUE))
      testthat::expect_true(grepl("Report Generator", moduleCardsHtml, fixed = TRUE))
      testthat::expect_equal(length(regmatches(moduleCardsHtml, gregexpr("Not included", moduleCardsHtml, fixed = TRUE))[[1]]), 0)

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
      moduleCardsHtml <- output$moduleCards$html
      testthat::expect_true(grepl("Data Sources", moduleCardsHtml, fixed = TRUE))
      testthat::expect_true(grepl("Report Generator", moduleCardsHtml, fixed = TRUE))
      testthat::expect_equal(length(regmatches(moduleCardsHtml, gregexpr("Not included", moduleCardsHtml, fixed = TRUE))[[1]]), 7)
    })
  
})

test_that("Test about ui", {
  # Test ui
  ui <- aboutViewer()
  checkmate::expect_list(ui)
})
