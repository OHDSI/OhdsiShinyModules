context("estimation-cohort-method-plots")

test_that("Test cm plot ui", {
  # Test ui
  ui <- estimationCmPlotsViewer()
  checkmate::expect_list(ui)
})

cmData <- function()
{
  return(data.frame(
    databaseName = 1,
    cdmSourceAbbreviation = 'eunomia',
    target = 'target',
    comparator = 'comp',
    covariateName = 1,
    description = 'fgfgf',
    calibratedRr = 1.2,
    calibratedCi95Lb = 1.1,
    calibratedCi95Ub = 1.8
  ))
}


shiny::testServer(
  estimationCmPlotsServer, 
  args = list(
    id = "estimationCmPlotsServer",
    connectionHandler = connectionHandlerEstimation,
    resultDatabaseSettings = resultDatabaseSettingsEstimation,
    cmData = cmData
  ), {
  
 testthat::expect_true(!is.null(output$esCohortMethodPlot))
    
    # check NULL returned when data are empty
    data <- function()
    {
      return(data.frame())
    }
    result <- estimationCreateCmPlot(
      data
    )
    testthat::expect_true(is.null(result))
    
    data <- function()
    {
      return(data.frame(
        row = 1,
        calibratedRr = NA
      ))
    }
    result <- estimationCreateCmPlot(
      data
    )
    testthat::expect_true(is.null(result))
  
})

test_that("Test estimationCreateCmPlot", {

  data <- function()
  {
    return(data.frame(
      databaseName = 1,
      cdmSourceAbbreviation = 'eunomia',
      target = 'target',
      comparator = 'comp',
      covariateName = 1,
      description = 'fgfgf',
      calibratedRr = 1.2,
      calibratedCi95Lb = 1.1,
      calibratedCi95Ub = 1.8
    ))
  }
  result <- estimationCreateCmPlot(
    data
  )
  testthat::expect_true(inherits(result, 'grob'))
  
  
})
