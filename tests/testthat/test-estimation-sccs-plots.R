context("estimation-sccs-plots")

test_that("Test sccs diagnostic ui", {
  # Test ui
  ui <- estimationSccsPlotsViewer()
  checkmate::expect_list(ui)
})

sccsData <- function()
{
  return(data.frame(
    databaseName = 1,
    covariateName = 1,
    indication = 1,
    description = 'fgfgf',
    calibratedRr = 1.2,
    calibratedCi95Lb = 1.1,
    calibratedCi95Ub = 1.8
  ))
}


shiny::testServer(
  estimationSccsPlotsServer, 
  args = list(
    id = "estimationSccsPlotsServer",
    connectionHandler = connectionHandlerEstimation,
    resultDatabaseSettings = resultDatabaseSettingsEstimation,
    sccsData = sccsData
  ), {
  
 testthat::expect_true(!is.null(output$esSccsPlot))
    
    # check NULL returned when data are empty
    data <- function()
    {
      return(data.frame())
    }
    result <- estimationCreateSccsPlot(
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
    result <- estimationCreateSccsPlot(
      data
    )
    testthat::expect_true(is.null(result))
  
})

test_that("Test estimationCreateSccsPlot", {

  data <- function()
  {
    return(data.frame(
      databaseName = 1,
      covariateName = 1,
      indication = 1,
      description = 'fgfgf',
      calibratedRr = 1.2,
      calibratedCi95Lb = 1.1,
      calibratedCi95Ub = 1.8
    ))
  }
  result <- estimationCreateSccsPlot(
    data
  )
  testthat::expect_true(inherits(result, 'grob'))
  
  
})