context("estimation-cohort-method-plots")

test_that("Test cm plot ui", {
  # Test ui
  ui <- estimationCmPlotsViewer()
  checkmate::expect_list(ui)
})

cmData <- function()
{
  return(data.frame(
    databaseId = '388020256', 
    databaseName = 'Synthea', 
    analysisId = 1,
    description  = 'madeup',
    targetName = 'Celecoxib',
    targetId = 1003,
    comparatorId = 2003, 
    comparatorName = 'Diclofenac',
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
    connectionHandler = connectionHandlerCharacterization,
    resultDatabaseSettings = resultDatabaseSettingsCharacterization,
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
      databaseId = 1,
      databaseName = 'eunomia',
      targetName = 'target',
      comparatorName = 'comp',
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
