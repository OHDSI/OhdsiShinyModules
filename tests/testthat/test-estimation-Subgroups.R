context("estimation-Subgroups")


# tests cannot be done due to  getEstimationSubgroupResults() missing?
if(F){
shiny::testServer(
  app = estimationSubgroupsServer, 
  args = list(
    selectedRow = shiny::reactiveVal(NULL
      #data.frame(databaseId = '1', analysisId = 2, psStrategy = '', unblind = F,
      #           targetSubjects = 100, comparatorSubjects = 100,
      #           targetOutcomes = 10, comparatorOutcomes = 5,
      #           targetDays = 1000, comparatorDays = 1000)
    ), 
    inputParams = shiny::reactiveVal(list(
      target = 1,
      comparator = 2, 
      outcome = 3,
      database = 1,
      analysis = 1
      )), 
    connectionHandler = connectionHandlerEst, 
    exposureOfInterest = list(exposureId = c(1,2), exposureName = c(1,2)), 
    outcomeOfInterest = list(outcomeId = 3, outcomeName = 3)
    #resultsSchema = 'main', 
    #tablePrefix = 'cm_',
    #cohortTablePrefix = cohortTablePrefix, 
    #databaseTable = databaseTable
    #metaAnalysisDbIds = NULL
  ), 
  expr = {
    
    # check result table loads
    testthat::expect_true(is.null(interactionEffects()))
    
    # select first row 
    selectedRow(
      data.frame(databaseId = '1', analysisId = 2, psStrategy = '', unblind = F,
                            targetSubjects = 100, comparatorSubjects = 100,
                            targetOutcomes = 10, comparatorOutcomes = 5,
                            targetDays = 1000, comparatorDays = 1000)
    )
    # setting selectedRow() activates the following
    testthat::expect_true(!is.null(interactionEffects()))
    
    testthat::expect_true(!is.null(output$subgroupTableCaption))
    testthat::expect_true(!is.null(output$subgroupTable))
    
    # add code to test blind works
    
  })
  
}