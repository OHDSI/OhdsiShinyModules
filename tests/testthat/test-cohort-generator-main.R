context("cohort-generator-main")

shiny::testServer(
  app = cohortGeneratorServer, 
  args = list(
    connectionHandler = connectionHandlerCG,
    resultDatabaseSettings = resultDatabaseSettingsCG
  ), 
  expr = {
    
    testthat::expect_true(inherits(connectionHandler,"ConnectionHandler"))
    
    
    testthat::expect_true(!is.null(inputColsCohortCounts))
    testthat::expect_true(!is.null(data))
    testthat::expect_true(!is.null(dataGen))
    testthat::expect_true(!is.null(cohortDefData))
    
    testthat::expect_true(is.null(selectedCohortDefInputs()))
    testthat::expect_true(is.null(attritionData()))
    
    # set input$selectedCohortDefRow to 10
    session$setInputs(selectedCohortDefRow = 11)
    
    # test input$generate_cohort_def set to 1 causes trigger 
    session$setInputs(generate_cohort_def = 1)
    testthat::expect_true(!is.null(selectedCohortDefInputs()))
  
    # test input$generate_attrition set to 1 causes trigger
    session$setInputs(generate_attrition = 1)
    
    # test with a subset cohort
    session$setInputs(selectedCohortDefRow = 12)
    session$setInputs(generate_cohort_def = 1)
    
    
    
  })


test_that("Test cg ui", {
  # Test ui
  ui <- cohortGeneratorViewer('test_cg')
  checkmate::expect_list(ui)
  checkmate::expect_file_exists(cohortGeneratorHelperFile())
})



test_that("Test getCohortGeneratorCohortCounts ", {
  
  result <- getCohortGeneratorCohortCounts(
    connectionHandler = connectionHandlerCG, 
    resultDatabaseSettings = resultDatabaseSettingsCG
  )
  
  testthat::expect_true( nrow(result) > 0 )
  
})


test_that("Test getCohortGeneratorCohortMeta ", {
  
  result <- getCohortGeneratorCohortMeta(
    connectionHandler = connectionHandlerCG, 
    resultDatabaseSettings = resultDatabaseSettingsCG
  )
  
  testthat::expect_true( nrow(result) > 0 )
  
})


test_that("Test getCohortGeneratorCohortInclusionSummary ", {
  
  result <- getCohortGeneratorCohortInclusionSummary(
    connectionHandler = connectionHandlerCG, 
    resultDatabaseSettings = resultDatabaseSettingsCG
  )
  
  testthat::expect_true( nrow(result) > 0 )
  
})



test_that("Test getCohortGeneratorInclusionRules ", {
  
  result <- getCohortGeneratorInclusionRules(
    connectionHandler = connectionHandlerCG, 
    resultDatabaseSettings = resultDatabaseSettingsCG
  )
  
  testthat::expect_true( nrow(result) > 0 )
  
})


test_that("Test getCohortGeneratorInclusionStats ", {
  
  result <- getCohortGeneratorInclusionStats(
    connectionHandler = connectionHandlerCG, 
    resultDatabaseSettings = resultDatabaseSettingsCG
  )
  
  testthat::expect_true( nrow(result) > 0 )
  
})


if(F){
test_that("Test getCohortGenerationAttritionTable ", {
  
  result <- getCohortGenerationAttritionTable(
    rules = data.frame(
      cohortDefinitionId = 1,
      ruleSequence = 1, 
      ruleName = 'dfd', 
      cohortName = 'dfdf',
      inclusionRuleMask = 2,
      databaseId = 1,
      cdmSourceName = 'fdfd',
      modeId = 1,
      personCount = 10
      
    ),
    stats = data.frame(
      databaseId = 1,
      cdmSourceName = 'fdfd',
      modeId = 1,
      personCount = 5,
      cohortName = 'dfdf',
      cohortDefinitionId = 1,
      inclusionRuleMask = 2
    )
  )
  
  testthat::expect_true( nrow(result) > 0 )
  
})
}


