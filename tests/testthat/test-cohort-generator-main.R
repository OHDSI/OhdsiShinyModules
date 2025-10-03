context("cohort-generator-main")

shiny::testServer(
  app = cohortGeneratorServer, 
  args = list(
    connectionHandler = connectionHandlerCharacterization,
    resultDatabaseSettings = resultDatabaseSettingsCharacterization
  ), 
  expr = {
    
    testthat::expect_true(inherits(connectionHandler,"ConnectionHandler"))
    
    testthat::expect_true(!is.null(data))
    # cohortCountsColDefs
    testthat::expect_true(!is.null(dataGen))
    # cohortGenerationColDefs
    testthat::expect_true(!is.null(cohortDefData))
    
    testthat::expect_true(is.null(selectedCohortDefInputs()))
    testthat::expect_true(is.null(attritionData()))
    
    # set input$selectedCohortDefRow to 10
    session$setInputs(selectedCohortDefRow = 2)
    
    # test input$generate_cohort_def set to 1 causes trigger 
    session$setInputs(generate_cohort_def = 1)
    testthat::expect_true(!is.null(selectedCohortDefInputs()))
  
    # test input$generate_attrition set to 1 causes trigger
    session$setInputs(generate_attrition = 1)
    
    # test with a subset cohort
    session$setInputs(selectedCohortDefRow = 2)
    session$setInputs(generate_cohort_def = 3)
    
  })


test_that("Test cg ui", {
  # Test ui
  ui <- cohortGeneratorViewer('test_cg')
  checkmate::expect_list(ui)
  checkmate::expect_file_exists(cohortGeneratorHelperFile())
})



test_that("Test getCohortGeneratorCohortCounts ", {
  
  result <- getCohortGeneratorCohortCounts(
    connectionHandler = connectionHandlerCharacterization, 
    resultDatabaseSettings = resultDatabaseSettingsCharacterization
  )
  
  testthat::expect_is(result, 'data.frame')
  
})


test_that("Test getCohortGeneratorCohortMeta ", {
  
  result <- getCohortGeneratorCohortMeta(
    connectionHandler = connectionHandlerCharacterization, 
    resultDatabaseSettings = resultDatabaseSettingsCharacterization
  )
  
  testthat::expect_is(result, 'data.frame')
  
})


test_that("Test getCohortGeneratorCohortInclusionSummary ", {
  
  result <- getCohortGeneratorCohortInclusionSummary(
    connectionHandler = connectionHandlerCharacterization, 
    resultDatabaseSettings = resultDatabaseSettingsCharacterization
  )
  
  testthat::expect_is(result, 'data.frame')
  
})



test_that("Test getCohortGeneratorInclusionRules ", {
  
  result <- getCohortGeneratorInclusionRules(
    connectionHandler = connectionHandlerCharacterization, 
    resultDatabaseSettings = resultDatabaseSettingsCharacterization
  )
  
  testthat::expect_is(result, 'data.frame')
  
})


test_that("Test getCohortGeneratorInclusionStats ", {
  
  result <- getCohortGeneratorInclusionStats(
    connectionHandler = connectionHandlerCharacterization, 
    resultDatabaseSettings = resultDatabaseSettingsCharacterization
  )
  
  testthat::expect_is(result, 'data.frame')
  
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


