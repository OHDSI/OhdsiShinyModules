context("cohortgenerator-main")

shiny::testServer(
  app = cohortGeneratorServer, 
  args = list(
    connectionHandler = connectionHandlerCG,
    resultDatabaseSettings = resultDatabaseSettingsCG
  ), 
  expr = {
    
    testthat::expect_true(inherits(connectionHandler,"ConnectionHandler"))
    
    
    testthat::expect_true(!is.null(output$cohortCounts))
    testthat::expect_true(!is.null(output$cohortGeneration))
    #testthat::expect_true(nrow(inclusionStats)>0)
    #testthat::expect_true(!is.null(output$inclusionsStats))
    
    
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


