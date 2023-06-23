context("cohort-generator-main")

shiny::testServer(
  app = cohortGeneratorServer, 
  args = list(
    connectionHandler = connectionHandlerCG,
    resultDatabaseSettings = list(
      dbms = 'sqlite',
      tablePrefix = 'cg_',
      cohortTablePrefix = 'cg_',
      databaseTable = 'DATABASE_META_DATA',
      schema = 'main',
      tempEmulationSchema = NULL
    )
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
    resultsSchema = 'main',
    tablePrefix = 'cg_',
    databaseTable = 'DATABASE_META_DATA',
    databaseTablePrefix = ''
  )
  
  testthat::expect_true( nrow(result) > 0 )
  
})


test_that("Test getCohortGeneratorCohortMeta ", {
  
  result <- getCohortGeneratorCohortMeta(
    connectionHandler = connectionHandlerCG, 
    resultsSchema = 'main',
    tablePrefix = 'cg_'
  )
  
  testthat::expect_true( nrow(result) > 0 )
  
})


test_that("Test getCohortGeneratorCohortInclusionSummary ", {
  
  result <- getCohortGeneratorCohortInclusionSummary(
    connectionHandler = connectionHandlerCG, 
    resultsSchema = 'main',
    tablePrefix = 'cg_',
    databaseTable = 'DATABASE_META_DATA',
    databaseTablePrefix = ''
  )
  
  testthat::expect_true( nrow(result) > 0 )
  
})



test_that("Test getCohortGeneratorInclusionRules ", {
  
  result <- getCohortGeneratorInclusionRules(
    connectionHandler = connectionHandlerCG, 
    resultsSchema = 'main',
    tablePrefix = 'cg_'
  )
  
  testthat::expect_true( nrow(result) > 0 )
  
})


test_that("Test getCohortGeneratorInclusionStats ", {
  
  result <- getCohortGeneratorInclusionStats(
    connectionHandler = connectionHandlerCG, 
    resultsSchema = 'main',
    tablePrefix = 'cg_',
    databaseTable = 'DATABASE_META_DATA',
    databaseTablePrefix = ''
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


