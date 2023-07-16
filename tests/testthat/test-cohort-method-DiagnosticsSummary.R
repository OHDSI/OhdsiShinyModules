context("cohort-method-DiagnosticsSummary")

shiny::testServer(
  app = cohortMethodDiagnosticsSummaryServer, 
  args = list(
    connectionHandler = connectionHandlerCm, 
    resultDatabaseSettings = resultDatabaseSettingsCm
  ), 
  expr = {
    
    # should start null
    testthat::expect_true(!is.null(data))
    
  })

test_that("styleColumns", {
  
  oid <- c(1,34)
  names(oid) <- c('a','b')
  aid <- c(3)
  names(aid) <- c('none')
  
colss <- styleColumns(
    customColDefs = list(a=1),
    outcomeIds = oid, 
    analysisIds = aid
)

testthat::expect_is(colss, 'list')
names(colss) <- c('a', 'a_none', 'b_none')
testthat::expect_is(colss$b_none$style, 'function')
testthat::expect_equal(colss$b_none$style('Pass')$background,"#AFE1AF")
})

test_that("diagnosticSummaryFormat", {
  
  datar <- function(){
    data.frame(a=1,b=1,c=1,name ='name', summaryValue = 1)
  }
val <- diagnosticSummaryFormat(
    data = datar, 
    idCols = c('a','b', 'c'),
    namesFrom = c('name')
)

testthat::expect_true(nrow(val) == 1)
testthat::expect_true(ncol(val) == 4)

})

test_that("getCmDiagCohorts", {
  
cohortIds <- getCmDiagCohorts(
    connectionHandler = connectionHandlerCm,
    resultDatabaseSettings = resultDatabaseSettingsCm,
    type = 'target'
    )

testthat::expect_true(length(cohortIds) > 0)
})

test_that("getCmDiagAnalyses", {
  
  analysisIds <- getCmDiagAnalyses(
    connectionHandler = connectionHandlerCm,
    resultDatabaseSettings = resultDatabaseSettingsCm
  )
  
  testthat::expect_true(length(analysisIds) > 0)
})

test_that("getCmDiagAnalyses", {
  
  analysisIds <- getCmDiagAnalyses(
    connectionHandler = connectionHandlerCm,
    resultDatabaseSettings = resultDatabaseSettingsCm
  )
  
  cohortIds <- getCmDiagCohorts(
    connectionHandler = connectionHandlerCm,
    resultDatabaseSettings = resultDatabaseSettingsCm,
    type = 'target'
  )
  
  outcomeIds <- getCmDiagCohorts(
    connectionHandler = connectionHandlerCm,
    resultDatabaseSettings = resultDatabaseSettingsCm,
    type = 'outcome'
  )
  
  comparatorIds <- getCmDiagCohorts(
    connectionHandler = connectionHandlerCm,
    resultDatabaseSettings = resultDatabaseSettingsCm,
    type = 'comparator'
  )
  
diag <- getCmDiagnosticsData(
    connectionHandler = connectionHandlerCm,
    resultDatabaseSettings = resultDatabaseSettingsCm,
    targetIds = cohortIds,
    outcomeIds = outcomeIds,
    comparatorIds = comparatorIds,
    analysisIds = analysisIds
)

testthat::expect_true(nrow(diag) > 0)

})

