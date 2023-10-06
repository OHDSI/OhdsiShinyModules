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

test_that("getCmDiagnosticData", {
colDefs <- getColDefsCmDiag(
  connectionHandler = connectionHandlerCm,
  resultDatabaseSettings = resultDatabaseSettingsCm
)

testthat::expect_is(colDefs, 'list')
testthat::expect_is(colDefs[[1]], 'colDef')

})

test_that("getCmDiagnosticData", {
  
  inputSelected <- function(){
    return(
      list(
        targetIds = 1,
        comparatorIds = 2,
        outcomeIds = 3,
        analysesIds = c(1,2)
      )
    )
    }

  
diag <- getCmDiagnosticsData(
    connectionHandler = connectionHandlerCm,
    resultDatabaseSettings = resultDatabaseSettingsCm,
    inputSelected
)

testthat::expect_true(nrow(diag) > 0)

})

