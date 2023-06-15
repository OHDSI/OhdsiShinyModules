context("phevaluator-main")

shiny::testServer(phevaluatorServer, args = list(
  id = "phevaluatorServer",
  connectionHandler = connectionHandlerPV,
  resultDatabaseSettings = resultDatabaseSettingsPV
), {
  #set inputs
  session$setInputs(
    phenotypes = c("hyperprolactinemia", "interstitialLungDisease"),
    databaseIds = c("CCAE_RS", "Amb EMR"),
    generate = T
  )
  
  #make sure the selection options are stored in an accesible df
  checkmate::expect_data_frame(optionCols)
  
  #make sure there is at least one selection for each input option
  checkmate::expect_character(unique(optionCols$databaseId), min.len = 1)
  checkmate::expect_character(unique(optionCols$phenotype), min.len = 1)
  
  #make sure all extracted data are accessible dfs
  checkmate::expect_data_frame(dataAlgorithmPerformance())
  checkmate::expect_data_frame(dataCohortDefinitionSet())
  checkmate::expect_data_frame(dataDiagnostics())
  checkmate::expect_data_frame(dataEvalInputParams())
  checkmate::expect_data_frame(dataModelCovars())
  checkmate::expect_data_frame(dataModelInputParams())
  checkmate::expect_data_frame(dataModelPerformance())
  checkmate::expect_data_frame(dataTestSubjects())
  checkmate::expect_data_frame(dataTestSubjectsCovars())
  
  #check that customColDefs are a list or that they are ser to null (no custom col defs specified)
  testthat::expect_true(class(customColDefs) == 'list' | is.null(customColDefs))
  
  #make sure all output tables work
  # output$algorithmPerformanceResultsTable
  # output$cohortDefinitionSetTable
  # output$diagnosticsTable
  # output$evaluationInputParametersTable
  # output$modelCovariatesTable
  # output$modelInputParametersTable
  # output$modelPerformanceTable
  # output$testSubjectsTable
  # output$testSubjectsCovariatesTable
  
})