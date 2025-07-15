context("characterization-main")

shiny::testServer(
  app = characterizationServer, 
  args = list(
    connectionHandler = connectionHandlerCharacterization,
    resultDatabaseSettings = resultDatabaseSettingsCharacterization 
  ), 
  expr = {
    
    testthat::expect_true(inherits(connectionHandler,"ConnectionHandler"))
    
    # check initial settinfs
    testthat::expect_true(nrow(targetTable)>0)
    testthat::expect_true(is.null(outcomeTable()))
    testthat::expect_true(resultType() == "")
    
    
    # check reactiveTargetRow triggers outcome and tars
    reactiveTargetRow(targetTable[2,])
    
    # TODO fix below
    ##testthat::expect_true(nrow(outcomeTable())>0)
    
    # TODO: check when it is unselected that things react correctly
    

    # check the tab selector works
    session$setInputs(
      resultType = "Dechallenge Rechallenge"
      )
    testthat::expect_true(resultType() == "Dechallenge Rechallenge")
    # can we check output$showOutcomeSelector is reactive(TRUE)?
    #testthat::expect_true(resultType() == "Dechallenge Rechallenge")
    
    # can we check the UI changes?
    session$setInputs(
      resultType = "Cohort Incidence"
    )
    testthat::expect_true(resultType() == "Cohort Incidence")
    
    session$setInputs(
      resultType = "Database Comparison"
    )
    testthat::expect_true(resultType() == "Database Comparison")
    
    session$setInputs(
      resultType = "Cohort Comparison"
    )
    testthat::expect_true(resultType() == "Cohort Comparison")
    
    session$setInputs(
      resultType = "Time-to-event"
    )
    testthat::expect_true(resultType() == "Time-to-event")
    
    session$setInputs(
      resultType = "Risk Factors"
    )
    testthat::expect_true(resultType() == "Risk Factors")
    
    session$setInputs(
      resultType = "Case Series"
    )
    testthat::expect_true(resultType() == "Case Series")
    
  })


test_that("Test characterization ui", {
  # Test ui
  ui <- characterizationViewer(id = 'viewer')
  checkmate::expect_list(ui)
})

