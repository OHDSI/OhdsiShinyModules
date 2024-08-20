context("characterization-main")

shiny::testServer(
  app = characterizationServer, 
  args = list(
    connectionHandler = connectionHandlerCharacterization,
    resultDatabaseSettings = resultDatabaseSettingsCharacterization 
  ), 
  expr = {
    
    testthat::expect_true(inherits(connectionHandler,"ConnectionHandler"))
    
    testthat::expect_true(length(charTypes)>0)
    testthat::expect_true(length(options)>0)
    
    testthat::expect_equal(parentIndex(), 1)
    
    
    # check the target selection
    session$setInputs(
      targetId = parents[2],
      subTargetId = characterizationGetChildren(options,2)[1]
      )
    
    # generate
    session$setInputs(targetSelect = T)
    
    testthat::expect_true(inherits(targetSelected(),'data.frame'))
    testthat::expect_equal(subTargetId(), characterizationGetChildren(options,2)[1])
    testthat::expect_true(length(outcomes())>0)
    
    # check outcomeSelect
    session$setInputs(outcomeId = outcomes()[1])
    session$setInputs(outcomeSelect = T)
    testthat::expect_true(inherits(outcomeSelected(),'data.frame'))
    testthat::expect_true(nrow(outcomeSelected()) == 1)
    testthat::expect_equal(outcomeId(), outcomes()[1])
    
    # check mainPanel
    testthat::expect_true(mainPanel() == 'None')
    testthat::expect_true(!"Cohort Comparison" %in% previouslyLoaded())
    session$setInputs(
      mainPanel = "Cohort Summary",
      cohortSummaryPanel = 'Cohort Comparison'
      )
    testthat::expect_true(mainPanel() == "Cohort Summary")
    testthat::expect_true(cohortSummaryPanel() == 'Cohort Comparison')
    testthat::expect_true("Cohort Comparison" %in% previouslyLoaded())
    
    session$setInputs(
      mainPanel = "Cohort Summary",
      cohortSummaryPanel = 'Database Comparison'
    )
    testthat::expect_true(cohortSummaryPanel() == 'Database Comparison')
    testthat::expect_true('Database Comparison' %in% previouslyLoaded())
    
    session$setInputs(
      mainPanel = "Exposed Cases Summary",
      exposedCasesPanel = "Risk Factor"
    )
    testthat::expect_true(currentTab() == "Risk Factor")
    testthat::expect_true("Risk Factor" %in% previouslyLoaded())
    
    session$setInputs(
      mainPanel = "Exposed Cases Summary",
      exposedCasesPanel = 'Case Series'
    )
    testthat::expect_true(currentTab() == 'Case Series')
    testthat::expect_true('Case Series' %in% previouslyLoaded())
    
    session$setInputs(
      mainPanel = "Exposed Cases Summary",
      exposedCasesPanel = 'Time-to-event'
    )
    testthat::expect_true(currentTab() == 'Time-to-event')
    testthat::expect_true('Time-to-event' %in% previouslyLoaded())
    
    session$setInputs(
      mainPanel = "Exposed Cases Summary",
      exposedCasesPanel = 'Dechallenge Rechallenge'
    )
    testthat::expect_true(currentTab() == 'Dechallenge Rechallenge')
    testthat::expect_true('Dechallenge Rechallenge' %in% previouslyLoaded())
    
    session$setInputs(
      mainPanel = "Cohort Incidence"
    )
    testthat::expect_true(currentTab() == "Cohort Incidence")
    testthat::expect_true("Incidence Results" %in% previouslyLoaded())
    
    
  })


test_that("Test characterization ui", {
  # Test ui
  ui <- characterizationViewer(id = 'viewer')
  checkmate::expect_list(ui)
})

test_that("getCharacterizationTypes", {

  types <- getCharacterizationTypes(
    connectionHandler = connectionHandlerCharacterization,
    resultDatabaseSettings = resultDatabaseSettingsCharacterization
  )
  
  testthat::expect_is(types$mainPanel, 'character')
  testthat::expect_is(types$subPanel, 'character')
  
})

test_that("characterizationGetOptions", {
options <- characterizationGetOptions(
  connectionHandler = connectionHandlerCharacterization,
  resultDatabaseSettings = resultDatabaseSettingsCharacterization,
    includeAggregate = T,
    includeIncidence = T
)

testthat::expect_true(inherits(options, 'list'))
testthat::expect_true(sum(c('cohortName','cohortId','children') %in% names(options[[1]])) == 3)

})


test_that("characterizationGetParents", {
  options <- characterizationGetOptions(
    connectionHandler = connectionHandlerCharacterization,
    resultDatabaseSettings = resultDatabaseSettingsCharacterization,
    includeAggregate = T,
    includeIncidence = T
  )
  
  parents <- characterizationGetParents(options)
  testthat::expect_true(inherits(parents, 'numeric'))
  testthat::expect_true(length(names(parents)) == length(parents))
  
  testthat::expect_true(length(options) == length(parents))
})

test_that("characterizationGetChildren", {
  options <- characterizationGetOptions(
    connectionHandler = connectionHandlerCharacterization,
    resultDatabaseSettings = resultDatabaseSettingsCharacterization,
    includeAggregate = T,
    includeIncidence = T
  )
  
  children <- characterizationGetChildren(options, 1)
  testthat::expect_true(length(children) > 0 )
  testthat::expect_true(inherits(children, 'numeric'))
  testthat::expect_true(length(names(children)) == length(children))

})

test_that("characterizationGetOutcomes", {
  options <- characterizationGetOptions(
    connectionHandler = connectionHandlerCharacterization,
    resultDatabaseSettings = resultDatabaseSettingsCharacterization,
    includeAggregate = T,
    includeIncidence = T
  )
  
  outcomes <- characterizationGetOutcomes(options, 1)
  testthat::expect_true(inherits(outcomes, 'numeric'))
  testthat::expect_true(length(names(outcomes)) == length(outcomes))
  
  })
