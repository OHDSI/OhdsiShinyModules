test_that("orphanConceptsModule loads", {
  shiny::testServer(orphanConceptsModule, args = list(
    id = "testOrphanConcepts",
    dataSource = dataSourceCd,
    selectedCohort = shiny::reactive("Any String"),
    selectedDatabaseIds = shiny::reactive("Eunomia"),
    targetCohortId = shiny::reactive({ c(14906) }),
    selectedConceptSets = shiny::reactiveVal(NULL),
    conceptSetIds = shiny::reactive({ c(0) })
  ), {
    ## input tests will go here
    session$setInputs(
      orphanConceptsType = 2
    )

  })
})
