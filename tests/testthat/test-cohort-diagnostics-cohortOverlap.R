context("cohort-diagnostics-overlap")

shiny::testServer(cohortOverlapModule, args = list(
  id = "testCohortOverlap", #Any string is ok?
  dataSource = dataSourceCd,
  selectedCohorts = shiny::reactive({ c(18349, 18350) }),
  selectedDatabaseIds = shiny::reactive("Eunomia"),
  targetCohortId = shiny::reactive({ c(18347) }),
  cohortIds = shiny::reactive({ c(18349, 18350) }),
  cohortTable = getCohortTable(dataSourceCd)
), {
  session$setInputs(showCohortIds = TRUE, showAsPercentage = TRUE, timeId = 1)

  # Just checking to make sure all the input data is following the correct variable types
  checkmate::expect_character(cohortOverlapData()$databaseId)
  checkmate::expect_numeric(cohortOverlapData()$comparatorCohortId)
  checkmate::expect_numeric(cohortOverlapData()$eitherSubjects)
  checkmate::expect_numeric(cohortOverlapData()$tOnlySubjects)
  checkmate::expect_numeric(cohortOverlapData()$cOnlySubjects)
  checkmate::expect_numeric(cohortOverlapData()$bothSubjects)
  checkmate::expect_numeric(cohortOverlapData()$targetCohortId)
  checkmate::expect_class(output$overlapTable, "json")
})
