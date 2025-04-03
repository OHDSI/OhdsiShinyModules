context("cohort-diagnostics-definition")

shiny::testServer(cohortDefinitionsModule, args = list(
  id = "testCdServer",
  dataSource = dataSourceCd,
  cohortDefinitions = shiny::reactive({ dataSourceCd$cohortTable })
), {
  getCohortDefinitionResolvedConceptsReactive()
  checkmate::expect_data_frame(cohortDefinitionTableData())
  defs <- getCdCohortRows(dataSourceCd, dataSourceCd$cohortTable$cohortId)
  checkmate::expect_list(getCirceRenderedExpression(defs$json[1]))

  checkmate::expect_list(getConceptSetDetailsFromCohortDefinition(defs$json[1]))

  testfile <- tempfile(fileext = ".zip")
  unlink(testfile)
  on.exit(unlink(testfile))

  exportCohortDefinitionsZip(defs, testfile)
  checkmate::expect_file_exists(testfile)

  checkmate::expect_data_frame(getCountForConceptIdInCohort(
    dataSource = dataSourceCd,
    databaseIds = dataSourceCd$dbTable$databaseId,
    cohortId = 14906
  ))
})
