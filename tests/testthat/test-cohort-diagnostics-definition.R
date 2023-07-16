context("cohort-diagnostics-definition")

shiny::testServer(cohortDefinitionsModule, args = list(
  id = "testCdServer",
  dataSource = dataSourceCd,
  cohortDefinitions = shiny::reactive({ dataSourceCd$cohortTable })
), {
  getCohortDefinitionResolvedConceptsReactive()
  checkmate::expect_data_frame(cohortDefinitionTableData())
  defs <- getCohortJsonSql(dataSourceCd, dataSourceCd$cohortTable$cohortId)
  def <- defs$json[1] %>% RJSONIO::fromJSON(digits = 23)
  checkmate::expect_list(getCirceRenderedExpression(def))

  checkmate::expect_list(getConceptSetDetailsFromCohortDefinition(def))

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