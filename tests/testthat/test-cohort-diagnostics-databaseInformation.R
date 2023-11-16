context("cohort-diagnostics-dbinfo")

shiny::testServer(databaseInformationModule, args = list(
  id = "test",
  dataSource = dataSourceCd,
  selectedDatabaseIds = shiny::reactive({ "Eunomia" })
), {
  getFilteredMetadataInformation()
  checkmate::expect_data_frame(getDatabaseInformation())

  checkmate::expect_class(output$databaseInformationTable, "json")

  res <- getExecutionMetadata(dataSourceCd, "Eunomia")
  checkmate::expect_data_frame(res)

})
