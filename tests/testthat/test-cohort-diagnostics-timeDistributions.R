context("cohort-diagnostics-timeDistributions")

shiny::testServer(timeDistributionsModule, args = list(
  id = "testTimeDistributions", #Any string is ok?
  dataSource = dataSourceCd,
  selectedCohorts = shiny::reactive("Any String"),
  selectedDatabaseIds = shiny::reactive("Eunomia"),
  cohortIds = shiny::reactive({ c(18347) })
), {

  session$setInputs(selecatableTimeMeasures = c(
    "observation time (days) prior to index",
    "observation time (days) after index",
    "time (days) between cohort start and end"
  ))
  # Checking data type of each column of output matches what it should be
  checkmate::expect_numeric(timeDistributionData()$cohortId)
  checkmate::expect_character(timeDistributionData()$databaseId)
  checkmate::expect_character(timeDistributionData()$timeMetric)
  checkmate::expect_numeric(timeDistributionData()$averageValue)
  checkmate::expect_numeric(timeDistributionData()$standardDeviation)
  checkmate::expect_numeric(timeDistributionData()$minValue)
  checkmate::expect_numeric(timeDistributionData()$p10Value)
  checkmate::expect_numeric(timeDistributionData()$p25Value)
  checkmate::expect_numeric(timeDistributionData()$medianValue)
  checkmate::expect_numeric(timeDistributionData()$p75Value)
  checkmate::expect_numeric(timeDistributionData()$p90Value)
  checkmate::expect_numeric(timeDistributionData()$maxValue)

  checkmate::expect_class(output$timeDistributionTable, "json")
  checkmate::expect_class(output$timeDistributionPlot, "json")
})
