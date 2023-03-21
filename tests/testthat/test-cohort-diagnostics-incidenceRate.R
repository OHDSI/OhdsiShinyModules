context("cohort-diagnostics-incidence")

test_that("plot works", {
  # just a small subset of data
  tData <- read.csv2(file.path("..", "resources", "cdDatabase", "ir_test_data.csv")) %>% tibble::tibble()
  plt <- plotIncidenceRate(data = tData,
                           cohortTable = dataSourceCd$cohortTable,
                           stratifyByGender = TRUE,
                           stratifyByCalendarYear = TRUE,
                           yscaleFixed = TRUE,
                           stratifyByAgeGroup = TRUE)

  checkmate::expect_class(plt, "plotly")
})

shiny::testServer(incidenceRatesModule, args = list(
  id = "testIncidenceRates", #Any string is ok?
  dataSource = dataSourceCd,
  cohortTable = getCohortTable(dataSourceCd),
  selectedCohorts = shiny::reactive("Any String"),
  selectedDatabaseIds = shiny::reactive("Eunomia"),
  cohortIds = shiny::reactive({ c(18347) })
), {
  ## input tests will go here
  session$setInputs(
    irStratification = c("Age", "Calendar Year", "Sex"),
    minPersonYear = 0,
    minSubjectCount = 0,
    generatePlot = 1,
    incidenceRateCalenderFilter = c(2010, 2019)
  )

  checkmate::expect_data_frame(incidenceRateData())
  checkmate::expect_integerish(incidenceRateCalenderFilter())
})
