context('tests-helpers-emptyPlotly')

test_that("emptyPlotly works", {
ePlot <- emptyPlot()
expect_true("plotly" %in% class(ePlot))
})