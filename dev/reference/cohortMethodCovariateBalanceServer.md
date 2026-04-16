# The module server for rendering the covariate balance plot

The module server for rendering the covariate balance plot

## Usage

``` r
cohortMethodCovariateBalanceServer(
  id,
  selectedRow,
  connectionHandler,
  resultDatabaseSettings,
  metaAnalysisDbIds = NULL
)
```

## Arguments

- id:

  the unique reference id for the module

- selectedRow:

  the selected row from the main results table

- connectionHandler:

  the connection to the PLE results database

- resultDatabaseSettings:

  a list containing the result schema and prefixes

- metaAnalysisDbIds:

  metaAnalysisDbIds

## Value

the PLE covariate balance content server

## See also

Other Estimation:
[`cohortMethodAttritionServer()`](cohortMethodAttritionServer.md),
[`cohortMethodAttritionViewer()`](cohortMethodAttritionViewer.md),
[`cohortMethodCovariateBalanceViewer()`](cohortMethodCovariateBalanceViewer.md),
[`cohortMethodKaplanMeierServer()`](cohortMethodKaplanMeierServer.md),
[`cohortMethodKaplanMeierViewer()`](cohortMethodKaplanMeierViewer.md),
[`cohortMethodPopulationCharacteristicsServer()`](cohortMethodPopulationCharacteristicsServer.md),
[`cohortMethodPopulationCharacteristicsViewer()`](cohortMethodPopulationCharacteristicsViewer.md),
[`cohortMethodPowerServer()`](cohortMethodPowerServer.md),
[`cohortMethodPowerViewer()`](cohortMethodPowerViewer.md),
[`cohortMethodPropensityModelServer()`](cohortMethodPropensityModelServer.md),
[`cohortMethodPropensityModelViewer()`](cohortMethodPropensityModelViewer.md),
[`cohortMethodPropensityScoreDistServer()`](cohortMethodPropensityScoreDistServer.md),
[`cohortMethodPropensityScoreDistViewer()`](cohortMethodPropensityScoreDistViewer.md),
[`cohortMethodSystematicErrorServer()`](cohortMethodSystematicErrorServer.md),
[`cohortMethodSystematicErrorViewer()`](cohortMethodSystematicErrorViewer.md),
[`estimationHelperFile()`](estimationHelperFile.md),
[`estimationServer()`](estimationServer.md),
[`estimationViewer()`](estimationViewer.md)
