# The module server for rendering a PLE propensity score distribution

The module server for rendering a PLE propensity score distribution

## Usage

``` r
cohortMethodPropensityScoreDistServer(
  id,
  selectedRow,
  connectionHandler,
  resultDatabaseSettings,
  metaAnalysisDbIds = F
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

the PLE propensity score distribution content server

## See also

Other Estimation:
[`cohortMethodAttritionServer()`](cohortMethodAttritionServer.md),
[`cohortMethodAttritionViewer()`](cohortMethodAttritionViewer.md),
[`cohortMethodCovariateBalanceServer()`](cohortMethodCovariateBalanceServer.md),
[`cohortMethodCovariateBalanceViewer()`](cohortMethodCovariateBalanceViewer.md),
[`cohortMethodKaplanMeierServer()`](cohortMethodKaplanMeierServer.md),
[`cohortMethodKaplanMeierViewer()`](cohortMethodKaplanMeierViewer.md),
[`cohortMethodPopulationCharacteristicsServer()`](cohortMethodPopulationCharacteristicsServer.md),
[`cohortMethodPopulationCharacteristicsViewer()`](cohortMethodPopulationCharacteristicsViewer.md),
[`cohortMethodPowerServer()`](cohortMethodPowerServer.md),
[`cohortMethodPowerViewer()`](cohortMethodPowerViewer.md),
[`cohortMethodPropensityModelServer()`](cohortMethodPropensityModelServer.md),
[`cohortMethodPropensityModelViewer()`](cohortMethodPropensityModelViewer.md),
[`cohortMethodPropensityScoreDistViewer()`](cohortMethodPropensityScoreDistViewer.md),
[`cohortMethodSystematicErrorServer()`](cohortMethodSystematicErrorServer.md),
[`cohortMethodSystematicErrorViewer()`](cohortMethodSystematicErrorViewer.md),
[`estimationHelperFile()`](estimationHelperFile.md),
[`estimationServer()`](estimationServer.md),
[`estimationViewer()`](estimationViewer.md)
