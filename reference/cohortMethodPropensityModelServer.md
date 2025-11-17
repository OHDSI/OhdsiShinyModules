# The module server for rendering the propensity score model

The module server for rendering the propensity score model

## Usage

``` r
cohortMethodPropensityModelServer(
  id,
  selectedRow,
  connectionHandler,
  resultDatabaseSettings
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

## Value

the PLE propensity score model

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
[`cohortMethodPropensityModelViewer()`](cohortMethodPropensityModelViewer.md),
[`cohortMethodPropensityScoreDistServer()`](cohortMethodPropensityScoreDistServer.md),
[`cohortMethodPropensityScoreDistViewer()`](cohortMethodPropensityScoreDistViewer.md),
[`cohortMethodSystematicErrorServer()`](cohortMethodSystematicErrorServer.md),
[`cohortMethodSystematicErrorViewer()`](cohortMethodSystematicErrorViewer.md),
[`estimationHelperFile()`](estimationHelperFile.md),
[`estimationServer()`](estimationServer.md),
[`estimationViewer()`](estimationViewer.md)
