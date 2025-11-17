# The module server for rendering the PLE attrition results

The module server for rendering the PLE attrition results

## Usage

``` r
cohortMethodAttritionServer(
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

the PLE attrition results content server

## See also

Other Estimation:
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
[`cohortMethodPropensityScoreDistServer()`](cohortMethodPropensityScoreDistServer.md),
[`cohortMethodPropensityScoreDistViewer()`](cohortMethodPropensityScoreDistViewer.md),
[`cohortMethodSystematicErrorServer()`](cohortMethodSystematicErrorServer.md),
[`cohortMethodSystematicErrorViewer()`](cohortMethodSystematicErrorViewer.md),
[`estimationHelperFile()`](estimationHelperFile.md),
[`estimationServer()`](estimationServer.md),
[`estimationViewer()`](estimationViewer.md)
