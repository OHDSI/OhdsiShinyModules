# The module server for exploring estimation studies

The module server for exploring estimation studies

## Usage

``` r
estimationServer(
  id,
  connectionHandler,
  resultDatabaseSettings = list(port = 1)
)
```

## Arguments

- id:

  the unique reference id for the module

- connectionHandler:

  a connection to the database with the results

- resultDatabaseSettings:

  a list containing the characterization result schema, dbms,
  tablePrefix, databaseTable and cgTablePrefix

## Value

The server for the estimation module

## Details

The user specifies the id for the module

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
[`cohortMethodPropensityScoreDistServer()`](cohortMethodPropensityScoreDistServer.md),
[`cohortMethodPropensityScoreDistViewer()`](cohortMethodPropensityScoreDistViewer.md),
[`cohortMethodSystematicErrorServer()`](cohortMethodSystematicErrorServer.md),
[`cohortMethodSystematicErrorViewer()`](cohortMethodSystematicErrorViewer.md),
[`estimationHelperFile()`](estimationHelperFile.md),
[`estimationViewer()`](estimationViewer.md)
