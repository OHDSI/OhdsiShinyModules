# The module server for exploring prediction models

The module server for exploring prediction models

## Usage

``` r
patientLevelPredictionModelServer(
  id,
  performances,
  performanceRowIds,
  connectionHandler,
  resultDatabaseSettings
)
```

## Arguments

- id:

  the unique reference id for the module

- performances:

  a reactive val with the performance data.frame

- performanceRowIds:

  a vector of selected performance rows

- connectionHandler:

  the connection to the prediction result database

- resultDatabaseSettings:

  a list containing the result schema and prefixes

## Value

The server to the model module

## Details

The user specifies the id for the module

## See also

Other PatientLevelPrediction:
[`patientLevelPredictionHeatmapServer()`](patientLevelPredictionHeatmapServer.md),
[`patientLevelPredictionHeatmapViewer()`](patientLevelPredictionHeatmapViewer.md),
[`patientLevelPredictionHelperFile()`](patientLevelPredictionHelperFile.md),
[`patientLevelPredictionModelViewer()`](patientLevelPredictionModelViewer.md),
[`patientLevelPredictionPlotServer()`](patientLevelPredictionPlotServer.md),
[`patientLevelPredictionPlotViewer()`](patientLevelPredictionPlotViewer.md),
[`patientLevelPredictionServer()`](patientLevelPredictionServer.md),
[`patientLevelPredictionViewer()`](patientLevelPredictionViewer.md)
