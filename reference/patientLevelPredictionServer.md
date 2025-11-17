# The module server for exploring PatientLevelPrediction

The module server for exploring PatientLevelPrediction

## Usage

``` r
patientLevelPredictionServer(
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

  a list containing the prediction result schema and connection details

## Value

The server for the PatientLevelPrediction module

## Details

The user specifies the id for the module

## See also

Other PatientLevelPrediction:
[`patientLevelPredictionHeatmapServer()`](patientLevelPredictionHeatmapServer.md),
[`patientLevelPredictionHeatmapViewer()`](patientLevelPredictionHeatmapViewer.md),
[`patientLevelPredictionHelperFile()`](patientLevelPredictionHelperFile.md),
[`patientLevelPredictionModelServer()`](patientLevelPredictionModelServer.md),
[`patientLevelPredictionModelViewer()`](patientLevelPredictionModelViewer.md),
[`patientLevelPredictionPlotServer()`](patientLevelPredictionPlotServer.md),
[`patientLevelPredictionPlotViewer()`](patientLevelPredictionPlotViewer.md),
[`patientLevelPredictionViewer()`](patientLevelPredictionViewer.md)
