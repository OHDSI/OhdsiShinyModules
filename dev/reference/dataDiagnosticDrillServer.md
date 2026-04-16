# The module server for exploring prediction summary results

The module server for exploring prediction summary results

## Usage

``` r
dataDiagnosticDrillServer(id, connectionHandler, resultDatabaseSettings)
```

## Arguments

- id:

  the unique reference id for the module

- connectionHandler:

  the connection to the prediction result database

- resultDatabaseSettings:

  a list containing the result schema and prefixes

## Value

The server to the summary module

## Details

The user specifies the id for the module

## See also

Other DataDiagnostics:
[`dataDiagnosticDrillViewer()`](dataDiagnosticDrillViewer.md),
[`dataDiagnosticHelperFile()`](dataDiagnosticHelperFile.md),
[`dataDiagnosticServer()`](dataDiagnosticServer.md),
[`dataDiagnosticSummaryServer()`](dataDiagnosticSummaryServer.md),
[`dataDiagnosticSummaryViewer()`](dataDiagnosticSummaryViewer.md),
[`dataDiagnosticViewer()`](dataDiagnosticViewer.md)
