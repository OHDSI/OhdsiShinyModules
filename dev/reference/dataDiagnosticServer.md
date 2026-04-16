# The module server for exploring data-diagnostic

The module server for exploring data-diagnostic

## Usage

``` r
dataDiagnosticServer(
  id = "dataDiag",
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

  a list containing the data-diagnostic result schema

## Value

The server for the data-diagnostic module

## Details

The user specifies the id for the module

## See also

Other DataDiagnostics:
[`dataDiagnosticDrillServer()`](dataDiagnosticDrillServer.md),
[`dataDiagnosticDrillViewer()`](dataDiagnosticDrillViewer.md),
[`dataDiagnosticHelperFile()`](dataDiagnosticHelperFile.md),
[`dataDiagnosticSummaryServer()`](dataDiagnosticSummaryServer.md),
[`dataDiagnosticSummaryViewer()`](dataDiagnosticSummaryViewer.md),
[`dataDiagnosticViewer()`](dataDiagnosticViewer.md)
