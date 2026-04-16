# The module server for exploring summary html files

The module server for exploring summary html files

## Usage

``` r
homeServer(id, connectionHandler, resultDatabaseSettings = list(port = 1))
```

## Arguments

- id:

  the unique reference id for the module

- connectionHandler:

  a connection to the database with the results

- resultDatabaseSettings:

  a list containing the prediction result schema and connection details

## Value

The server for the home module

## Details

The user specifies the id for the module

## See also

Other Home: [`homeHelperFile()`](homeHelperFile.md)
