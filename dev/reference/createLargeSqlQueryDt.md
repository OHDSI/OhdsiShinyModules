# Create Large Sql Query Data Table

Construct an instance of a LargeDataTable R6 instance for use inside
largeTableServer

This should pass a parameterized sql query that can be used to
iteratively return data from a table rather than returning the entire
object.

## Usage

``` r
createLargeSqlQueryDt(
  connectionHandler = NULL,
  connectionDetails = NULL,
  baseQuery,
  countQuery = NULL
)
```

## Arguments

- connectionHandler:

  ResultModelManager connectionHandler instance

- connectionDetails:

  DatabaseConnector connectionDetails instance

- baseQuery:

  base sql query

- countQuery:

  count query string (should match query). Can be auto generated with
  sub query (default) but this will likely result in slow results

## See also

Other LargeTables: [`largeTableServer()`](largeTableServer.md),
[`largeTableView()`](largeTableView.md)
