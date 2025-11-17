# Large Table Component Viewer

Componenet for results sets with many thousands of rows More limited
than other table components in terms of automatic handling of search and
filtering but will allow responsive apps

## Usage

``` r
largeTableView(
  id,
  pageSizeChoices = c(10, 25, 50, 100),
  selectedPageSize = 10,
  fullDownloads = TRUE
)
```

## Arguments

- id:

  Shiny module id. Must match largeTableServer

- pageSizeChoices:

  numeric selection options for pages

- selectedPageSize:

  numeric selection options for pages

- fullDownloads:

  allow download button of full dataset from query

## See also

Other LargeTables:
[`createLargeSqlQueryDt()`](createLargeSqlQueryDt.md),
[`largeTableServer()`](largeTableServer.md)
