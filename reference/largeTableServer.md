# Large Table Component Server

Display large data tables in a consistent way - server side pagination
for reactable objects

## Usage

``` r
largeTableServer(
  id,
  ldt,
  inputParams,
  modifyData = NULL,
  columns = shiny::reactive(list()),
  ...
)
```

## Arguments

- id:

  Shiny module id. Must match Large Table Viewer

- ldt:

  LargeDataTable instance

- inputParams:

  reactive that returns list of parameters to be passed to ldt

- modifyData:

  optional callback function that takes the data page, page number, page
  size as parameters must return data.frame compatable instance

- columns:

  List or reactable returning list of reactable::columnDef objects

- ...:

  Additional reactable options (searchable, sortable

## See also

Other LargeTables:
[`createLargeSqlQueryDt()`](createLargeSqlQueryDt.md),
[`largeTableView()`](largeTableView.md)
