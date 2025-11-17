# Large Data Table

Large data table R6 class.

Uses ResultModelManager::ConnectionHandler class to create paginating
tables

NOTE Only currently works with sqlite and postgresql database backends
(probably redshift too) as this method uses limit and offset for the
queries

Alternatively, you might want to subclass this class. For example, if
your backend query is against an API such as and ATLAS instance or
ATHENA

If subclassing use inheritance and treat this class as an interface to
implement - implementing the methods:

get

## Public fields

- `baseQuery`:

  query string sql

- `countQuery`:

  count query string (should match query). Can be auto generated with
  sub query (default) but this will likely result in slow results

- `connectionHandler`:

  ResultModelManager connection handler to execute query inside

## Methods

### Public methods

- [`LargeDataTable$new()`](#method-LargeDataTable-new)

- [`LargeDataTable$getCount()`](#method-LargeDataTable-getCount)

- [`LargeDataTable$getPage()`](#method-LargeDataTable-getPage)

- [`LargeDataTable$getAllResults()`](#method-LargeDataTable-getAllResults)

- [`LargeDataTable$clone()`](#method-LargeDataTable-clone)

------------------------------------------------------------------------

### Method `new()`

#### Usage

    LargeDataTable$new(connectionHandler, baseQuery, countQuery = NULL)

#### Arguments

- `connectionHandler`:

  ResultModelManager connectionHandler instance

- `baseQuery`:

  base sql query

- `countQuery`:

  count query string (should match query). Can be auto generated with
  sub query (default) but this will likely result in slow results

#### Returns

self get count

------------------------------------------------------------------------

### Method `getCount()`

execute count query with specified parameters

#### Usage

    LargeDataTable$getCount(...)

#### Arguments

- `...`:

#### Returns

count Get Page

------------------------------------------------------------------------

### Method `getPage()`

#### Usage

    LargeDataTable$getPage(pageNum, pageSize = self$pageSize, ...)

#### Arguments

- `pageNum`:

  page number

- `pageSize`:

  page size

- `...`:

#### Returns

data.frame of query result get all results

------------------------------------------------------------------------

### Method `getAllResults()`

#### Usage

    LargeDataTable$getAllResults(...)

#### Arguments

- `...`:

#### Returns

data.frame of all results. Used for large file downloads

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    LargeDataTable$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
