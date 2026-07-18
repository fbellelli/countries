# Find a set of columns that uniquely identifies table entries

This function takes a data frame as argument and returns the column
names (or indices) of a set of columns that uniquely identify the table
entries (i.e. table key). It can be used to automate the search of table
keys. Since the function was designed for country data, it will first
search for columns containing country names and dates/years. These
columns will be given priority in the search for keys. Next, the
function prioritises left-most columns in the table. For time
efficiency, the function does not test all possible combination of
columns, it just tests the most likely combinations. The function will
look for the most common country data formats (e.g. cross-sectional,
time-series, panel data, dyadic, etc.) and searches for up to 2
additional key columns beyond country and time columns.

## Usage

``` r
find_keycol(
  x,
  return_index = FALSE,
  search_only = NA,
  sample_size = 1000,
  allow_NA = FALSE
)
```

## Arguments

- x:

  A data frame object

- return_index:

  A logical value indicating whether the function should return the
  index of country columns instead of the column names. Default is
  `FALSE`, column names are returned.

- search_only:

  This parameter can be used to restrict the search of table keys to a
  subset of columns. The default is `NA`, which will result in the
  entire table being searched. Alternatively, users may restrict the
  search by providing a vector containing the name or the numeric index
  of columns to check. For example, search could be restricted to the
  first ten columns by passing `1:10`. This could be useful in speeding
  up the search in wide tables.

- sample_size:

  Either `NA` or a numeric value indicating the sample size used for
  evaluating columns. Default is `1000`. If `NA` is passed, the function
  will evaluate the full table. The minimum accepted value is `100`
  (i.e. 100 randomly sampled rows are used to evaluate the columns).
  This parameter can be tuned to speed up computation on long datasets.
  Taking a sample could result in inexact identification of key columns,
  accuracy improves with larger samples.

- allow_NA:

  Logical value indicating whether to allow key columns to have `NA`
  values. Default is `allow_NA=FALSE`. If set to `TRUE`, `NA` is
  considered as a distinct value.

## Value

Returns a vector of column names (or indices) that uniquely identify the
entries in the table. If no key is found, the function will return
`NULL`. The output is a named vector indicating whether the identified
key columns contain country names (`"country"`), year and dates
(`"time"`), or other type of information (`"other"`).

## See also

[find_timecol](https://fbellelli.github.io/countries/reference/find_timecol.md),
[find_countrycol](https://fbellelli.github.io/countries/reference/find_countrycol.md),
[is_keycol](https://fbellelli.github.io/countries/reference/is_keycol.md)

## Examples

``` r
example <-data.frame(nation=rep(c("FRA","ALB","JOR"),3),
                     year=c(rep(2000,3),rep(2005,3),rep(2010,3)),
                     var=runif(9))
find_keycol(x=example)
#>  country     time 
#> "nation"   "year" 
```
