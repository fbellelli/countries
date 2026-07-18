# Finds columns containing country names

This function takes a data frame as argument and returns the column name
(or index) of all columns containing country names. It can be used to
automate the search of country columns in data frames. For the purpose
of this function, a country is any of the 249 territories designated in
the ISO standard `3166`. On large datasets a random sample is used for
evaluating the columns.

## Usage

``` r
find_countrycol(
  x,
  return_index = FALSE,
  allow_NA = TRUE,
  min_share = 0.6,
  sample_size = 1000
)
```

## Arguments

- x:

  A data frame object

- return_index:

  A logical value indicating whether the function should return the
  index of country columns instead of the column names. Default is
  `FALSE`, column names are returned.

- allow_NA:

  Logical value indicating whether columns containing `NA` values are to
  be considered as country columns. Default is `allow_NA=FALSE`, the
  function will not return country column containing `NA` values.

- min_share:

  A value between `0` and `1` indicating the minimum share of country
  names in columns that are returned. A value of `0` will return any
  column containing a country name. A value of `1` will return only
  columns whose entries are all country names. Default is `0.6`, i.e. at
  least 60 percent of the column entries need to be country names.

- sample_size:

  Either `NA` or a numeric value indicating the sample size used for
  evaluating columns. Default is `1000`. If `NA` is passed, the function
  will evaluate the full table. The minimum accepted value is `100`
  (i.e. 100 randomly sampled rows are used to evaluate the columns).
  This parameter can be tuned to speed up computation on long datasets.
  Taking a sample could result in inexact identification of key columns,
  accuracy improves with larger samples.

## Value

Returns a vector of country names (`return_index=FALSE`) or column
indices (`return_index=TRUE`) of columns containing country names.

## See also

[is_country](https://fbellelli.github.io/countries/reference/is_country.md),
[country_name](https://fbellelli.github.io/countries/reference/country_name.md),
[find_keycol](https://fbellelli.github.io/countries/reference/find_keycol.md),
[find_timecol](https://fbellelli.github.io/countries/reference/find_timecol.md)

## Examples

``` r
find_countrycol(x=data.frame(a=c("Brésil","Tonga","FRA"), b=c(1,2,3)))
#> [1] "a"
```
