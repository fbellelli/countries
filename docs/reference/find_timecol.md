# Finds columns containing date and year data

This function takes a data frame as argument and returns the column
names (or indices) of all columns containing dates and the most likely
column containing year information, if any. It can be used to automate
the search of date and year columns in data frames.

## Usage

``` r
find_timecol(x, return_index = FALSE, allow_NA = TRUE, sample_size = 1000)
```

## Arguments

- x:

  A data frame object

- return_index:

  A logical value indicating whether the function should return the
  index of time columns instead of the column names. Default is `FALSE`,
  column names are returned.

- allow_NA:

  Logical value indicating whether to allow time columns to contain `NA`
  values. Default is `allow_NA=FALSE`, the function will not return time
  column containing `NA` values.

- sample_size:

  Either `NA` or a numeric value indicating the sample size used for
  evaluating columns. Default is `1000`. If `NA` is passed, the function
  will evaluate the full table. The minimum accepted value is `100`
  (i.e. 100 randomly sampled rows are used to evaluate the columns).
  This parameter can be tuned to speed up computation on long datasets.
  Taking a sample could result in inexact identification of key columns,
  accuracy improves with larger samples.

## Value

Returns a vector of names (`return_index=FALSE`) or indices
(`return_index=TRUE`) of columns containing date or year information.
Only the most likely year column is returned.

## See also

[is_date](https://fbellelli.github.io/countries/reference/is_date.md),
[find_countrycol](https://fbellelli.github.io/countries/reference/find_countrycol.md)

## Examples

``` r
find_timecol(x=data.frame(a=1970:2020, year=1970:2020, b=rep("2020-01-01",51),c=sample(1:1000,51)))
#> [1] "year" "b"   
```
