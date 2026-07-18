# Test wheter a set of column could be a data frame key

This function takes a data frame and a vector of column names as
argument and returns a logical value indicating whether the indicated
columns uniquely identify entries in the data frame. If the output is
`TRUE`, the indicated columns could be the keys of the table.

## Usage

``` r
is_key(x, cols, allow_NA = FALSE, verbose = TRUE)
```

## Arguments

- x:

  A data frame object

- cols:

  A vector of column names to be tested.

- allow_NA:

  Logical value indicating whether to allow columns to have `NA` values.
  Default is `allow_NA=FALSE`, the function will return `FALSE` if any
  `NA` value is present in `colnames`.

- verbose:

  Logical value indicating whether messages should be printed on the
  console. Default is `TRUE`.

## Value

Returns a logical value. If `TRUE`, the columns indicated in `colnames`
uniquely identify the entries in `x`.

## See also

[find_countrycol](https://fbellelli.github.io/countries/reference/find_countrycol.md)
[find_timecol](https://fbellelli.github.io/countries/reference/find_timecol.md)

## Examples

``` r
is_key(data.frame(a=1:10,b=sample(c("a","b","c"),10, replace=TRUE)), cols="a")
#> [1] TRUE
is_key(data.frame(a=1:10,b=sample(c("a","b","c"),10, replace=TRUE)), cols="b")
#> [1] FALSE
is_key(data.frame(a=c(1:5,1:5),b=sample(c("a","b","c"),10, replace=TRUE), c=c(rep("a",5),rep("b",5))), cols=c("a","c"))
#> [1] TRUE
```
