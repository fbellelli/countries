# Internal function - Tests whether a vector of data could be containing year information

Internal function - Tests whether a vector of data could be containing
year information

## Usage

``` r
is.yearcol(x, limit = c(1800, 2200), regularity = FALSE, allow_NA = TRUE)
```

## Arguments

- x:

  A vector to be tested

- limit:

  A minimum and maximum year value `c(1800,2200)`

- regularity:

  Require that no gap is present in the year values (e.g. 2005, 2000,
  2010, 2020 would return FALSE because 2015 is missing). Default is
  `FALSE` (regularity is not required).

- allow_NA:

  If set to true the date vector is allowed to contain `NA` values. If
  set to `FALSE` the function returns a FALSE if any value is NA.
  Default is `TRUE`.

## Value

Returns a logical vector indicating whether the vector could be
containing year information

## See also

[is_country](https://fbellelli.github.io/countries/reference/is_country.md),
[find_countrycol](https://fbellelli.github.io/countries/reference/find_countrycol.md)

## Examples

``` r
countries:::is.yearcol(1990:2020)
#> [1] TRUE
countries:::is.yearcol(c("a",2000))
#> [1] FALSE
```
