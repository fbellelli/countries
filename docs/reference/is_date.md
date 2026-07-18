# Test whether the input is a date

This function checks if a value is a date by attempting to convert it to
a date format. The user can specify which date formats should be tested
with the argument `formats`.

## Usage

``` r
is_date(
  x,
  formats = c("%Y-%m-%d", "%y-%m-%d", "%m-%d-%Y", "%m-%d-%y", "%d-%m-%Y",
    "%d-%m-%y", "%Y/%m/%d", "%y/%m/%d", "%m/%d/%Y", "%m/%d/%y",
    "%d/%m/%Y", "%d/%m/%y", "%Y.%m.%d", "%y.%m.%d", "%m.%d.%Y",
    "%m.%d.%y", "%d.%m.%Y", "%d.%m.%y", "%d %b %Y", "%d %B %Y",
    "%b %d %Y", "%B %d %Y", "%b %d, %Y", "%B %d, %Y", "%d%b%Y",
    "%d%B%Y", "%Y%B%d", "%Y%b%d", "%b %Y", "%B %Y", "%b %y", "%B %y",
    "%m-%Y", "%Y-%m", "%m/%Y", "%Y/%m")
)
```

## Arguments

- x:

  A vector of values to be tested

- formats:

  Date formats to be checked for (expressed in R date notation).

## Value

Returns a logical vector indicating whether the values can be converted
to any of the date formats provided. Notice that unless specified, the
default allowed formats do not include simple year numbers (e.g. 2022 or
1993) because number vectors could wrongly be identified as dates. Also,
notice that testing `NA` values will return `FALSE`.

## See also

[find_timecol](https://fbellelli.github.io/countries/reference/find_timecol.md),
[find_keycol](https://fbellelli.github.io/countries/reference/find_keycol.md),
[is_country](https://fbellelli.github.io/countries/reference/is_country.md)

## Examples

``` r
is_date(c("2020-01-01","test",2020,"March 2030"))
#> [1]  TRUE FALSE FALSE  TRUE
```
