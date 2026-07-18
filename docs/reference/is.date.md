# internal function - Tests whether the value is convertible to a date format

Internal function behind
[`is_date()`](https://fbellelli.github.io/countries/reference/is_date.md).

## Usage

``` r
is.date(x, formats)
```

## Arguments

- x:

  A vector of length 1 to be tested

- formats:

  Formats to check for expressed in standard notation.

## Value

Returns a logical value indicating whether the value can be converted to
any of the date formats provided. This function is the basis for
[`countries::is_date()`](https://fbellelli.github.io/countries/reference/is_date.md).

## See also

[is_date](https://fbellelli.github.io/countries/reference/is_date.md)
[find_timecol](https://fbellelli.github.io/countries/reference/find_timecol.md)

## Examples

``` r
is.date("2020-01-01")
#> Error in is.date("2020-01-01"): could not find function "is.date"
is.yearcol("a")
#> Error in is.yearcol("a"): could not find function "is.yearcol"
```
