# Tests whether a string is a country name

This function checks whether the string is a country name. It supports
different languages and naming conventions. The function returns `TRUE`
if it relates to one of the 249 countries currently in the ISO standard
`3166`. Alternatively, the argument `check_for` allows to narrow down
the test to a subset of countries. Fuzzy matching can be used to allow a
small margin of error in the string.

## Usage

``` r
is_country(x, check_for = NULL, fuzzy_match = FALSE)
```

## Arguments

- x:

  A character vector to be tested (also supports UN/ISO country codes)

- check_for:

  A vector of country names to narrow down testing. The function will
  return `TRUE` only if the string relates to a country in this vector.
  Default is NULL.

- fuzzy_match:

  A logical value indicating whether to tolerate small discrepancies in
  the country name matching. The default and fastest option is `FALSE`.

## Value

Returns a logical vector indicating whether the string is a country name

## See also

[match_table](https://fbellelli.github.io/countries/reference/match_table.md),
[country_name](https://fbellelli.github.io/countries/reference/country_name.md),
[find_countrycol](https://fbellelli.github.io/countries/reference/find_countrycol.md)

## Examples

``` r
#Detect strings that are country names
is_country(x=c("ITA","Estados Unidos","Estado Unidos","bungalow","dog",542), fuzzy_match=FALSE)
#> [1]  TRUE  TRUE FALSE FALSE FALSE FALSE
is_country(x=c("ITA","Estados Unidos","Estado Unidos","bungalow","dog",542), fuzzy_match=TRUE)
#> [1]  TRUE  TRUE  TRUE FALSE FALSE FALSE
#Checking for a specific subset of countries
is_country(x=c("Ceylon","LKA","Indonesia","Inde"), check_for=c("India","Sri Lanka"))
#> [1]  TRUE  TRUE FALSE  TRUE
```
