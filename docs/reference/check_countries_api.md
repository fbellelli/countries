# Check if the connection to Countries REST API is working

Check if the connection to [REST Countries
API](https://restcountries.com/) is working. The function checks if the
user has an internet connection and if any answer is returned from the
Countries REST API.

## Usage

``` r
check_countries_api(warnings = TRUE, timeout = 4)
```

## Arguments

- warnings:

  Logical value indicating whether to output a warning when there is no
  connection. Default is `TRUE`.

- timeout:

  Numeric value giving the timeout in seconds for attempting connection
  to the API. Default is `4` second.

## Value

Returns a logical value: `TRUE` if there is a connection, `FALSE` if
there is no connection.

## See also

[list_fields](https://fbellelli.github.io/countries/reference/list_fields.md),
[country_info](https://fbellelli.github.io/countries/reference/country_info.md)

## Examples

``` r
check_countries_api()
#> [1] TRUE
```
