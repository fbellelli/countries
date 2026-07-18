# Get information about countries

This function is an interface for [REST Countries
API](https://restcountries.com/). It allows to request and download
information about countries, such as: currency, capital city, language
spoken, flag, neighbouring countries, and much more. **NOTE:** From
2026, a personal key is needed to use the API, the key can be created on
the API website [REST Countries API](https://restcountries.com/).
Internet access is needed to download information from the API. At times
the API may be unstable or slow to respond. Every time this function is
executied it performs three API calls (the entire dataset is downloaded
in three batches).

## Usage

``` r
country_info(
  key,
  countries = NULL,
  fields = NULL,
  fuzzy_match = TRUE,
  match_info = FALSE,
  base_url = "https://api.restcountries.com/countries/v5"
)
```

## Arguments

- key:

  String containing the personal API key used to authenticate the user.
  The key can be created on the API website [REST Countries
  API](https://restcountries.com/).

- countries:

  (optional) A vector of countries for which we wish to filter the
  downloaded information. The function also supports fuzzy matching
  capabilities to facilitate filtering. When left blank or set to
  `NULL`, all the countries in the dataset are returned.

- fields:

  (optional) Character vector indicating the fields to query. If
  `fields` is left empty, all available fields will be returned. A
  description of the accepted fields can be found on the API website or
  it can be obtained with the function
  [`list_fields()`](https://fbellelli.github.io/countries/reference/list_fields.md).

- fuzzy_match:

  Logical value indicating whether to allow fuzzy matching of country
  names. Default is `TRUE`.

- match_info:

  Logical value indicating whether to return information on country
  names matched to each input in `countries`. If `TRUE`, two additional
  columns will be added to the output (`matched_country` and
  `is_country`). Default is `FALSE`.

- base_url:

  Base URL used to construct the API calls. The default is
  `"https://api.restcountries.com/countries/v5"`.

## Value

Returns the requested information about the countries in a table. The
rows of the table correspond to entries in `countries`, columns
correspond to requested `fields`.

## See also

[list_fields](https://fbellelli.github.io/countries/reference/list_fields.md),
[check_countries_api](https://fbellelli.github.io/countries/reference/check_countries_api.md)

## Examples

``` r
# Run examples only if a connection to the API is available:
if (check_countries_api(warnings = FALSE)){

# NOTE: A VALID KEY IS NEEDED TO RUN THE EXAMPLES. CREATE ONE AT https://restcountries.com/

# downloaded all the available information by leaving both countries and fields arguments empty
# The example below uses a test key (it will return a fixed output for Canada only)
info <- country_info(key = "rc_live_demo")

# The example below filters the dataset to obtain information for one or more countries:
# info <- country_info(countries = "DR Congo", key = "YOUR_KEY")
# info <- country_info(countries = c("Morocco", "Brazil", "FR"), key = "YOUR_KEY")

# The fields argument can be used to query only for specific information
# info <- country_info(fields = "capitals", key = "YOUR_KEY")
# info <- country_info(countries = c("Brazil", "USA", "FR"), fields = "capitals", key = "YOUR_KEY")

}
```
