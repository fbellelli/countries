# List of accepted fields for the function country_info

This function queries [REST Countries API](https://restcountries.com/)
and returns a list of all possible fields that can be used in the
function `country_info`. **NOTE:** Internet access is needed to download
information from the API.

## Usage

``` r
list_fields()
```

## Value

A vector of accepted fields for the function
[`country_info()`](https://fbellelli.github.io/countries/reference/country_info.md)

## See also

[country_info](https://fbellelli.github.io/countries/reference/country_info.md)

## Examples

``` r
# Run example only if a connection to the API is available
if (check_countries_api(warnings = FALSE)){

list_fields()

}
#>  [1] "capitals"        "region"          "subregion"       "assets"         
#>  [5] "borders"         "calling_codes"   "continents"      "currencies"     
#>  [9] "government_type" "landlocked"      "languages"       "leaders"        
#> [13] "population"      "timezones"       "tlds"            "uuid"           
#> [17] "names"           "codes"           "flag"            "area"           
#> [21] "cars"            "classification"  "coordinates"     "date"           
#> [25] "demonyms"        "descriptions"    "economy"         "links"          
#> [29] "memberships"     "number_format"   "parent"          "postal_code"    
#> [33] "units"          
```
