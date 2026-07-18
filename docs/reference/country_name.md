# Convert and translate country names

This function recognises and converts country names to different
nomenclatures and languages using a fuzzy matching algorithm.
`country_name()` can identify countries even when they are provided in
mixed formats or in different languages. It is robust to small
misspellings and recognises many alternative country names and old
nomenclatures.

## Usage

``` r
country_name(
  x,
  to = "ISO3",
  fuzzy_match = TRUE,
  verbose = FALSE,
  simplify = TRUE,
  poor_matches = FALSE,
  na_fill = FALSE,
  custom_table = NULL
)
```

## Arguments

- x:

  A vector of country names

- to:

  A string containing the desired naming conventions to which `x` should
  be converted to (e.g. `"ISO3"`, `"name_en"`, `"UN_fr"`, ...). For a
  list of all possible values [click
  here](https://fbellelli.github.io/countries/articles/dealing_with_names.html)
  or refer to the vignette on country names
  [`vignette("dealing_with_names")`](https://fbellelli.github.io/countries/articles/dealing_with_names.md).
  Default is `"ISO3"`, which converts to the 249 3-letter country codes
  currently in ISO standard 3166.

- fuzzy_match:

  Logical value indicating whether fuzzy matching of country names
  should be allowed (`TRUE`), or only exact matches are allowed
  (`FALSE`). Default is `TRUE`.

- verbose:

  Logical value indicating whether the function should print to the
  console a full report. Default is `FALSE`.

- simplify:

  Logical value. If set to `TRUE` the function will return a vector of
  converted names. If set to `FALSE`, the function will return a list
  object containing the converted vector and additional details on the
  country matching process. Default is `TRUE`.

- poor_matches:

  Logical value. If set to `FALSE` (the default), the function will
  return `NA` in case of poor matching. If set to `TRUE`, the function
  will always return the closest matching country name, even if the
  match is poor.

- na_fill:

  Logical value. If set to `TRUE`, any `NA` in the output names will be
  filled with the original country name supplied in `x`. The default is
  `FALSE` (no filling). In general, `NA`s are produced if: 1) the
  country is not present in the nomenclature requested in `to` (e.g.
  `country_name("Abkhazia", to = "ISO3")`), 2) the input country name is
  `NA`, 3) No exact match is found and the user sets the option
  `fuzzy_match = FALSE`, 4) When the fuzzy match algorithm does not find
  a good match and the user sets the option `poor_match = FALSE`. The
  `na_fill` argument gives the option to replace the resulting NA with
  the original value in `x`.

- custom_table:

  Custom conversion table to be used. This needs to be a `data.frame`
  object. Default is `NULL`.

## Value

Returns a vector of converted country names. If multiple nomenclatures
are passed to the argument `to`, the vectors are arranged in a data
frame. If `simplify=FALSE`, the function will return a list object.

## See also

[is_country](https://fbellelli.github.io/countries/reference/is_country.md),
[match_table](https://fbellelli.github.io/countries/reference/match_table.md),
[find_countrycol](https://fbellelli.github.io/countries/reference/find_countrycol.md)

## Examples

``` r
#Convert country names to a single nomenclatures: (e.g. 3-letters ISO code)
country_name(x=c("UK","Estados Unidos","Zaire","C#te d^ivoire"), to= "ISO3")
#> [1] "GBR" "USA" "COD" "CIV"

#When multiple arguments are provided to the - to - argument, a data frame is returned:
country_name(x=c("UK","Estados Unidos","Zaire","C#te d^ivoire"), to= c("UN_en","UN_fr","ISO3"))
#>                                                  UN_en
#> 1 United Kingdom of Great Britain and Northern Ireland
#> 2                             United States of America
#> 3                     Democratic Republic of the Congo
#> 4                                        Côte d’Ivoire
#>                                                 UN_fr ISO3
#> 1 Royaume-Uni de Grande-Bretagne et d’Irlande du Nord  GBR
#> 2                               États-Unis d’Amérique  USA
#> 3                    République démocratique du Congo  COD
#> 4                                       Côte d’Ivoire  CIV

#This function can also be used to translate country names: (e.g. translating all to Chinese)
country_name(x=c("UK","Estados Unidos","Zaire","C#te d^ivoire"), to= "name_zh")
#> [1] "英国"           "美国"           "刚果民主共和国" "科特迪瓦"      
```
