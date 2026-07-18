# Create a conversion table for country names

This function returns a conversion table for country names to the
desired naming conventions and languages. The use of fuzzy matching
allows more flexibility in recognising and identifying country names.

## Usage

``` r
match_table(
  x,
  to = c("simple", "ISO3"),
  fuzzy_match = TRUE,
  verbose = FALSE,
  matching_info = FALSE,
  simplify = TRUE,
  na_fill = FALSE,
  poor_matches = TRUE,
  custom_table = NULL
)
```

## Arguments

- x:

  A vector of country names

- to:

  A vector containing one or more desired naming conventions to which
  `x` should be converted to (e.g. `"ISO3"`, `"name_en"`, `"UN_fr"`,
  ...). For a list of all possible values [click
  here](https://fbellelli.github.io/countries/articles/dealing_with_names.html)
  or refer to the vignette on country names
  [`vignette("dealing_with_names")`](https://fbellelli.github.io/countries/articles/dealing_with_names.md).
  Default is `c("simple", "ISO3")`.

- fuzzy_match:

  Logical value indicating whether fuzzy matching of country names
  should be allowed (`TRUE`), or only exact matches are allowed
  (`FALSE`). Default is `TRUE`. Switching to `FALSE` will result in much
  faster execution.

- verbose:

  Logical value indicating whether the function should print to the
  console a report on the matching process. Default is `FALSE`.

- matching_info:

  Logical value. If set to true the output match table will include
  additional information on the matching of `x`'s entries. Default is
  `FALSE`.

- simplify:

  Logical value. If set to `TRUE` the function will return the match
  table as a `data.frame` object. If set to `FALSE`, the function will
  return a list object containing the match table and additional details
  on the country matching process. Default is `TRUE`.

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

- poor_matches:

  Logical value. If set to `TRUE` (the default option), the function
  will always return the closest matching country name, even if the
  matching is poor. If set to `FALSE`, the function will return `NA` in
  case of poor matching.

- custom_table:

  Custom conversion table to be used. This needs to be a data.frame
  object. Default is `NULL`.

## Value

Returns a conversion table for countries names to the desired naming
conventions. If `simplify=FALSE` it returns a list object.

## See also

[country_name](https://fbellelli.github.io/countries/reference/country_name.md),
[is_country](https://fbellelli.github.io/countries/reference/is_country.md)

## Examples

``` r
match_table(x=c("UK","Estados Unidos","Zaire","C#te d^ivoire"), to= c("UN_en","ISO3"))
#>   list_countries                                                UN_en ISO3
#> 1  C#te d^ivoire                                        Côte d’Ivoire  CIV
#> 2 Estados Unidos                             United States of America  USA
#> 3             UK United Kingdom of Great Britain and Northern Ireland  GBR
#> 4          Zaire                     Democratic Republic of the Congo  COD
```
