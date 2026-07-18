# Simplified merging of country data tables supporting different country nomenclatures and date formats

The aim of this function is to simplify country data merging. The
function performs merging of multiple data tables at once and is able to
automatically detect country and time columns. It can further simplify
merging by handling differing country naming conventions and date
formats.

## Usage

``` r
auto_join(
  ...,
  by = NULL,
  country_to = "ISO3",
  coalesce = FALSE,
  inner_join = FALSE,
  verbose = TRUE,
  merging_info = TRUE
)
```

## Arguments

- ...:

  Data to be merged. Inputs need to be data frames or coercible to data
  frames

- by:

  A list or a vector indicating the columns to be used for merging the
  data. If not provided, the function will automatically detect country
  and time columns and attempt to merge them. Other columns will be
  merged only if they share the same name. First time column and first
  two distinct country columns.

- country_to:

  Nomenclature to which country names should be converted to in the
  output. Default is `simple`. For a description of possible options,
  refer to the table in the vignette [Dealing with country
  names](https://fbellelli.github.io/countries/articles/dealing_with_names.html).

- inner_join:

  Logical value indicating whether to perform an inner join. The default
  is `FALSE`, which results in a full join of the provided tables.

- verbose:

  Logical value indicating if status messages should be printed to the
  console. Default is `TRUE`.

- merging_info:

  Logical value. If `TRUE`, the function will output a list containing
  the merged data and information generated during the merging process,
  such as the conversion table used for country names or information on
  table variables.
