# Automatic pivoting of country and year columns to a long format

When at least 3 country names or years are found in the column names,
the function will automatically transform the table from a wide to a
long format by pivoting the country/year columns. This is equivalent to
applying
[`tidyr::pivot_longer()`](https://tidyr.tidyverse.org/reference/pivot_longer.html)
or
[`data.table::melt()`](https://rdrr.io/pkg/data.table/man/melt.data.table.html)
on the columns with years or countries as names. The function is able to
detect years also when they are preceded by a prefix.

## Usage

``` r
auto_melt(
  x,
  names_to = "pivoted_colnames",
  values_to = "pivoted_data",
  verbose = TRUE,
  pivoting_info = FALSE
)
```

## Arguments

- x:

  A data.frame object to check and pivot country or year columns.

- names_to:

  String indicating how the column holding the name of the pivoted
  columns should be called in the output table. Default is
  `"pivoted_colnames"`

- values_to:

  String indicating how the column containing the values of the pivoted
  columns should be called in the output table. Default is
  `"pivoted_data"`

- verbose:

  Logical value. If set to `TRUE` (the default), a message will be
  displayed on the console indicating which columns are being pivoted.
  If set to `FALSE`, the messages are turned off.

- pivoting_info:

  Logical value indicating whether to return the list of names of the
  column that have been pivoted. Default is `FALSE`. If set to `TRUE`,
  the output will be a list instead of simple data.frame. Teh list will
  contain 1) the pivoted table, 2) the list of pivoted columns.

## Value

A table transformed into a "long" format by pivoting country or year
columns. If year columns are found, a numeric column called
`"year_pivoted_colnames"` is added isolating the years extracted from
the table header's.

## See also

[auto_merge](https://fbellelli.github.io/countries/reference/auto_merge.md),
[find_countrycol](https://fbellelli.github.io/countries/reference/find_countrycol.md),[find_timecol](https://fbellelli.github.io/countries/reference/find_timecol.md)

## Examples

``` r
# example data
example <- data.frame(Date = c("01.01.2019", "01.02.2019", "01.03.2019"),
                      Japan = 1:3,
                      Norway = 2:4,
                      Germany = 3:5,
                      US = 4:6)
example2 <- data.frame(Sector = c("Agriculture", "Mining", "Forestry"),
                       X2000 = 1:3,
                       X2001 = 2:4,
                       X2002 = 3:5,
                       X2003 = 4:6)

# examples pivotting countries and years from column names
auto_melt(example)
#> countries detected in column names, pivoting columns: Japan, Norway, Germany, ..., US
#>    pivoted_colnames       Date pivoted_data
#> 1             Japan 01.01.2019            1
#> 2            Norway 01.01.2019            2
#> 3           Germany 01.01.2019            3
#> 4                US 01.01.2019            4
#> 5             Japan 01.02.2019            2
#> 6            Norway 01.02.2019            3
#> 7           Germany 01.02.2019            4
#> 8                US 01.02.2019            5
#> 9             Japan 01.03.2019            3
#> 10           Norway 01.03.2019            4
#> 11          Germany 01.03.2019            5
#> 12               US 01.03.2019            6
auto_melt(example2)
#> year detected in column names, pivoting columns: X2000, X2001, X2002, ..., X2003
#>    pivoted_colnames      Sector pivoted_data year_pivoted_colnames
#> 1             X2000 Agriculture            1                  2000
#> 2             X2001 Agriculture            2                  2001
#> 3             X2002 Agriculture            3                  2002
#> 4             X2003 Agriculture            4                  2003
#> 5             X2000      Mining            2                  2000
#> 6             X2001      Mining            3                  2001
#> 7             X2002      Mining            4                  2002
#> 8             X2003      Mining            5                  2003
#> 9             X2000    Forestry            3                  2000
#> 10            X2001    Forestry            4                  2001
#> 11            X2002    Forestry            5                  2002
#> 12            X2003    Forestry            6                  2003
```
