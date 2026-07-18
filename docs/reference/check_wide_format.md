# Check for wide country data formats

The function looks for country names or year information in the column
names. This function is designed for simple panel country data, in which
countries' time series are arranged side by side on columns or stacked
on rows. The function will only return year/country column names if at
least 3 country/year column names are detected.

## Usage

``` r
check_wide_format(x, adjacency = TRUE)
```

## Arguments

- x:

  A dataframe

- adjacency:

  Logical value indicating whether column names containing country or
  year information need to be adjacent to each other. Default is `TRUE`

## Value

Returns a data.frame identifying the columns names that contain country
or year information.

## See also

[find_keycol](https://fbellelli.github.io/countries/reference/find_keycol.md),
[find_countrycol](https://fbellelli.github.io/countries/reference/find_countrycol.md),
[find_timecol](https://fbellelli.github.io/countries/reference/find_timecol.md)

## Examples

``` r
example <- data.frame(Year=2000:2010, China=0:10, US=10:20, Vietnam=30:40)
check_wide_format(x=example)
#>   countries col_indx col_name
#> 1       CHN        2    China
#> 2       USA        3       US
#> 3       VNM        4  Vietnam
```
