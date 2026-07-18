# Internal function - return n spaced indices for picking colours from a palette

Internal function - return n spaced indices for picking colours from a
palette

## Usage

``` r
pick_colours(n, n_colours_palette)
```

## Arguments

- n:

  Desired Number of colours desired

- n_colours_palette:

  Total number of colours in the palette

## Value

Returns n indices that can be used to select colours from a palette of
n_colours_palette length

## See also

[palettes_countries](https://fbellelli.github.io/countries/reference/palettes_countries.md)

## Examples

``` r
countries:::pick_colours(5, 10)
#> [1] 2 3 5 7 9
```
