# Discrete colour palettes

This function provides access to the discrete colours palettes used in
this packages' 11 themes.

## Usage

``` r
palettes_countries(n, theme = 1, reverse = FALSE)
```

## Arguments

- n:

  Number of desired colours

- theme:

  A numeric value or name identifying the theme's colours. Can be a
  number between 1 and 11, or one of the theme's names:
  `c("Default", "Greyscale", "Candy", "RedBlue", "Dark", "Reds", "Blues", "Greens", "Viridis", "Cividis", "Distinct", "Distinct2", "Paired")`.

- reverse:

  Logical value indicating whether to reverse the order of the palette's
  colours Default is FALSE.

## Value

Returns n colours from the requested theme

## See also

[quick_map](https://fbellelli.github.io/countries/reference/quick_map.md)

## Examples

``` r
palettes_countries(5, theme = 1)
#> [1] "#cb997e" "#ddbea9" "#ffe8d6" "#b7b7a4" "#545849"
```
