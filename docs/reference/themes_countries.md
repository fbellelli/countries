# Internal function - ggplot themes for quick_map

Internal function - ggplot themes for quick_map

## Usage

``` r
themes_countries(theme = 1)
```

## Arguments

- theme:

  A numeric value or name identifying the theme's colours. Can be a
  number between 1 and 11, or one of the theme's names:
  `c("Default", "Greyscale", "Candy", "RedBlue", "Dark", "Reds", "Blues", "Greens", "Viridis", "Cividis", "Distinct")`.

## Value

Returns n indices that can be used to select colours from a palette of
n_colours_palette length

## See also

[quick_map](https://fbellelli.github.io/countries/reference/quick_map.md)

## Examples

``` r
countries:::themes_countries(theme = 1)
#> List of 9
#>  $ axis.text        : list()
#>   ..- attr(*, "class")= chr [1:2] "element_blank" "element"
#>  $ axis.ticks       : list()
#>   ..- attr(*, "class")= chr [1:2] "element_blank" "element"
#>  $ legend.background:List of 5
#>   ..$ fill         : chr "transparent"
#>   ..$ colour       : logi NA
#>   ..$ linewidth    : NULL
#>   ..$ linetype     : NULL
#>   ..$ inherit.blank: logi FALSE
#>   ..- attr(*, "class")= chr [1:2] "element_rect" "element"
#>  $ legend.position  : chr "bottom"
#>  $ legend.box.margin: 'margin' num [1:4] 0points 0points 0points 0points
#>   ..- attr(*, "unit")= int 8
#>  $ panel.background :List of 5
#>   ..$ fill         : chr "#D0E8FA"
#>   ..$ colour       : chr "grey30"
#>   ..$ linewidth    : NULL
#>   ..$ linetype     : NULL
#>   ..$ inherit.blank: logi FALSE
#>   ..- attr(*, "class")= chr [1:2] "element_rect" "element"
#>  $ panel.border     : list()
#>   ..- attr(*, "class")= chr [1:2] "element_blank" "element"
#>  $ panel.grid       : list()
#>   ..- attr(*, "class")= chr [1:2] "element_blank" "element"
#>  $ plot.background  :List of 5
#>   ..$ fill         : chr "transparent"
#>   ..$ colour       : logi NA
#>   ..$ linewidth    : NULL
#>   ..$ linetype     : NULL
#>   ..$ inherit.blank: logi FALSE
#>   ..- attr(*, "class")= chr [1:2] "element_rect" "element"
#>  - attr(*, "class")= chr [1:2] "theme" "gg"
#>  - attr(*, "complete")= logi FALSE
#>  - attr(*, "validate")= logi TRUE
```
