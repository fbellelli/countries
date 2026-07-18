# Return location of minimum, maximum and mode values' index

These function return the position (index) of all the minimum, maximum,
and mode values of the vector `x`. `which_min()` and `which_max()` only
support numeric and logical vectors. These functions are identical to
[`which.min()`](https://rdrr.io/r/base/which.min.html) and
[`which.max()`](https://rdrr.io/r/base/which.min.html), except that ALL
minima/maxima are returned instead of only the first one.

## Usage

``` r
which_min(x, first_only = FALSE)

which_max(x, first_only = FALSE)

which_mode(x, first_only = FALSE)
```

## Arguments

- x:

  A numeric or vector

- first_only:

  Logical value indicating whether only the first value should be
  returned (i.e. if `TRUE` the function behaves like
  [`which.min()`](https://rdrr.io/r/base/which.min.html) and
  [`which.max()`](https://rdrr.io/r/base/which.min.html)). Default is
  FALSE.

## Value

Returns the position of the minimum, maximum and mode values of a vector
`x`

## See also

[Mode](https://fbellelli.github.io/countries/reference/Mode.md),
[which.min](https://rdrr.io/r/base/which.min.html),
[which.max](https://rdrr.io/r/base/which.min.html)

## Examples

``` r
which_mode(c("a","a",2,3))
#> [1] 1 2
which_min(c(1,1,2,3,NA,2))
#> [1] 1 2
which_max(c(NA,NA,NA,1,1,2))
#> [1] 6
```
