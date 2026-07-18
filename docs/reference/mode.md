# Statistical mode of a vector

This function returns the mode of vectors. That is to say, for any given
vector of values, it returns the value that appears most frequently. The
function works with strings, numerical and mixed inputs. `NA` values are
treated as distinct values.

## Usage

``` r
Mode(x, na.rm = FALSE, first_only = FALSE)
```

## Arguments

- x:

  A vector

- na.rm:

  Logical value indicating whether `NA` values should be omitted.
  Default is `FALSE`.

- first_only:

  Logical value indicating whether only the first mode should be
  returned if `x` has multiple modes (i.e. there are multiple values
  with the highest number of observations). Default is FALSE.

## Value

Returns the mode of the vector `x`

## Examples

``` r
countries::Mode(c("a","a",2,3))
#> [1] "a"
countries::Mode(c(1,1,2,3,NA,2))
#> [1] 1 2
countries::Mode(c(NA,NA,NA,1,1,2))
#> [1] NA
```
