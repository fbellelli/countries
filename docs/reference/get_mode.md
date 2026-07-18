# Get the mode of a vector

This function returns the mode of a distribution. For any given vector
of values, it returns the value that appears most frequently. The
function works with strings, numerical and mixed inputs. `NA` values are
treated as a distinct value.

## Usage

``` r
get_mode(x, first_only = FALSE)
```

## Arguments

- x:

  A vector

- first_only:

  Logical value indicating whether only the first mode should be
  returned in `x` has multiple modes (i.e. there are multiple values
  with the highest number of observations). Default is FALSE.

## Value

Returns the mode of the vector `x`

## Examples

``` r
get_mode(c("a","a",2,3))
#> [1] "a"
get_mode(c(1,1,2,3,NA,2))
#> [1] 1 2
```
