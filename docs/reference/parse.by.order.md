# Internal function - Parses input merging order for function auto_join

Internal function - Parses input merging order for function auto_join

## Usage

``` r
parse.by.order(data, by = NULL)
```

## Arguments

- data:

  list of tables provided to auto_join function

- by:

  by order provided by user to auto_join function

## Value

Returns a cleaned `by` order and a vector indicating the type of data in
each element of the order.
