# Detecting the structure of data tables

The package introduces several functions that identify special columns
in country data. These functions can be used to automate basic tasks and
detect data formats. Here is an overview of a few functionalities.

## Example dataset

In this vignette we will use [a dataset of country
policies](https://fbellelli.com/EDB-data.html) as an example for the
functions in the package. This dataset contains 41 columns and has a
rather complex structure. Every row is identified by a combination of
Country name (column 21), Year (Column 9), Policy measure number (column
1), and HS code (column 39) — which is a code identifying traded
commodities. This dataset is an extension based on the [WTO
environmental databse (EDB)](https://edb.wto.org/). More information on
the dataset can be found
[here](https://www.wto.org/english/res_e/reser_e/ersd202203_e.pdf).

``` r

#download data
temp <- tempfile()
download.file("https://fbellelli.com/data%20files/Extended%20EDB%20by%20measure-country-HS%20chapter.zip", temp, mode="wb")

#unzip and load it in R
library(data.table)
#> 
#> Attaching package: 'data.table'
#> The following object is masked from 'package:base':
#> 
#>     %notin%
example <- fread(
  unzip(temp,"Extended EDB (by measure-country-HS chapter).csv"),
  stringsAsFactors=FALSE,
  encoding = "Latin-1")
```

## Finding columns containing country and time information

The functions
[`find_countrycol()`](https://fbellelli.github.io/countries/reference/find_countrycol.md)
and
[`find_timecol()`](https://fbellelli.github.io/countries/reference/find_timecol.md)
can be used to return respectively all columns containing country names
and date/year information.

In the example below, the function
[`find_countrycol()`](https://fbellelli.github.io/countries/reference/find_countrycol.md)
is used to examine our example dataset. The argument `min_share` is used
to indicate whether to look for an entire column of Country names or any
column that contains Country names. This is a numeric value indicating
the minimum share of entries in the column that are country names.

``` r

#Columns of country names
find_countrycol(example)  
#> [1] "COUNTRIES"

#Return any column containing a country name
find_countrycol(example, min_share=0) 
#> [1] "Notifying Member" "COUNTRIES"
```

Similarly, we can look for time columns in the following way:

``` r

# Date and year columns
find_timecol(example)  
#> [1] "Year"
```

By default, the function will return the name of the column. However,
the argument `return_index` can be used to get the indices of the column
in the table.

``` r

find_countrycol(example, return_index = TRUE)
#> [1] 22
find_timecol(example, return_index = TRUE)  
#> [1] 9
```

Computations may take long on large tables with many columns. To speed
up operations, these functions operate by evaluating the table on a
random sample of rows. The sample size can be adjusted with the argument
`sample_size`. It also possible to request to use the entire table by
passing the value `NA`. Finally, it is also possible to request not to
return columns containing `NA` values by specifying: `allow_NA = FALSE`.

These two functions are based on
[`is_country()`](https://fbellelli.github.io/countries/reference/is_country.md)
and
[`is_date()`](https://fbellelli.github.io/countries/reference/is_date.md),
which can be used to test if a string is a Country name and a date. An
overview of the first function is provided in the vignette *[Dealing
with country
names](https://fbellelli.github.io/countries/articles/dealing_with_names.html)*,
here we have a look at \`is_date()´.

## Testing dates

[`is_date()`](https://fbellelli.github.io/countries/reference/is_date.md)
takes a string vector as argument and outputs a boolean vector
indicating whether the strings are dates. The argument `formats` can be
used to specify the formats to be checked. Standard R notation can be
used for date elements (see the table below).

``` r

test <- c("December 2022", "20/01/1970", "Banana", "12.13.2000") 

is_date(test)
#> [1]  TRUE  TRUE FALSE  TRUE
is_date(test, formats=c("%d/%m/%Y"))
#> [1] FALSE  TRUE FALSE FALSE
```

| Symbol | Definition             | Example  |
|:-------|:-----------------------|:---------|
| %d     | Day number             | 13       |
| %a     | Abbreviated day name   | Mon      |
| %A     | Full day name          | Monday   |
| %m     | Month number           | 2        |
| %b     | Abbreviated month name | Feb      |
| %B     | Full month name        | February |
| %y     | Year 2-digits          | 22       |
| %Y     | Year 4-digits          | 2022     |

## Finding table keys

[`find_keycol()`](https://fbellelli.github.io/countries/reference/find_keycol.md)
is a function that can be used to automate the search of key columns in
datasets. A table key is a column or set of columns that uniquely
identifies the entries in the table. This function was specifically
designed for country data, so it will prioritise the search of country
and time columns, which are often keys in country data.

As shown below, the function correctly identifies the keys for the
complex dataset introduced at the beginning of this article.

``` r

find_keycol(example, allow_NA = TRUE)
#>                       country                          time 
#>                   "COUNTRIES"                        "Year" 
#>                         other                         other 
#>                          "Nr" "Tentative HS chapters match"
```

The function outputs a vector of column names that uniquely identifies
the entries in the table. In addition, it also indicates whether the
column contains *country*, *time* or *other* information. Just like the
other *find* function in this page, it is possible to request the column
index instead of the name by passing `return_index = TRUE`.

Due to the potentially very high number of column combinations in a
table, the function only tests the more likely options. The function
will first look for country and time columns. Next, left-most columns
are prioritised. Also, the function will only look for the most common
country data formats (e.g. cross-sectional, time-series, panel data,
dyadic, etc.) and only searches for up to two additional key columns of
type `other`. If no key is found at the end of the search, the function
returns a `NULL` value.

Computation can be slow on large datasets. Therefore, the function
evaluates only a random sample of rows from the table. The sample size
can be controlled with `sample_size`. Moreover, it is also possible to
restrict the search to a subset of columns by providing a vector of
column names or indices in the argument `search_only`.

``` r

#This will only check if a key is found among the first three columns
find_keycol(example, allow_NA = TRUE, search_only = 1:3)
#> NULL
```

Finally, the function also permits the user to specify whether columns
containing `NA` values are allowed to be considered as table keys. The
default is `allow_NA = FALSE` because keys should typically not contain
missing values. However, in our example dataset the column
`Tentative.HS.chapters.match` contains blank entries which are read in R
as `NA`, therefore the default would return `NULL`. The argument
`allow_NA` is present to deal with these edge cases.

``` r

find_keycol(example, allow_NA = FALSE)
#> NULL
```

If the user wants to test a specific set of columns, the package also
provides the function
[`is_keycol()`](https://fbellelli.github.io/countries/reference/is_keycol.md)
to check whether the provided columns uniquely identify the entries in
the dataset.

``` r

is_keycol(example, c("COUNTRIES","Year"), allow_NA = TRUE)
#> [1] FALSE
is_keycol(example, c("COUNTRIES","Year", "Nr","Tentative HS chapters match"), allow_NA = TRUE)
#> 
#> Found NA values in: Tentative HS chapters match
#> [1] TRUE
```
