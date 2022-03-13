
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Countries

<!-- badges: start -->
<!-- badges: end -->

´Countries´ is an R package designed to quickly wrangle, merge and
explore country data. This package contains functions to easily identify
and convert country names, pull country info and datasets, merge country
data from different sources, and make quick world maps.

## Installing and loading the package

Since the package is not yet on CRAN, the development version needs to
be downloaded directly from the Github repository. This can be done with
the `devtools` package.

``` r
# Install and load devtools
install.packages("devtools")
library(devtools)

# Install Countries
devtools::install_github("fbellelli/countries", build_vignettes = TRUE)
```

The package can then be loaded normally

``` r
library(Countries)
```

## Dealing with country names

The package contains several functions to work with country names. For
instance, the function `is_country()` can be used to test for country
names or subsets of countries:

``` r
#Detect strings that are country names
is_country(x=c("ITA","Estados Unidos","bungalow","dog",542))
#> [1]  TRUE  TRUE FALSE FALSE FALSE

#Checking for a specific subset of countries
is_country(x=c("Ceylon","LKA","Indonesia","Inde"), check_for=c("India","Sri Lanka"))
#> [1]  TRUE  TRUE FALSE  TRUE
```

The function `country_name()` can be used to convert country names to
different naming conventions or to translate them to different
languages. `country_name()` can identify countries even when they are
provided in mixed formats or in different languages. It is robust to
small misspellings and recognises many alternative country names and old
nomenclatures.

``` r
example <- c("US","C@ète d^Ivoire", "Morocco","FYROM", "Arabie Saoudite")

# Getting 3-letters ISO code
country_name(x= example, to="ISO3")
#> [1] "USA" "CIV" "MAR" "MKD" "SAU"

# Translating to spanish
country_name(x= example, to="name_es")
#> [1] "Estados Unidos"      "Costa de Marfil"     "Marruecos"          
#> [4] "Macedonia del Norte" "Arabia Saudita"

# Getting multiple nomenclatures
country_name(x= example, to=c("ISO3","ISO2","UN_en"))
#>   ISO3 ISO2                    UN_en
#> 1  USA   US United States of America
#> 2  CIV   CI            Côte d’Ivoire
#> 3  MAR   MA                  Morocco
#> 4  MKD   MK          North Macedonia
#> 5  SAU   SA             Saudi Arabia
```

Learn more about country names functions in [this
article](/articles/dealing_with_names.html).

## Work in progress:

-   function for downloading up-to-date information on countries
    (e.g. currency, language, population, etc.)
-   function for downloading country data for analysis from different
    sources (e.g. UN, World Bank, FRED, etc.)
-   function to quickly merge country data from different sources
-   function to tag countries based on common criteria (e.g. developing
    status, World bank income group, geographic region, etc.)
-   function to easily plot chloropleth maps
-   publish on CRAN
