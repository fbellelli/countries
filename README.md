
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Countries <img src='man/figures/hexagon_sticker.png' align="right" height="180px" />

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

## Work in progress:

-   function to test strings on whether they are country names
-   functions to identify columns in dataframes containing country names
    and date information
-   function for downloading up-to-date information on countries
    (e.g. currency, language, population, etc.)
-   function for downloading country data for analysis from different
    sources (e.g. UN, World Bank, FRED, etc.)
-   function to quickly merge country data from different sources
-   function to tag countries based on common criteria (e.g. developing
    status, World bank income group, geographic region, etc.)
-   function to easily plot chloropleth maps
-   publish on CRAN
