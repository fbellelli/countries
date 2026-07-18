# Getting started

## Installing and loading the package

Since the package is not yet on CRAN, it needs to be downloaded directly
from the Github repository. This can be done with the `devtools`
package.

``` r

# Install and load devtools
install.packages("devtools")
library(devtools)

# Install and load countries package
devtools::install_github("fbellelli/countries", build_vignettes = TRUE)
library(countries)
```
