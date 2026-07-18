# World map data

A table containing points to draw a world map. The data comes from the
package maps ("world") An additional column is added with ISO 3-digit
country codes.

## Usage

``` r
world
```

## Format

A data frame with six columns providing information to plot world maps.

- long:

  Longitude

- lat:

  Latitude

- group:

  Numeric value used to identify polygons

- order:

  Order in which lines should be traced

- region:

  Name of the polygon's geographic region

- ISO3:

  3-digits ISO country code for the region
