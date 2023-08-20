#' World map data
#'
#' A table containing points to draw a world map. The data comes from the package maps ("world") An additional column is added with ISO 3-digit country codes.
#'
#' @format A data frame with six columns providing information to plot world maps.
#' \describe{
#'   \item{long}{Longitude}
#'   \item{lat}{Latitude}
#'   \item{group}{Numeric value used to identify polygons}
#'   \item{order}{Order in which lines should be traced}
#'   \item{region}{Name of the polygon's geographic region}
#'   \item{ISO3}{3-digits ISO country code for the region}
#' }
"world"

data(world, envir=environment())
