#' Conversion table in long format
#'
#' A table containing country names in different naming conventions
#'
#' @format A data frame with three columns providing information on country naming conventions. This table is a long-format version of "country_reference_list".
#' \describe{
#'   \item{ID}{Numeric value that uniquely identifies entity. This corresponds to the row number in table "country_reference_list".}
#'   \item{nomenclature}{Country naming convention (e.g. UN english, ISO 3-digit code, etc.).}
#'   \item{name}{Country names}
#' }
"country_reference_list_long"

data(country_reference_list_long, envir=environment())




