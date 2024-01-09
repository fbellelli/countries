#' List of accepted fields for the function country_info
#'
#' This function queries \href{https://restcountries.com/}{REST Countries API} and returns a list of all possible fields that can be used in the function \code{country_info}.
#' \strong{NOTE:} Internet access is needed to download information from the API.
#'
#' @returns A vector of accepted fields for the function \code{country_info()}
#' @seealso \link[countries]{country_info}
#' @export
#' @examples
#' # Run example only if internet connection is available
#' if (curl::has_internet()){
#'
#' list_fields()
#'
#' }
list_fields <- function(){

  # download all country data to get all possible fields
  temp <- country_info()

  # extract and clean column names to remove sub-fields
  col_names <- colnames(temp)
  points <- stringr::str_locate(col_names, pattern = "\\.")
  cleaned_names <- unique(substr(col_names, 1, ifelse(is.na(points[,1]), 1000, points[,1] - 1)))

  # final output
  return(cleaned_names)
}
