#' Check if the connection to Countries REST API is working
#'
#' Check if the connection to \href{https://restcountries.com/}{REST Countries API} is working. The function checks if the user has an internet connection and if any answer is returned from the Countries REST API.
#'
#' @param warnings Logical value indicating whether to output a warning when there is no connection. Default is \code{TRUE}.
#' @param timeout Numeric value giving the timeout in seconds for attempting connection to the API. Default is \code{4} second.
#' @returns Returns a logical value: \code{TRUE} if there is a connection, \code{FALSE} if there is no connection.
#' @seealso \link[countries]{list_fields}, \link[countries]{country_info}
#' @export
#' @examples
#' check_countries_api()
check_countries_api <- function(warnings = TRUE, timeout = 4){

  # check input format
  if (!is.logical(warnings) | length(warnings)!=1) stop("Function argument - warnings - needs to be a single logical statement (TRUE/FALSE)")
  if (!is.numeric(timeout) | length(timeout)!=1) stop("Function argument - timeout - needs to be a single numeric value")
  if (timeout <= 0) stop("Function argument - timeout - needs to be a positive value")

  # check if there is internet connection
  if (curl::has_internet()){

    # If there is internet connection, try to send a request to the API
    simple_query <- "restcountries.com/v3.1/alpha?codes=ita&fields=capital"

    # Send request to API
    results <- "start query"
    try(results <- httr::GET(url = simple_query, httr::timeout(timeout)), silent = TRUE)

    # If no result is returned, assume the API is down
    if (identical(results,  "start query")){
      output <- FALSE
      if (warnings == T) warning("Countries REST API appears to have some issues")
    }  else {
      output <- TRUE
    }


  } else {

    output <- FALSE
    if (warnings == T) warning("There might be a problem with your internet connection")
  }



  return(output)
}
