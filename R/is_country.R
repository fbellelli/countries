#' Tests whether a string is a country name
#'
#' This function checks whether the string is a country name. It supports different languages and naming conventions.
#' The function returns \code{TRUE} if it relates to one of the 249 countries of the ISO standard \code{3166}.
#' Alternatively, the argument \code{check_for} allows to narrow down the test to a subset of countries.
#' Fuzzy matching can be used to allow a small margin of error in the string.
#' @param x A character vector to be tested (also supports UN/ISO country codes)
#' @param check_for A vector of country names to narrow down testing. The function will return \code{TRUE} only if the string relates to a country in this vector. Default is NULL.
#' @param fuzzy_match A logical value indicating whether to tolerate small discrepancies in the country name matching. The default and fastest option is \code{FALSE}.
#' @returns Returns a logical vector indicating whether the string is a country name
#' @seealso \link[countries]{match_table}, \link[countries]{country_name}, \link[countries]{find_countrycol}
#' @export
#' @examples
#' #Detect strings that are country names
#' is_country(x=c("ITA","Estados Unidos","Estado Unidos","bungalow","dog",542), fuzzy_match=FALSE)
#' is_country(x=c("ITA","Estados Unidos","Estado Unidos","bungalow","dog",542), fuzzy_match=TRUE)
#' #Checking for a specific subset of countries
#' is_country(x=c("Ceylon","LKA","Indonesia","Inde"), check_for=c("India","Sri Lanka"))
is_country <- function(x, check_for=NULL, fuzzy_match=FALSE){

  #check inputs
  if (!is.logical(fuzzy_match) | length(fuzzy_match)!=1) stop("Function argument - fuzzy_match - needs to be a logical statement (TRUE/FALSE)")
  if (all(is.na(check_for))&!is.null(check_for)) stop("Function argument - check_for - needs to be a vector of country names.")
  if (is.null(x)){
    return(logical(0))
  } else {
    if (!is.atomic(x)) {
      stop("Function argument - x - needs to be a character vector")
    } else {
        if (all(is.na(x))) return(rep(NA, length(x)))
    }
  }
  x <- as.character(x)

  #clean inputs and give error if a country is not recognised exactly
  if (!is.null(check_for)){
    temp <- suppressWarnings(suppressMessages(country_name(check_for, to= "name_en", fuzzy_match = FALSE)))
    if (any(is.na(temp))) stop(paste0("Unable to recognise the following country name(s): ", paste0(check_for[is.na(temp)], collapse = ", "), "\nPlease try providing an ISO code or a simplified name"))
    check_for <- unique(temp)
  }

  #use match table to test unique values
  match <- suppressWarnings(suppressMessages(match_table(x, to="name_en", fuzzy_match = fuzzy_match, matching_info = TRUE, poor_matches = TRUE)))

  #test if string is country by applying a calipper to discrepancy
  if (fuzzy_match){
    match$is_country <- match$dist/pmin(sqrt(nchar(match$closest_match)),5) < 0.04
  } else {
    match$is_country <- match$exact_match
  }

  #set as FALSE matched entities that are not countries in the ISO standard
  match$is_country[is.na(match$name_en)]<-FALSE

  #set as FALSE matched entities if they are not in requested subset
  if (!is.null(check_for)){
    match$is_country[!(match$name_en %in% check_for)]<-FALSE
  }

  #return
  return(match$is_country[match(x,match$list_countries)])
}
