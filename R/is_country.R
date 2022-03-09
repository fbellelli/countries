#' Tests whether a string is a country name
#'
#' This function checks whether the string is a country name. It supports different languages and naming conventions.
#' The function returns \code{TRUE} if it relates to one of the 249 countries of the ISO standard \code{3166}.
#' Alternatively, the argument \code{check_for} allows to narrow down the test to a subset of countries.
#' Fuzzy matching can be used to allow a small margin of error in the string.
#' @param x A character vector to be tested (also supports UN/ISO country codes)
#' @param fuzzy_margin A number between 0 and 1 indicating the margin of error tolerated by the fuzzy matching. 0 indicates that an exact match is requested. Default is 0.1 - i.e. up to 10% of the string characters are allowed to be wrong.
#' @param check_for A vector of country names to narrow down testing. The function will return \code{TRUE} only if the string relates to a country in this vector. Default is NULL.
#' @return Returns a logical vector indicating whether the string is a country name
#' @seealso \link[Countries]{match_table} \link[Countries]{country_name}
#' @export
#' @examples
#' #Detect strings that are country names
#' is_country(x=c("ITA","Estados Unidos","Estado Unidos","bungalow","dog",542), fuzzy_margin=0)
#' is_country(x=c("ITA","Estados Unidos","Estado Unidos","bungalow","dog",542), fuzzy_margin=0.1)
#'
#' #Checking for a specific subset of countries
#' is_country(x=c("Ceylon","LKA","Indonesia","Inde"), check_for=c("India","Sri Lanka"))
is_country <- function(x, fuzzy_margin=0.1, check_for=NULL){

  #check inputs
  if (fuzzy_margin>1 | fuzzy_margin<0) stop("Function argument - fuzzy_margin - needs to be a number between 0 and 1.")
  if (all(is.na(check_for))&!is.null(check_for)) stop("Function argument - check_for - needs to be a vector of country names.")
  x <- as.character(x)

  #prendere unique e convertire inputs check_for. Dare errore se non riconosciuti.
  if (!is.null(check_for)){
    #clean inputs and give error if a country is not recognised exactly
    temp <- country_name(check_for, to= "name_en", fuzzy_match = FALSE)
    if (any(is.na(temp))) stop(paste0("Unable to recognise the following country name(s): ", paste0(check_for[is.na(temp)], collapse = ", "), "\nPlease try providing an ISO code or a simplified name"))
    check_for <- unique(temp)
  }

  #use match table to test unique values
  match <- suppressMessages(match_table(x, to="name_en", matching_info = TRUE))
  match$nchar <- nchar(match$list_countries)

  #test applying calliper
  match$is_country <- match$dist <= fuzzy_margin * match$nchar

  #set as FALSE matched entities that are not countries in the ISO standard
  match$is_country[is.na(match$name_en)]<-FALSE

  #set as FALSE matched entities if they are not in requested subset
  if (!is.null(check_for)){
    match$is_country[!(match$name_en %in% check_for)]<-FALSE
  }

  #return
  return(match$is_country[match(x,match$list_countries)])
}
