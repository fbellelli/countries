#' Tests whether a string is a country name
#'
#' This function checks whether the string is a country name. It supports different languages and naming conventions.
#' For the purpose of this function, a string is considered a country name if it relates to one of the 249 countries of the ISO standard \code{3166}.
#' Fuzzy matching can be used to allow a small margin of error in the string.
#' @param x A character vector to be tested (also supports UN/ISO country codes)
#' @param fuzzy_margin A number between 0 and 1 indicating the margin of error tolerated by the fuzzy matching. 0 indicates that an exact match is requested. Default is 0.1 - i.e. up to 10% of the string characters are allowed to be wrong.
#' @return Returns a logical vector indicating whether the string is a country name
#' @seealso \link[Countries]{match_table} \link[Countries]{country_name}
#' @export
#' @examples
#' is_country(x=c("ITA","Estados Unidos","Estado Unidos","bungalow","dog",542), fuzzy_margin=0)
#' is_country(x=c("ITA","Estados Unidos","Estado Unidos","bungalow","dog",542), fuzzy_margin=0.1)
is_country <- function(x, fuzzy_margin=0.1){

  #check inputs
  if (fuzzy_margin>1 | fuzzy_margin<0) stop("Function argument - fuzzy_margin - needs to be a number between 0 and 1.")
  x <- as.character(x)

  #use match table to test unique values
  match <- suppressMessages(match_table(x, to="name_en", matching_info = TRUE))
  match$nchar <- nchar(match$list_countries)

  #test applying calliper
  match$is_country <- match$dist <= fuzzy_margin * match$nchar

  #set as FALSE matched entities that are not countries in the ISO standard
  match$is_country[is.na(match$name_en)]<-FALSE

  #return
  return(match$is_country[match(x,match$list_countries)])
}
