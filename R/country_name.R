#' Convert and translate country names
#'
#' This function recognises and converts country names to different nomenclatures and languages.
#' It supports fuzzy matching for greater flexibility.
#' @param x A vector of country names
#' @param to A string containing the desired naming conventions to which \code{x} should be converted to. Possible values are: "all", "ISO-3", "ISO-2", ... Default is "ISO3".
#' @param fuzzy_match Logical value indicating whether fuzzy matching of country names should be allowed (\code{TRUE}), or only exact matches are allowed (\code{FALSE}). Default is \code{TRUE}.
#' @param verbose Logical value indicating whether the function should print to the console a report. Default is \code{FALSE}.
#' @param simplify Logical value. If set to \code{TRUE} the function will return a vector of converted names. If set to \code{FALSE}, the function will return a list object containing the the converted vector and additional details on the country matching process. Default is \code{TRUE}.
#' @param custom_table Custom conversion table to be used. This needs to be a data.frame object. Default is \code{NULL}.
#' @return Returns a vector of converted country names.
#' @export
#' @import dplyr magrittr assertthat
#' @examples
#' country_name(x=c("US","Italia","France","United States"), to= "ISO3")
country_name <- function(x,
                          to = "ISO3",     # all ISO3 ISO2 M49_name M49_code WB IMF WTO ...
                          fuzzy_match = TRUE,
                          verbose = FALSE,
                          simplify = TRUE,
                          custom_table = NULL){

  #CHECK INPUTS (the rest is checked by match_table)
  if (!is.logical(simplify) | length(simplify)!=1) stop("Function argument - simplify - needs to be a logical statement (TRUE/FALSE)")

  # BUILD CONVERSION TABLE
  matches <- match_table(x, to = to, fuzzy_match = fuzzy_match, verbose = verbose, matching_info= TRUE, simplify = FALSE, custom_table = custom_table)
  to <- matches$call$to

  # CONVERT DATA
  conv_table <- as.data.frame(x)
  colnames(conv_table) <- "Countries"
  conv_table <- left_join(conv_table, matches$match_table[,c("list_countries",to)], by = c("Countries"="list_countries"))

  #warning
  if (verbose == FALSE & (!is.null(matches$summary$ids_no_equiv)|!is.null(matches$summary$ids_confluent))) message("Set - verbose - to TRUE for more details")

  #RETURN RESULTS
  if (simplify){
    return(conv_table[,to])
  } else {
    matches$converted_data <- conv_table[,to]
    return(matches)
  }
}
