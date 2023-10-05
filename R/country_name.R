#' Convert and translate country names
#'
#' This function recognises and converts country names to different nomenclatures and languages using a fuzzy matching algorithm.
#' \code{country_name()} can identify countries even when they are provided in mixed formats or in different languages. It is robust to small misspellings and recognises many alternative country names and old nomenclatures.
#' @param x A vector of country names
#' @param to A string containing the desired naming conventions to which \code{x} should be converted to (e.g. \code{"ISO3"}, \code{"name_en"}, \code{"UN_fr"}, ...). For a list of all possible values \href{https://fbellelli.github.io/countries/articles/dealing_with_names.html}{click here} or refer to the vignette on country names \code{vignette("dealing_with_names")}. Default is \code{"ISO3"}.
#' @param fuzzy_match Logical value indicating whether fuzzy matching of country names should be allowed (\code{TRUE}), or only exact matches are allowed (\code{FALSE}). Default is \code{TRUE}.
#' @param verbose Logical value indicating whether the function should print to the console a full report. Default is \code{FALSE}.
#' @param simplify Logical value. If set to \code{TRUE} the function will return a vector of converted names. If set to \code{FALSE}, the function will return a list object containing the converted vector and additional details on the country matching process. Default is \code{TRUE}.
#' @param poor_matches Logical value. If set to \code{FALSE} (the default), the function will return \code{NA} in case of poor matching. If set to \code{TRUE}, the function will always return the closest matching country name, even if the match is poor.
#' @param custom_table Custom conversion table to be used. This needs to be a \code{data.frame} object. Default is \code{NULL}.
#' @returns Returns a vector of converted country names. If multiple nomenclatures are passed to the argument \code{to}, the vectors are arranged in a data frame. If \code{simplify=FALSE}, the function will return a list object.
#' @seealso \link[countries]{is_country}, \link[countries]{match_table}, \link[countries]{find_countrycol}
#' @export
#' @examples
#' #Convert country names to a single nomenclatures: (e.g. 3-letters ISO code)
#' country_name(x=c("UK","Estados Unidos","Zaire","C#te d^ivoire"), to= "ISO3")
#'
#' #When multiple arguments are provided to the - to - argument, a data frame is returned:
#' country_name(x=c("UK","Estados Unidos","Zaire","C#te d^ivoire"), to= c("UN_en","UN_fr","ISO3"))
#'
#' #This function can also be used to translate country names: (e.g. translating all to Chinese)
#' country_name(x=c("UK","Estados Unidos","Zaire","C#te d^ivoire"), to= "name_zh")

country_name <- function(x,
                         to = "ISO3",
                         fuzzy_match = TRUE,
                         verbose = FALSE,
                         simplify = TRUE,
                         poor_matches = FALSE,
                         custom_table = NULL){

  #CHECK INPUTS (the rest is checked by match_table)
  if (!is.logical(simplify) | length(simplify)!=1) stop("Function argument - simplify - needs to be a logical statement (TRUE/FALSE)")

  if (all(is.na(x))){

    message("All values in argument - x - are NA or NULL")
    return(rep(NA, length(x)))

  } else{

    # BUILD CONVERSION TABLE
    matches <- match_table(x, to = to, fuzzy_match = fuzzy_match, verbose = verbose, matching_info= TRUE, simplify = FALSE, poor_matches = poor_matches, custom_table = custom_table)
    to <- matches$call$to

    # CONVERT DATA
    conv_table <- as.data.frame(x)
    colnames(conv_table) <- "Countries"
    conv_table <- dplyr::left_join(conv_table, matches$match_table[,c("list_countries",to)], by = c("Countries"="list_countries"))

    #warning
    if (verbose == FALSE & matches$warning) message("\nSet - verbose - to TRUE for more details")

    #RETURN RESULTS
    if (simplify){
      return(conv_table[,to])
    } else {
      matches$converted_data <- conv_table[,to]
      return(matches)
    }
  }
}
