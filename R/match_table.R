#' Create a conversion table for country names
#'
#' This function returns a conversion table for country names to the desired naming conventions.
#' It can also be used to convert country names in different languages.
#' The use of fuzzy matching allows more flexibility in recognising and identifying country names.
#' @param x A vector of country names
#' @param to A vector containing one or more desired naming conventions to which \code{x} should be converted to. For a list of possible values use: \code{print_nomenclatures()}. Default is "ISO3".
#' @param fuzzy_match Logical value indicating whether fuzzy matching of country names should be allowed (\code{TRUE}), or only exact matches are allowed (\code{FALSE}). Default is \code{TRUE}.
#' @param verbose Logical value indicating whether the function should print to the console a report on the matching process. Default is \code{FALSE}.
#' @param matching_info Logical value. If set to true the output match table will include additional information on the matching of \code{x}'s entries.
#' @param simplify Logical value. If set to \code{TRUE} the function will return the match table as a \code{data.frame} object. If set to \code{FALSE}, the function will return a list object containing the match table and additional details on the country matching process. Default is \code{TRUE}.
#' @param custom_table Custom conversion table to be used. This needs to be a data.frame object. Default is \code{NULL}.
#' @return Returns a conversion table for countries names to the desired naming conventions.
#' @export
#' @import dplyr magrittr assertthat
#' @importFrom stringdist stringdist
#' @importFrom stringr str_trim
#' @importFrom stats na.omit quantile
#' @importFrom utils data
#' @examples
#' match_table(x=c("UK","Estados Unidos","Zaire","C#te d^ivoire"), to= c("UN_en","ISO3"))
match_table <- function(x,
                        to = c("name_en","ISO3"),
                        fuzzy_match = TRUE,
                        verbose = FALSE,
                        matching_info = FALSE,
                        simplify = TRUE,
                        custom_table = NULL){

  #CHECK VALIDITY OF FUNCTION ARGUMENTS :-------------------
  stopifnot(is.vector(x))
  if (!is.vector(x)) stop("The function argument - x - needs to be a vector")
  if (!is.logical(fuzzy_match) & length(fuzzy_match)==1) stop("Function argument - fuzzy_match - needs to be a logical statement (TRUE/FALSE)")
  if (!is.logical(verbose) | length(verbose)!=1) stop("Function argument - verbose - needs to be a logical statement (TRUE/FALSE)")
  if (!is.logical(matching_info) & length(matching_info)==1) stop("Function argument - matching_info - needs to be a logical statement (TRUE/FALSE)")
  if (!is.logical(simplify) | length(simplify)!=1) stop("Function argument - simplify - needs to be a logical statement (TRUE/FALSE)")
  #_________________________________________________________


  #LOADING REFERENCE TABLE AND PREP INPUTS :----------------

  list_nomenclatures <- c("simple", "ISO3", "ISO2", "ISO_code", "UN_ar", "UN_zh", "UN_en", "UN_fr", "UN_ru", "UN_es", "WTO_en", "WTO_fr", "WTO_es", "GTAP", "name_ar", "name_bg", "name_cs", "name_da", "name_de", "name_el", "name_en", "name_es", "name_et", "name_eu", "name_fi", "name_fr", "name_hu", "name_it", "name_ja", "name_ko", "name_lt", "name_nl", "name_no", "name_pl", "name_pt", "name_ro", "name_ru", "name_sk", "name_sv", "name_th", "name_uk", "name_zh", "name_zh-tw")
  x <- as.character(x)
  if (is.null(custom_table)){
    if (!all(to %in% c("all",list_nomenclatures)) | length(to)<1)  stop("The value provided to the - to - argument is not valid")
    if ("all" %in% to){to <- list_nomenclatures}
    utils::data("country_reference_list")
    table_references <- country_reference_list
  } else {

    #coerce to data frame and check that the provided table has at least two columns. otherwise give error
    custom_table <- as.data.frame(custom_table)
    if(!is.data.frame(custom_table)){stop("The table provided in - custom_table - needs to be coecible to a data.frame class")}
    if (to %in% "all"){to <- c(colnames(custom_table))}
    if (!all(to %in% c("all",colnames(custom_table)) | length(to)<1))  stop("The value provided to the - to - argument is not valid. It needs to be a vector of column names from the table used in the - custom_table - argument or the string `all`.")
    table_references <- custom_table
  }

  #prepare the reference table for matching
  table_references_lower <- table_references %>% mutate_all(.funs=tolower)
  flat_references_lower <- unlist(table_references_lower)
  flat_references_lower <- flat_references_lower[flat_references_lower != "" & !is.na(flat_references_lower)]
  #_________________________________________________________


  # PREPARING CONVERSION TABLE:-----------------------------

  #extract list of countries from file
  list_countries <- sort(unique(x))
  if (length(list_countries)>250) warning(paste("Your data contains",length(list_countries), "unique country identifiers. That's a lot! Are you sure it contains only country names? You should check the conversion table."),call. = FALSE)

  #create table
  conversion_table <- data.frame(list_countries, simplified = str_trim(tolower(list_countries), side = "both"), exact_match = NA , closest_match=NA, dist=NA)

  #check that values given in "to" are are among the column header name, otherwise stop execution
  if (!all(to %in% colnames(table_references))){
    stop("One or more of the values provided to the - to - argument is not valid. If a custom conversion is used, make sure the values in - to - are among the column names of the provided table.")
  }

  #add conversion columns for each of the desired naming conventions
  for (i in to){
    conversion_table[,i] <- NA
  }
  #_________________________________________________________


  # COUNTRY MATCHING: --------------------------------------

  #loop over every country to find a match
  for (i in 1:length(list_countries)){

    #make a table of EXACT MATCHES
    matches <- as.data.frame(which(table_references_lower == conversion_table$simplified[i], arr.ind=TRUE))

    if (nrow(matches)>0){
      # find exact match
      conversion_table[i,to] <- table_references[get_mode(matches$row), to]

      # fill in table
      conversion_table[i,c("exact_match","closest_match","dist")]<-c(TRUE,conversion_table$simplified[i],0)
    } else {
      #USE FUZZY MATCHING WHEN NECESSARY
      # fill in table
      conversion_table$exact_match[i] <- FALSE

      if(fuzzy_match){
        # find closest matches using string distance osa method.
        dist <- stringdist(conversion_table$simplified[i],flat_references_lower,method = "osa")
        min_dist <- min(dist)
        matches <-  unique(flat_references_lower[dist == min(dist)])

        #if multiple matches are found, also checks lcs and jw distance metrics and pick the match that is more frequent
        if (length(unique(matches))>1){
          dist <- stringdist(conversion_table$simplified[i],flat_references_lower,method = "lcs")
          matches <-  c(matches,unique(flat_references_lower[dist == min(dist)]))
          dist <- stringdist(conversion_table$simplified[i],flat_references_lower,method = "jw")
          matches <-  c(matches,unique(flat_references_lower[dist == min(dist)]))

          match <- names(sort(table(matches), decreasing=TRUE))[1]
        } else {
          match <- unique(matches)
        }

        # fill in table
        conversion_table[i,c("closest_match","dist")] <- c(match,min_dist)
        matches <- as.data.frame(which(table_references_lower == match, arr.ind=TRUE))
        conversion_table[i,to] <- table_references[get_mode(matches$row), to]
      }
    }
  }

  # When naming convention has no equivalent insert NA
  conversion_table[,to][conversion_table[,to] == ""] <- NA
  conversion_table$dist <- as.numeric(conversion_table$dist)
  conversion_table$exact_match <- as.logical(conversion_table$exact_match)
  #__________________________________________________________


  #MATCHING REPORT:------------------------------------------

  #info for report:
  if (length(to)>1) no_equiv <- rowSums(is.na(conversion_table[,to])) else no_equiv <- is.na(conversion_table[,to])
  repeated <- duplicated(na.omit(conversion_table[,to]))|duplicated(na.omit(conversion_table[,to]),fromLast=TRUE)
  n_exact_matches <- sum(conversion_table$exact_match)
  dist_summary <- summary(conversion_table$dist[!conversion_table$exact_match], na.rm=TRUE)
  uncertain_matches <- list_countries[conversion_table$dist/nchar(list_countries) > 0.4]
  uncertain_matches_to <- conversion_table[conversion_table$dist/nchar(list_countries) > 0.4, to[1]]

  #issue report
  output_warning <- FALSE
  if (verbose){
    # Exact matching and fuzzy matching
    cat(paste0("\nIn total ",length(list_countries)," unique country identifiers have been found\n",n_exact_matches,"/",length(list_countries)," have been matched with EXACT matching"))
    if(fuzzy_match & n_exact_matches<length(list_countries)){
      cat(paste0("\n",sum(!conversion_table$exact_match),"/",length(list_countries)," have been matched with FUZZY matching\n"))
      cat("\nFuzzy matching DISTANCE summary:")
      cat("\n | Average: ",mean(conversion_table$dist[!conversion_table$exact_match]))
      cat(paste0("\n | ",c("Min: ","Q1: ","Median: ","Q3: ","Max: "),quantile(conversion_table$dist[!conversion_table$exact_match])))
    }

    # Message on missing conversion
    if (any(no_equiv>0)){
      cat("\n\nThe following country IDs do not have a match in one or more of the naming conventions:")
      cat(paste0("\n  - ", conversion_table$list_countries[no_equiv>0]))
    }

    # Message on multiple ids matched to same country
    if (any(repeated)){
      cat("\n\nMultiple arguments have been matched to the same country name:")
      cat(paste0("\n  - ", na.omit(conversion_table)[repeated,"list_countries"]," : ",na.omit(conversion_table)[repeated,to[1]]))
    }

    #Message on uncertain matches
    if(length(uncertain_matches)>0 & all(!is.na(uncertain_matches))){
      cat("\n\nThe matching for the following countries could be inaccurate:")
      cat(paste0("\n - ", uncertain_matches," : ",uncertain_matches_to))
    }
  } else {
    if (any(no_equiv>0)){
      message("Some country IDs have no match in one or more country naming conventions")
      output_warning <- TRUE}
    if (any(repeated)){
      message("Multiple country IDs have been matched to the same country name")
      output_warning <- TRUE}
    if (length(uncertain_matches)>0){
      message("There is low confidence on the matching of some country names")
      output_warning <- TRUE}
  }
  cat("\n\n")
  #___________________________________________________________


  # OUTPUT RESULTS -------------------------------------------

  #clean output
  conversion_table$simplified <- NULL
  if (!matching_info){
    conversion_table$exact_match <- NULL
    conversion_table$closest_match <- NULL
    conversion_table$dist <- NULL
  }

  #return conversion table
  if (simplify){
    return(conversion_table)
  } else {
    return(list(converted_data = NULL,
                match_table = conversion_table,
                summary = list(tot_IDs = length(list_countries),
                               n_exact_matches = n_exact_matches,
                               n_fuzzy_matches = if (fuzzy_match) length(list_countries) - n_exact_matches else NULL,
                               dist_summary = if (fuzzy_match & n_exact_matches < length(list_countries)) dist_summary else NULL,
                               ids_no_equiv = if (any(no_equiv>0)) conversion_table$list_countries[no_equiv>0] else NULL,
                               ids_confluent = if (any(repeated)) data.frame(ID = na.omit(conversion_table)[repeated,"list_countries"], to = na.omit(conversion_table)[repeated,to[1]]) else NULL),
                warning = output_warning,
                call = list(data = x,
                            to = to,     # ISO3 ISO2 M49_name M49_code WB IMF WTO ...
                            fuzzy_match = fuzzy_match,
                            verbose = verbose,
                            matching_info = matching_info,
                            simplify = simplify)))
  }
  #____________________________________________________________

}
