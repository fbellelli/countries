#' Create a conversion table for country names
#'
#' This function returns a conversion table for country names to the desired naming conventions.
#' It can also be used to convert country names in different languages.
#' The use of fuzzy matching allows more flexibility in recognising and identifying country names.
#' @param x A vector of country names
#' @param to A vector containg one or more desired naming conventions to which \code{x} should be converted. Possible values are: "all", "ISO-3", "ISO-2", ... Default is "all".
#' @param fuzzy_match Logical value indicating whether fuzzy matching of country names should be allowed (\code{TRUE}), or only exact matches are allowed (\code{FALSE}). Default is \code{TRUE}.
#' @param verbose Logical value indicating whether the function should print to the console a report on the matching process
#' @param matching_info Logical value. If set to true the output match table will include additional information on the matching of \code{x}'s entries.
#' @param simplify Logical value. If set to \code{TRUE} the function will return a the match table as a data.frame object. If set to \code{FALSE}, the function will return a list object containing the match table and additional details on the country matching process. Default is \code{TRUE}.
#' @param custom_table Custom conversion table to be used. Default is \code{NULL}.
#' @return Returns a conversion table for countries names to the desired naming conventions.
#' @export
#' @import dplyr data.table magrittr stringdist stringr fastmatch assertthat
#' @examples
#' match_table(x=c("US","Italia","France","United States"), to= "ISO3")
match_table <- function(x,
                        to = "all",     # ISO3 ISO2 M49_name M49_code WB IMF WTO ...
                        fuzzy_match = TRUE,
                        verbose = FALSE,
                        matching_info = TRUE,
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

  x <- as.character(x)
  if (is.null(custom_table)){
    if (!all(to %in% c("all","ISO3","M49_code")) | length(to)<1)  stop("The value provided to the - to - argument is not valid")
    if (to %in% "all"){to <- c("ISO3","M49_code")}
    table_references<-read.csv("./programme files/country_reference_list_FULL.csv",header=TRUE,colClasses = "character")
    table_references_lower <- table_references %>% mutate_all(.funs=tolower)
    flat_references_lower <- unlist(table_references_lower)
    flat_references_lower <- flat_references_lower[flat_references_lower != "" & !is.na(flat_references_lower)]

  } else {

    #coerce to data frame and check that the provided table has at least two columns. otherwise give error
    custom_table <- as.data.frame(custom_table)
    if(ncol(custom_table)<2){stop("The table provided in - custom table - needs to have at least two column: a country ")}

    #check for "all" in to. if present then use all column names
    #fuzzy match column name - set maximum distance, otherwise give error

    #!this snippet needs to produce a table_references and a flat_references_lower
  }


  #_________________________________________________________


  # PREPARING CONVERSION TABLE:-----------------------------

  #extract list of countries from file
  list_countries <- sort(unique(x))
  if (length(list_countries)>250) warning(paste("Your data contains",length(list_countries), "unique country identifiers. That's a lot! Are you sure it contains only country names? You should check the conversion table."),call. = FALSE)

  #create table
  conversion_table <- data.frame(list_countries, simplified = str_trim(tolower(list_countries), side = "both"), exact_match = NA , closest_match=NA, dist=NA)

  #add conversionn columns for each of the desired standards
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

  #issue report
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
      cat("\n\nMultiple country IDs have been matched to the same country:")
      cat(paste0("\n  - ", na.omit(conversion_table)[repeated,"list_countries"]," : ",na.omit(conversion_table)[repeated,to[1]]))
    }
  } else {
    if (any(no_equiv>0)) message("Some country IDs have no match in one or more country naming conventions")
    if (any(repeated)) message("Multiple country IDs have been matched to the same country name")
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
                call = list(data = x,
                            to = to,     # ISO3 ISO2 M49_name M49_code WB IMF WTO ...
                            fuzzy_match = fuzzy_match,
                            verbose = verbose,
                            matching_info = matching_info,
                            simplify = simplify)))
  }
  #____________________________________________________________

}
