#' Create a conversion table for country names
#'
#' This function returns a conversion table for country names to the desired naming conventions and languages.
#' The use of fuzzy matching allows more flexibility in recognising and identifying country names.
#' @param x A vector of country names
#' @param to A vector containing one or more desired naming conventions to which \code{x} should be converted to (e.g. \code{"ISO3"}, \code{"name_en"}, \code{"UN_fr"}, ...). For a list of all possible values \href{https://fbellelli.github.io/countries/articles/dealing_with_names.html}{click here} or refer to the vignette on country names \code{vignette("dealing_with_names")}. Default is \code{c("simple", "ISO3")}.
#' @param fuzzy_match Logical value indicating whether fuzzy matching of country names should be allowed (\code{TRUE}), or only exact matches are allowed (\code{FALSE}). Default is \code{TRUE}. Switching to \code{FALSE} will result in much faster execution.
#' @param verbose Logical value indicating whether the function should print to the console a report on the matching process. Default is \code{FALSE}.
#' @param matching_info Logical value. If set to true the output match table will include additional information on the matching of \code{x}'s entries. Default is \code{FALSE}.
#' @param simplify Logical value. If set to \code{TRUE} the function will return the match table as a \code{data.frame} object. If set to \code{FALSE}, the function will return a list object containing the match table and additional details on the country matching process. Default is \code{TRUE}.
#' @param poor_matches Logical value. If set to \code{TRUE} (the default option), the function will always return the closest matching country name, even if the matching is poor. If set to \code{FALSE}, the function will return \code{NA} in case of poor matching.
#' @param na_fill Logical value. If set to \code{TRUE}, any \code{NA} in the output names will be filled with the original country name supplied in \code{x}. The default is \code{FALSE} (no filling). In general, \code{NA}s are produced if: 1) the country is not present in the nomenclature requested in \code{to} (e.g. \code{country_name("Abkhazia", to = "ISO3")}), 2) the input country name is \code{NA}, 3) No exact match is found and the user sets the option \code{fuzzy_match = FALSE}, 4) When the fuzzy match algorithm does not find a good match and the user sets the option \code{poor_match = FALSE}. The \code{na_fill} argument gives the option to replace the resulting NA with the original value in \code{x}.
#' @param custom_table Custom conversion table to be used. This needs to be a data.frame object. Default is \code{NULL}.
#' @returns Returns a conversion table for countries names to the desired naming conventions. If \code{simplify=FALSE} it returns a list object.
#' @seealso \link[countries]{country_name}, \link[countries]{is_country}
#' @export
#' @importFrom stats na.omit quantile
#' @examples
#' match_table(x=c("UK","Estados Unidos","Zaire","C#te d^ivoire"), to= c("UN_en","ISO3"))
match_table <- function(x,
                        to = c("simple","ISO3"),
                        fuzzy_match = TRUE,
                        verbose = FALSE,
                        matching_info = FALSE,
                        simplify = TRUE,
                        na_fill = FALSE,
                        poor_matches = TRUE,
                        custom_table = NULL){

  #CHECK VALIDITY OF FUNCTION ARGUMENTS :-------------------
  if (is.factor(x)) x <- as.character(x)
  if (is.null(x)) stop("NULL provided in argument - x")
  if (!is.atomic(x)) stop("The function argument - x - needs to be a vector")
  if (all(is.na(x))) return(rep(NA_character_, length(x)))
  if (!is.logical(fuzzy_match) | length(fuzzy_match)!=1) stop("Function argument - fuzzy_match - needs to be a logical statement (TRUE/FALSE)")
  if (!is.logical(verbose) | length(verbose)!=1) stop("Function argument - verbose - needs to be a logical statement (TRUE/FALSE)")
  if (!is.logical(matching_info) & length(matching_info)!=1) stop("Function argument - matching_info - needs to be a logical statement (TRUE/FALSE)")
  if (!is.logical(simplify) | length(simplify)!=1) stop("Function argument - simplify - needs to be a logical statement (TRUE/FALSE)")
  if (!is.logical(poor_matches) | length(poor_matches)!=1) stop("Function argument - poor_matches - needs to be a logical statement (TRUE/FALSE)")
  if (!is.logical(na_fill) | length(na_fill)!=1) stop("Function argument - na_fill - needs to be a logical statement (TRUE/FALSE)")
  #_________________________________________________________



  #LOADING REFERENCE TABLE AND PREP INPUTS :----------------

  list_nomenclatures <- c("simple", "ISO3", "ISO2", "ISO_code", "UN_ar", "UN_zh", "UN_en", "UN_fr", "UN_ru", "UN_es", "WTO_en", "WTO_fr", "WTO_es", "GTAP", "name_ar", "name_bg", "name_cs", "name_da", "name_de", "name_el", "name_en", "name_es", "name_et", "name_eu", "name_fi", "name_fr", "name_hu", "name_it", "name_ja", "name_ko", "name_lt", "name_nl", "name_no", "name_pl", "name_pt", "name_ro", "name_ru", "name_sk", "name_sv", "name_th", "name_uk", "name_zh", "name_zh-tw")
  x <- as.character(x)
  if (is.null(custom_table)){

    if (!all(to %in% c("all",list_nomenclatures)) | length(to)<1)  stop("The value provided to the - to - argument is not valid")
    if ("all" %in% to){to <- list_nomenclatures}
    table_references <- country_reference_list_long

  } else {

    #coerce to data frame and check that the provided table has at least two columns. otherwise give error
    custom_table <- as.data.frame(custom_table)
    if(!is.data.frame(custom_table)){stop("The table provided in - custom_table - needs to be coecible to a data.frame class")}
    if (to %in% "all"){to <- c(colnames(custom_table))}
    if (!all(to %in% c("all",colnames(custom_table)) | length(to)<1))  stop("The value provided to the - to - argument is not valid. It needs to be a vector of column names from the table used in the - custom_table - argument or the string `all`.")

    #verify argument to is valid
    if (!all(to %in% colnames(custom_table))){
      to <- c(colnames(custom_table))
      if (to != c("simple","ISO3")){warning("One or more of the - to - arguments cannot be found in the - custom_table - columns. Converting to all table's columns.\n")} #give warning if to is not on default
    }

    #transform to a long format
    custom_table <- as.data.frame(sapply(custom_table, as.character)) #transform all columns into character vectors
    columns <- colnames(custom_table) #make list of columns to pivot
    custom_table$ID <- row.names(custom_table) #create entry ID number
    custom_table <- tidyr::pivot_longer(custom_table,
                                        cols = dplyr::all_of(columns) ,
                                        names_to = "nomenclature",
                                        values_to = "name")
    table_references <- custom_table[!(is.na(custom_table$name)|custom_table$name==""),]
  }

  # change to lower case to facilitate matching
  table_references$name_lower <- stringr::str_to_lower(table_references$name)

  # create a shorter version of the reference table by eliminating duplicated entries and numeric country codes to avoid mismatches
  table_references_short <- table_references[!duplicated(table_references$name_lower) & table_references$nomenclature != "ISO_code",]


  #_________________________________________________________


  ##########################################################
  # PREPARING CONVERSION TABLE:
  ##########################################################

  #extract list of countries
  list_countries <- sort(unique(x))
  if (length(list_countries)>500) warning(paste("Your data contains",length(list_countries), "unique country identifiers. That's a lot! Are you sure it contains only country names? You should check the conversion table."),call. = FALSE)

  #check that values given in "to" are are among the column header name, otherwise stop execution
  if (!all(to %in% table_references$nomenclature)){
    stop("One or more of the values provided to the - to - argument is not valid. If a custom conversion is used, make sure the values in - to - are among the column names of the provided table.")
  }

  #check for encoding issues
  if(has.invalid.multibyte.string(list_countries)){
    temp <- has.invalid.multibyte.string(list_countries, return.elements = TRUE)
    n_temp <- sum(temp)
    stop(paste0("Check encoding of the data. Some of the characters are not read properly, such as in: ", paste(list_countries[temp][1:min(n_temp,5)], collapse = ", ", sep = "")))
  }

  #create table adding conversion columns for each of the desired naming conventions
  conversion_table <- data.frame(list_countries, simplified = stringr::str_trim(stringr::str_to_lower(list_countries), side = "both"), exact_match=NA, closest_match=NA, dist=NA, stats::setNames(rep(list(NA), length(to)),to))




  ##########################################################
  # COUNTRY MATCHING:
  ##########################################################

  # 1. EXACT MATCH
  #----------------

  # get index of first EXACT match
  exact_matches_index <- fastmatch::fmatch(conversion_table$simplified, table_references_short$name_lower, nomatch=NA)

  # 2. FUZZY MATCH
  #----------------

  #find index for closest matches
  if (any(is.na(exact_matches_index)) & fuzzy_match == TRUE){
    #remove stopwords from country vector
    stopwords <- stopwords_country_names
    conversion_table$simplified[is.na(exact_matches_index)] <- unlist(lapply(strsplit(conversion_table$simplified[is.na(exact_matches_index)], " "),
                                                                             function(x){paste(x[!x %in% stopwords], collapse = " ")}))
    #remove stopwords from reference table
    table_references_short$name_clean <- unlist(lapply(strsplit(table_references_short$name_lower, " "),
                                                       function(x){paste(x[!x %in% stopwords], collapse = " ")}))

    #Compute distance matrix
    distance_matrix <- stringdist::stringdistmatrix(conversion_table$simplified[is.na(exact_matches_index)],table_references_short$name_clean, method = "jw", p=0.2)

    #select first country with the closest matches
    fuzzy_matches_index <-apply(distance_matrix, 1, which.min)
  } else {
    distance_matrix <- NULL
    fuzzy_matches_index <- NULL
  }

  # 3. FILL CONVERSION TABLE
  #-------------------------

  #retrieve country ID from reference table for all closest matches
  matches_ID <- table_references_short$ID[exact_matches_index] #exact matches
  matches_ID[is.na(exact_matches_index)] <- if (fuzzy_match == TRUE & length(fuzzy_matches_index)>0){table_references_short$ID[fuzzy_matches_index]} else {NA} #fuzzy matches

  #fill converted country names in table
  if (any(!is.na(matches_ID))){
    temp <- table_references[table_references$ID %in% matches_ID & table_references$nomenclature %in% to,]
    if (nrow(temp)>0){
      temp <- tidyr::pivot_wider(temp[,c("ID","nomenclature","name")], values_from = "name", names_from = "nomenclature")

      # if no country in x is present in a nomenclature, temp will not have all - to - columns. HEre we deal with this edge case
      if (ncol(temp) - 1 < length(to) ) {
        for (i in colnames(temp)[colnames(temp) != "ID"]){
          conversion_table[,i] <- temp[fastmatch::fmatch(matches_ID, temp$ID), i]
        }
      } else {
        conversion_table[,to] <- temp[fastmatch::fmatch(matches_ID, temp$ID), to]
      }

    } else {
      conversion_table[,to] <- NA
    }

  }


  #FILL MATCHING INFORMATION IN CONVERSION TABLE

  #exact matches
  conversion_table$exact_match <- !is.na(exact_matches_index)
  conversion_table$closest_match <- table_references_short$name_lower[exact_matches_index]
  conversion_table$dist[!is.na(exact_matches_index)] <- 0

  #fuzzy matches
  conversion_table$closest_match[is.na(exact_matches_index)] <- if (!is.null(fuzzy_matches_index)){table_references_short$name_lower[fuzzy_matches_index]} else {NA}
  conversion_table$dist[is.na(exact_matches_index)] <- if (!is.null(distance_matrix)){apply(distance_matrix, 1, min)} else {NA}

  #eliminate poor matches if requested
  ids_poor_matches <- conversion_table$dist/pmin(sqrt(nchar(conversion_table$closest_match)),5) > 0.05
  if (poor_matches == FALSE & fuzzy_match == TRUE){
    for (i in to){
      conversion_table[ids_poor_matches, i] <- NA
    }
  }

  # Check if any conversion results in NA because country is not in destination nomenclature
  if (length(to)>1) no_equiv <- rowSums(is.na(conversion_table[,to])) else no_equiv <- is.na(conversion_table[,to])

  # fill NAs if requested
  if (na_fill){
    for (i in to){
      temp <- is.na(conversion_table[,i])
      conversion_table[temp, i] <- conversion_table$list_countries[temp]
    }
  }

  ##########################################################
  #MATCHING REPORT:
  ##########################################################

  #info for report:
  repeated <- duplicated(na.omit(conversion_table[,to]))|duplicated(na.omit(conversion_table[,to]),fromLast=TRUE)
  n_exact_matches <- sum(conversion_table$exact_match)
  n_matched <- sum(!is.na(conversion_table$closest_match))
  n_poor_matches <- sum(ids_poor_matches)
  uncertain_matches <- list_countries[ids_poor_matches]
  uncertain_matches_to <- if (poor_matches) conversion_table[ids_poor_matches, to[1]] else NA
  missing_conversion <- (no_equiv>0) & (conversion_table$exact_match == TRUE | !is.na(conversion_table$closest_match)) & (poor_matches == TRUE | poor_matches == FALSE & !(conversion_table$list_countries %in% na.omit(uncertain_matches)))
  if (matching_info | !simplify){
    dist_summary <- summary(conversion_table$dist[!conversion_table$exact_match], na.rm=TRUE)
  }

  #issue report
  output_warning <- FALSE
  if (verbose){
    # Number of exact matching and fuzzy matching
    cat(paste0("\nIn total ",length(list_countries)," unique country names were provided\n",n_exact_matches,"/",length(list_countries)," have been matched with EXACT matching\n"))
    if (fuzzy_match == TRUE){
      cat(paste0(
        sum(!conversion_table$exact_match),"/",length(list_countries)," have been matched with FUZZY matching",
        if (n_poor_matches>0) paste0(", out of which:\n", n_poor_matches, "/", length(list_countries) - n_exact_matches, " are a POOR match (likely wrongly identified)\n") else "\n"
      ))
    }
    if (n_matched < length(list_countries)) cat(paste0("Unable to find an exact match for ",length(list_countries)- n_matched, "/",length(list_countries)))

    # Summary statistics on fuzzy matching
    # if(fuzzy_match & matching_info & n_exact_matches<length(list_countries)){
    #   cat("\nFuzzy matching DISTANCE summary:")
    #   cat("\n | Average: ",mean(conversion_table$dist[!conversion_table$exact_match]))
    #   cat(paste0("\n | ",c("Min: ","Q1: ","Median: ","Q3: ","Max: "),quantile(conversion_table$dist[!conversion_table$exact_match])))
    # }
  } else {
    if (fuzzy_match == FALSE & n_exact_matches < length(list_countries)) message("Unable to find an EXACT match for all country names, NA is returned. (We suggest trying with - fuzzy_match = TRUE)")
  }

  # Message on missing conversion
  if (any(missing_conversion)){
    output_warning <- TRUE
    if (verbose){
      cat("\n\nThe following country IDs do not have a match in one or more of the requested naming conventions, ", if (na_fill) "used original name to fill the NAs:" else paste0("NA returned:\n(To avoid NAs, use - to = 'simple'- or set - na_fill = TRUE)"))
      cat(paste0("\n  - ", conversion_table$list_countries[missing_conversion]))
    } else {
      message(paste0("Some country IDs have no match in one or more of the requested country naming conventions, ", if (na_fill) "used original name to fill the NAs." else "NA returned."))
    }
  }

  # Message on multiple ids matched to same country
  if (any(repeated) & n_matched>0){
    output_warning <- TRUE
    if (verbose){
      cat("\n\nMultiple arguments have been matched to the same country name:")
      cat(paste0("\n  - ", na.omit(conversion_table)[repeated,"list_countries"]," : ",na.omit(conversion_table)[repeated,to[1]]))
    } else {
      message("Multiple country IDs have been matched to the same country name.")
    }
  }

  #Message on uncertain matches
  if( n_matched > 0 & length(na.omit(uncertain_matches))>0 & all(!is.na(uncertain_matches))){
    output_warning <- TRUE

    if (verbose){
      if (poor_matches == TRUE){
        cat("\n\nThe matching for the following countries is likely inaccurate:\n(Set - poor_matches - to FALSE to return NAs and - na_fill - to replace the NAs with the original name in - x)")
        cat(paste0("\n - ", uncertain_matches," : ",uncertain_matches_to))
      } else {
        if (na_fill){
          cat("\n\nNo close match found for the following countries, kept the original name:")
        } else {
          cat("\n\nNo close match found for the following countries, NA returned:")
        }
        cat("\n(set - poor_matches - to TRUE if you want the closest match to be returned or set - na_fill - to TRUE if you wish to fill the NAs with the original name supplied in - x)")
        cat(paste0("\n - ", uncertain_matches))
      }
    } else {
      message(paste0("There is low confidence on the matching of some country names", if(poor_matches == FALSE & na_fill == TRUE) ", keeping the original names in - x." else if (poor_matches == TRUE) ", returning the closest match." else ", NA returned."))
    }
  }

  if (verbose){cat("\n\n")}
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
                               ids_no_equiv = if (any(missing_conversion)) conversion_table$list_countries[missing_conversion] else NULL,
                               ids_confluent = if (any(repeated)) data.frame(ID = na.omit(conversion_table)[repeated,"list_countries"], to = na.omit(conversion_table)[repeated,to[1]]) else NULL,
                               ids_uncertain = if (n_matched > 0 & length(na.omit(uncertain_matches))>0 & all(!is.na(uncertain_matches))) data.frame(uncertain_matches, uncertain_matches_to) else NULL),
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
