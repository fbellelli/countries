#' Check for wide country data formats
#'
#' The function looks for country names or year information in the column names. This function is designed for simple panel country data, in which countries' time series are arranged side by side on columns or stacked on rows.
#' The function will only return year/country column names if at least 3 country/year column names are detected.
#' @param x A dataframe
#' @param adjacency Logical value indicating whether column names containing country or year information need to be adjacent to each other. Default is \code{TRUE}
#' @returns Returns a data.frame identifying the columns names that contain country or year information.
#' @seealso \link[countries]{find_keycol}, \link[countries]{find_countrycol}, \link[countries]{find_timecol}
#' @importFrom fastmatch %fin%
#' @export
#' @examples
#' example <- data.frame(Year=2000:2010, China=0:10, US=10:20, Vietnam=30:40)
#' check_wide_format(x=example)
check_wide_format <- function(x,
                              adjacency = TRUE){

  #------- CHECK INPUTS ------
  if(!is.data.frame(as.data.frame(x))|is.null(x)){stop("Argument - x - needs to be a dataframe")}
  if (!is.logical(adjacency) | length(adjacency)!=1) stop("Function argument - adjacency - needs to be a logical statement (TRUE/FALSE)")


  #------- PREP VARIABLES -----

  #extract column names
  cols <- colnames(x)

  #initiate output variable
  output <- NULL


  #------- CHECK FOR YEARS --------

  #define pattern to match
  yr_regex <- "([^0-9]{0,6})(\\d{4})"

  #look for 4-digits years in column names
  matches <- grep(pattern= yr_regex,
                  x=cols,
                  perl=TRUE)

  #If there is any match proceed with further checks
  if (length(matches)>0){

    #extract information from column names
    match_table <- as.data.frame(stringr::str_match(cols, yr_regex))
    colnames(match_table) <- c("col_name","pre","year")
    match_table$year <- as.numeric(match_table$year)

    #remove rows corresponding to non matching column names
    match_table <- match_table[matches,]

    #check if detected years are in a reasonable range:
    reasonable_yr <- all(match_table$year <=2200 & match_table$year >= 1800, na.rm = TRUE)

    #check that there is no multiple repeating year
    no_duplicated_yr <- !any(duplicated(match_table$year))

    #check that only one year name pattern is identified to false positives
    single_pattern <- length(unique(match_table$pre))==1

    #check that there are multiple years in column names (at least 2)
    multiple_yr <- nrow(match_table)>2

    #check column adjacency (we would expect year column to be next to each other)
    if (adjacency){
      adjacency_yr <- all(min(matches):max(matches) %in% matches)
    } else {
      adjacency_yr <- TRUE
    }

    #if criteria are fulfilled, pass column info as output
    if (reasonable_yr & adjacency_yr & no_duplicated_yr & single_pattern & multiple_yr){

      #prepare destination name(s) for columns
      match_table$col_indx <- matches

      #prepare outputs
      output<- data.frame(match_table[,c("year","col_indx","col_name")])
    }
  }


  #------- CHECK FOR COUNTRY NAMES -----

  #only proceed with checks if years are not found in column names
  if(length(matches)==0){

    # PERFORM NAME MATCHING WITH REFERENCE TABLE ----

    #prepare reference table for matching
    special_characters <- "[,\\. ,\\(\\)-_']"
    reference_table <- country_reference_list_long[country_reference_list_long$nomenclature != "ISO_code",] #exclude numeric ISO code
    reference_table$simplified <- stringr::str_trim(tolower(reference_table$name))   #all to lower case and trim
    reference_table$simplified <- gsub(special_characters,"",reference_table$simplified, perl = TRUE)   #remove special characters that are unlikely to appear in column names
    reference_table <- reference_table[!duplicated(reference_table$simplified) & reference_table$simplified != "" & !is.na(reference_table$simplified),] #keep only unique values and remove NAs and empty names, if any

    #transform column names for easier matching
    cols_simplified <- gsub(special_characters, "", stringr::str_trim(tolower(cols)), perl = TRUE)

    #find country matches
    matches <- fastmatch::fmatch(cols_simplified, reference_table$simplified)

    #If any country names is found, perform checks
    if (any(!is.na(matches))){

      #check that there are multiple country columns (at least 3)
      multiple_countries <- sum(!is.na(matches))>2

      #retrieve index of columns
      col_indx <- c(1:length(cols))[!is.na(matches)]

      #check for adjacency if required
      if (adjacency){
        adjacency_country <- all(min(col_indx):max(col_indx) %in% col_indx)
      } else {
        adjacency_country <- TRUE
      }

      #check that no duplicate country is found
      no_duplicated_country <- !any(duplicated(reference_table$ID[matches[!is.na(matches)]]))

      #prepare dataframe output
      if (adjacency_country & no_duplicated_country & multiple_countries){
        output<- data.frame(countries = suppressWarnings(suppressMessages(country_name(cols[col_indx], to="ISO3", poor_matches=TRUE))),
                            col_indx = col_indx,
                            col_name = cols[col_indx])
      }
    }
  }


  #------- OUTPUT RESULTS --------

  return(output)

}
