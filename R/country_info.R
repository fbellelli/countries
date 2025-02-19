#' Get information about countries
#'
#' This function is an interface for \href{https://restcountries.com/}{REST Countries API}.
#' It allows to request and download information about countries, such as: currency, capital city, language spoken, flag, neighbouring countries, and much more.
#' \strong{NOTE:} Internet access is needed to download information from the API. At times the API may be unstable or slow to respond.
#'
#' @param countries A vector of countries for which we wish to download information. The function also supports fuzzy matching capabilities to facilitate querying. Information is only returned for the 249 countries in the ISO standard \code{3166}.
#' @param fields Character vector indicating the fields to query. A description of the \href{https://gitlab.com/restcountries/restcountries/-/blob/master/FIELDS.md}{accepted fields can be found here}. Alternatively, a list of accepted field names can be obtained with the function \code{list_fields()}.
#' @param fuzzy_match Logical value indicating whether to allow fuzzy matching of country names. Default is \code{TRUE}.
#' @param match_info Logical value indicating whether to return information on country names matched to each input in \code{countries}. If \code{TRUE}, two additional columns will be added to the output (\code{matched_country} and \code{is_country}). Default is \code{FALSE}.
#' @param collapse Logical value indicating whether to collapse multiple columns relating to a same field together. Default is \code{TRUE}. For some specific fields (currencies, languages, names), multiple columns will be returned. This happens because countries can take multiple values for these fields. For example, \code{country_info("Switzerland", "languages", collapse = FALSE)} will return 4 columns for the field languages. When \code{collapse = TRUE}, these four columns will be collapsed into one string, with values separated by semicolons.
#' @param base_url Base URL used to construct the API calls. The default is \code{"restcountries.com:8080/v3.1/"}.
#' @returns Returns the requested information about the countries in a table. The rows of the table correspond to entries in \code{countries}, columns correspond to requested \code{fields}.
#' @seealso \link[countries]{list_fields}, \link[countries]{check_countries_api}
#' @export
#' @examples
#' # Run examples only if a connection to the API is available:
#' if (check_countries_api(warnings = FALSE)){
#'
#' # The example below queries information on the currency used in Brazil, US and France:
#' info <- country_info(countries = "Brazil", fields = "capital")
#'
#' # data for multiple countries can be requested
#' info <- country_info(countries = c("Brazil", "USA", "FR"), fields = "capital")
#'
#'#' # Data can be returned for all countries by leaving - countries - empty
#' info <- country_info(fields = "capital")
#'
#' # All available fields can be requested by leaving fields empty
#' info <- country_info(countries = c("Brazil", "USA", "FR"))
#'
#' # All information for all countries can be downloaded by leaving both arguments empty
#' info <- country_info()
#'
#' }
country_info <- function(countries = NULL, fields = NULL, fuzzy_match = TRUE, match_info = FALSE, collapse = TRUE, base_url = "restcountries.com:8080/v3.1/"){

  # check input format
  if (!is.logical(fuzzy_match) | length(fuzzy_match)!=1) stop("Function argument - fuzzy_match - needs to be a single logical statement (TRUE/FALSE)")
  if (!is.logical(match_info) | length(match_info)!=1) stop("Function argument - match_info - needs to be a single logical statement (TRUE/FALSE)")
  if (!(is.atomic(countries) || is.null(countries))) stop("Function argument - countries - needs to be a vector of country names")
  if (length(countries)>0){
    if (all(is.na(countries))) stop("All elements in input - countries - are NAs")
  }
  if (!is.null(fields)){
    if (!is.atomic(fields)) stop("Function argument - fields - needs to be a character vector")
    if (all(is.na(fields))) stop("Only NAs in function argument - fields -")
  }


  # convert inputs to character
  countries <- as.character(countries)
  fields <- as.character(fields)


  # IDENTIFY COUNTRIES TO QUERY ------------------

  if (length(countries) > 0){

    # check that provided input countries are actually countries
    inputs <- data.frame(original = countries,
                         is_country = is_country(countries, fuzzy_match = fuzzy_match))

    # deal with potential NAs in is_country
    inputs$is_country[is.na(inputs$is_country)] <- FALSE

    # translate country names to ISO 3 code for querying
    inputs$matched_country[inputs$is_country] <- suppressMessages(suppressWarnings(country_name(inputs$original[inputs$is_country], fuzzy_match = fuzzy_match, poor_matches = TRUE, verbose = FALSE)))

    # make a list without duplicates countries that will be queried
    list_countries <- unique(inputs$matched_country[inputs$is_country])


  } else {

    list_countries <- "all"
  }


  # if there is no country to query data for, return empty result
  if (length(list_countries) == 0){
    warning(paste0("No country was found in input - countries - returning an empty output.", if (fuzzy_match == FALSE) " (try setting fuzzy_match to TRUE?)" else ""))
    return(NULL)
  }

  # issue warning for inputs that are not recognised as countries
  if (length(countries)>0){
    if (!all(inputs$is_country)) warning(paste0("The following names were not recognised as countries, NAs will be returned", if (fuzzy_match == FALSE) " (try setting fuzzy_match to TRUE?)" else "" , ":\n - ",paste(unique(inputs$original[inputs$is_country == FALSE]), sep = "", collapse = "\n - ")))
  }


  # PREPARE QUERY --------------------------------

  # clean requested fields
  if (length(fields)>0){
    # add request for ISO 3-letter codes to requested fields (will be used for merging with input table)
    fields <- c(fields, "cca3")

    #remove any NA values
    fields <- fields[!is.na(fields)]
  }

  query <- paste0(base_url,
                  if (length(countries) == 0) "all?" else paste0("alpha?codes=", paste(stringr::str_to_lower(list_countries), collapse = ",", sep = "")),
                  if (length(fields) == 0) "" else paste0("&fields=", paste(fields, collapse = ",", sep = "")))


  # GET RESULTS ----------------------------------

  if (!curl::has_internet()){
    warning("Unable to connect to API. There might be a problem with your internet connection.")
    return(NULL)
  }

  # get results for query
  results <- "start query"
  try(results <- httr::GET(url = query), silent = TRUE)

  if (identical(results,  "start query")){
    warning("Unable to connect to API. There might be a problem with Countries REST API.")
    return(NULL)
  }

  # check query status
  status <- httr::status_code(results)
  if (status != 200){
    stop("Query failed with status ", status)
  }

  # Converting content to text
  data <- httr::content(results,"text", encoding = "UTF-8")

  # Parsing data in JSON
  data <- jsonlite::fromJSON(data, flatten = TRUE)

  # Converting into data.frame
  data <- as.data.frame(data)

  # issue warning when returned table is empty
  if (ncol(data[colnames(data) != "cca3"]) == 0){
    warning("Query resulted in empty table: none of the fields was recognised")

    # exit function
    return(NULL)

  } else {

    # check if any of the fields was not recognised and return a warning
    col_names <- colnames(data)
    not_a_field <- NULL
    for ( i in fields){
      if (!any(grepl(paste0("^",i), col_names, perl = TRUE))) not_a_field <- c(not_a_field, i)
    }
    if (length(not_a_field) >0) warning(paste0("No response for the following fields:\n - ", paste(not_a_field, collapse = "\n - ", sep = "")))

  }


  # COLLAPSE COLUMNS RELATING TO SAME FIELD -------


  regex_collapse <- c("^(languages)\\.[a-z]{3}()$",
                      "^(currencies)\\.[A-Z]{3}(\\.name)$",
                      "^(currencies)\\.[A-Z]{3}(\\.symbol)$",
                      "^(name.nativeName)\\.[a-z]{3}(\\.official)$",
                      "^(name.nativeName)\\.[a-z]{3}(\\.common)$")
  if (collapse == TRUE){
    for (i in regex_collapse){

      # check if collapse columns i are in table
      temp <- grepl(i, colnames(data), perl = TRUE)

      # if they are in the table, collapse them
      if (any(temp)){

        # extract column names to collapse
        cols <- colnames(data)[temp]
        other_cols <- colnames(data)[!temp]

        # prepare name for collapsed column
        new_name <- gsub(x = cols[1], pattern = i, replacement = "\\1\\2", perl = TRUE)

        # Collapse if there are multiple columns matching the pattern
        if (length(cols) > 1){

          # collapse columns
          temp <- as.data.frame(unname(apply(data[,temp], MARGIN = 1, FUN = function(x) paste(na.omit(x), collapse = "; ", sep = ""))))
          colnames(temp) <- new_name

          # replace columns with collapsed version
          data <- cbind(data[other_cols], temp)

        } else {

          # if only one column match the pattern, just change the name
          colnames(data)[colnames(data) == cols] <- new_name
        }
      }

    }
  }

  # paste together list columns
  if (collapse == TRUE){
    for (i in 1:ncol(data)){
      if (is.list(data[,i])){
        data[,i] <- unlist(lapply(data[,i], paste, collapse = "; ", sep = ""))
      }
    }


  }


  # PREPARE FINAL OUTPUT --------------------------

  if (length(countries) == 0){

    # if data was requested for all countries, return output as it is.
    return(data)

  } else {

    # change name of inputs columns
    colnames(inputs)[1] <- "countries"

    # if data was requested for a vector of country names, merge data with input vector
    final <- merge(inputs, data,  all.x = TRUE, by.x = "matched_country", by.y = "cca3", sort = FALSE)

    # remove country matching info if requested
    if (match_info == FALSE){
      final <- final[!colnames(final) %in% c("matched_country", "is_country")]
    }

    return(final)
  }

}
