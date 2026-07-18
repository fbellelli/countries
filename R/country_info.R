#' Get information about countries
#'
#' This function is an interface for \href{https://restcountries.com/}{REST Countries API}.
#' It allows to request and download information about countries, such as: currency, capital city, language spoken, flag, neighbouring countries, and much more.
#' \strong{NOTE:} From 2026, a personal key is needed to use the API, the key can be created on the API website \href{https://restcountries.com/}{REST Countries API}. Internet access is needed to download information from the API. At times the API may be unstable or slow to respond. Every time this function is executied it performs three API calls (the entire dataset is downloaded in three batches).
#'
#' @param key String containing the personal API key used to authenticate the user. The key can be created on the API website \href{https://restcountries.com/}{REST Countries API}.
#' @param countries (optional) A vector of countries for which we wish to filter the downloaded information. The function also supports fuzzy matching capabilities to facilitate filtering. When left blank or set to \code{NULL}, all the countries in the dataset are returned.
#' @param fields (optional) Character vector indicating the fields to query. If \code{fields} is left empty, all available fields will be returned. A description of the accepted fields can be found on the API website or it can be obtained with the function \code{list_fields()}.
#' @param fuzzy_match Logical value indicating whether to allow fuzzy matching of country names. Default is \code{TRUE}.
#' @param match_info Logical value indicating whether to return information on country names matched to each input in \code{countries}. If \code{TRUE}, two additional columns will be added to the output (\code{matched_country} and \code{is_country}). Default is \code{FALSE}.
#' @param base_url Base URL used to construct the API calls. The default is \code{"https://api.restcountries.com/countries/v5"}.
#' @returns Returns the requested information about the countries in a table. The rows of the table correspond to entries in \code{countries}, columns correspond to requested \code{fields}.
#' @seealso \link[countries]{list_fields}, \link[countries]{check_countries_api}
#' @export
#' @import httr
#' @importFrom jsonlite fromJSON
#' @examples
#' # Run examples only if a connection to the API is available:
#' if (check_countries_api(warnings = FALSE)){
#'
#' # NOTE: A VALID KEY IS NEEDED TO RUN THE EXAMPLES. CREATE ONE AT https://restcountries.com/
#'
#' # downloaded all the available information by leaving both countries and fields arguments empty
#' # The example below uses a test key (it will return a fixed output for Canada only)
#' info <- country_info(key = "rc_live_demo")
#'
#' # The example below filters the dataset to obtain information for one or more countries:
#' # info <- country_info(countries = "DR Congo", key = "YOUR_KEY")
#' # info <- country_info(countries = c("Morocco", "Brazil", "FR"), key = "YOUR_KEY")
#'
#' # The fields argument can be used to query only for specific information
#' # info <- country_info(fields = "capitals", key = "YOUR_KEY")
#' # info <- country_info(countries = c("Brazil", "USA", "FR"), fields = "capitals", key = "YOUR_KEY")
#'
#'}
country_info <- function(key, countries = NULL, fields = NULL, fuzzy_match = TRUE, match_info = FALSE, base_url = "https://api.restcountries.com/countries/v5"){

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

  # Query base
  query <- paste0(base_url,
                  "?pretty&limit=100")

  # add fields filter
  if (length(fields)>0){
    # add request for ISO 3-letter codes to requested fields (will be used for merging with input table)
    fields <- stringr::str_trim(unique(c(fields, "codes.alpha_3")))

    #remove any NA values
    fields <- fields[!is.na(fields)]

    # chain the fields to the request
    query <- paste0(query, "&response_fields=",
                    paste(fields, collapse = ","))

  }

  # The new API version does not allow to filter for multiple countries in one call so,
  # so for simplicity, the whole dataset will be queried and filtered locally
  # create multiple request batches
  query <- paste0(query, "&offset=", c(0, 100, 200))

  # clean authentication key information
  if (!grepl("bearer", key, ignore.case = TRUE)){
    key <- paste0("Bearer ", key)
  }



  # GET RESULTS ----------------------------------

  # initiate list to hold the results for all the batches
  data <- list()

  # get results for query
  for (batch in 1:length(query)){

    # Perform the GET request (URL, query, and headers combined)
    response <- httr::GET(
      url = query[batch],
      add_headers(Authorization = key)
    )

    # check fo errors before attempting to parse
    warn_for_status(response)
    if (response$status_code != 200){
      warning("Request failed")
      return(data.frame(Result = "API call failed (see warning message)"))
    }

    # Parse the response into JSON and flatten the nested objects
    parsed_data <- jsonlite::fromJSON(content(response, as = "text", encoding = "UTF-8"), flatten = TRUE)

    # Extract the country data
    data[[batch]] <- parsed_data$data$objects
  }

  # Extract a unique set of all column names across all batches
  all_names <- unique(unlist(lapply(data, names)))

  # Standardize the columns in every batch
  aligned_list <- lapply(data, function(df) {

    # Identify which columns are missing from this specific data frame
    missing_cols <- setdiff(all_names, names(df))

    # If there are missing columns, create them and fill with NA
    if (length(missing_cols) > 0) {
      df[missing_cols] <- NA
    }

    # Return the data frame with columns reordered to match the master list
    df[, all_names, drop = FALSE]
  })

  # bind the tables together
  data <- do.call(rbind, c(aligned_list, list(make.row.names = FALSE)))

  # check if any of the fields was not recognised and return a warning
  not_a_field <- NULL
  for (i in fields){
    if (!any(grepl(paste0("^",i,"(\\.\\w+|$)"), all_names, perl = TRUE))) not_a_field <- c(not_a_field, i)
  }
  if (length(not_a_field) >0) warning(paste0("No response for the following fields:\n - ", paste(not_a_field, collapse = "\n - ", sep = "")))







  # PREPARE FINAL OUTPUT --------------------------

  if (length(countries) == 0){

    # if data was requested for all countries, return output as it is.
    return(data)

  } else {

    # change name of inputs columns
    colnames(inputs)[1] <- "countries"

    # if data was requested for a vector of country names, merge data with input vector
    final <- cbind(inputs, data[match(inputs$matched_country, data$codes.alpha_3),])

    # remove country matching info if not requested
    if (match_info == FALSE){
      final <- final[!colnames(final) %in% c("matched_country", "is_country")]
    }

    # fix row numbers
    row.names(final) <- 1:nrow(final)

    return(final)
  }

}
