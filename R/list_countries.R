#' Get a list of country names
#'
#' This function returns a vector of country names in different nomenclatures.
#'
#' @param nomenclature String indicating the nomenclature from which the list of countries should be taken. Not all countries are present in all nomenclatures, for example Taiwan is not recognised by the UN, so it will not be returned with \code{"WTO_en"}. The function accepts any of the nomenclatures supported country_name. For a list of accepted values, refer to \href{https://fbellelli.github.io/countries/articles/dealing_with_names.html}{this page}. The default is \code{name_en}, which is the English list of names in the ISO standard 3166.
#' @returns A vector of country names in the desired nomenclature.
#' @export
#' @seealso \link[countries]{random_countries}, \link[countries]{country_name}
#' @examples
#' list_countries("ISO3")
#' list_countries("UN_en")
#' list_countries()
list_countries <- function(nomenclature = "name_en") {

  # CHECK INPUTS ----

  # check input type is as expected
  if (!is.character(nomenclature)){stop("Argument - nomenclature - needs to a string")}
  if (length(nomenclature)!=1){stop("Argument - nomenclature - needs to be of length 1")}

  # get reference table
  tab <- country_reference_list

  # check that nomenclature name exists
  accepted_names <- colnames(tab)[2:43]
  if (!nomenclature %in% accepted_names){
    stop(paste0(nomenclature,
                " - is an invalid name. The following nomenclatures are accepted: ",
                paste(accepted_names, sep="", collapse = ", ")))
    }


  # GET LIST OF COUNTRIES ----

    # subset column from reference list
  countries <- tab[, nomenclature]

  # remove empty entries
  countries <- countries[countries != "" & !is.na(countries)]

  return(countries)
}





#' Output random country names
#'
#' This function returns the mode of vectors. That is to say, for any given vector of values, it returns the value that appears most frequently.
#' The function works with strings, numerical and mixed inputs. \code{NA} values are treated as distinct values.
#'
#' @param n Number of desired (pseudo)random country names.
#' @param replace Logical value indicating whether sampling should be with replacement.
#' @param seed Single numerical value to be used as seed.
#' @param nomenclature Nomenclature from which the list of countries should be taken. Not all countries are present in all nomenclature, for example Taiwan is not recognised by the UN, so it will not be returned with \code{"WTO_en"}. The function accept any of the nomenclatures of country_name. For a list of accepted values, refer to \href{https://fbellelli.github.io/countries/articles/dealing_with_names.html}{this page}. The default is \code{name_en}, which is the English list of names in the ISO standard 3166.
#' @returns A vector of n (pseudo)random country names.
#' @seealso \link[countries]{list_countries}, \link[countries]{country_name}
#' @export
#' @examples
#' random_countries(10)
#' random_countries(n = 500, replace = TRUE)
#' random_countries(n = 5, nomenclature = "ISO3", seed = 5)
random_countries <- function(n, replace = FALSE,  nomenclature = "name_en", seed = NULL) {

  # CHECK INPUTS ----

  # check inputs are as expected
  if (!is.logical(replace) | length(replace)!=1) stop("Function argument - replace - needs to be a logical statement (TRUE/FALSE)")
  if (length(n)!=1){stop("Argument - n - needs to be a single number")}
  if (!is.numeric(n)){stop("Argument - n - needs to a numeric value")}
  if (is.na(n)){stop("Argument - n - cannot be NA or NaN")}
  if (!is.null(seed)){
    if (!is.numeric(seed)){stop("Argument - seed - needs to a numeric value")}
    if (length(seed)>1){stop("Argument - seed - needs to be a single number or equal to NULL")}
    if (is.na(seed)){stop("Argument - seed - cannot be NA or NaN")}
  }

  # --> nomenclature is checked by list_countries()

  # get a full list of countries in the requested nomenclature
  countries <- list_countries(nomenclature)

  # check if n > countries. Issue warning/message based on replacing setting
  if (n > length(countries) & replace == FALSE){
    warning(paste0("Requested ",n,
                   " country names, but there are only " ,
                   length(countries),
                   " countries in the nomenclature. - replace - has been set to TRUE to allow repeating country names in the output"))
    replace <- TRUE
  }


  # SELECT n RANDOM COUNTRIES ----

  # sample countries
  if (!is.null(seed)) set.seed(seed)
  random_countries <- sample(countries, size = n, replace = replace)

  # output random countries
  return(random_countries)
}
