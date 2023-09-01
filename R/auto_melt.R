#' @title Automatic pivoting of country and year columns to a long format
#'
#' @description
#' When at least 3 country names or years are found in the column names, the function will automatically transform the table from a wide to a long format by pivoting the country/year columns.
#' This is equivalent to applying \code{tidyr::pivot_longer()} or \code{data.table::melt()} on the columns with years or countries as names.
#' The function is able to detect years also when they are preceded by a prefix.
#'
#' @param x A data.frame object to check and pivot country or year columns.
#' @param names_to String indicating how the column holding the name of the pivoted columns should be called in the output table. Default is \code{"pivoted_colnames"}
#' @param values_to String indicating how the column containing the values of the pivoted columns should be called in the output table. Default is \code{"pivoted_data"}
#' @param verbose Logical value. If set to \code{TRUE} (the default), a message will be displayed on the console indicating which columns are being pivoted. If set to \code{FALSE}, the messages are turned off.
#' @param pivoting_info Logical value indicating whether to return the list of names of the column that have been pivoted. Default is \code{FALSE}. If set to \code{TRUE}, the output will be a list instead of simple data.frame. Teh list will contain 1) the pivoted table, 2) the list of pivoted columns.
#' @returns A table transformed into a "long" format by pivoting country or year columns. If year columns are found, a numeric column called \code{"year_pivoted_colnames"} is added isolating the years extracted from the table header's.
#' @seealso \link[countries]{auto_merge}, \link[countries]{find_countrycol},\link[countries]{find_timecol}
#' @export
#' @examples
#' # example data
#' example <- data.frame(Date = c("01.01.2019", "01.02.2019", "01.03.2019"),
#'                       Japan = 1:3,
#'                       Norway = 2:4,
#'                       Germany = 3:5,
#'                       US = 4:6)
#' example2 <- data.frame(Sector = c("Agriculture", "Mining", "Forestry"),
#'                        X2000 = 1:3,
#'                        X2001 = 2:4,
#'                        X2002 = 3:5,
#'                        X2003 = 4:6)
#'
#' # examples pivotting countries and years from column names
#' auto_melt(example)
#' auto_melt(example2)
auto_melt <- function(
    x,
    names_to = "pivoted_colnames",
    values_to = "pivoted_data",
    verbose = TRUE,
    pivoting_info = FALSE
){

  # check inputs
  if(!is.data.frame(as.data.frame(x))|is.null(x)){stop("Argument - x - needs to be a dataframe")}
  if (is.na(names_to)|is.null(names_to)) stop("The argument - names_to - cannot be NA or NULL. It needs to be a string.")
  if (is.na(values_to)|is.null(values_to)) stop("The argument - values_to - cannot be NA or NULL. It needs to be a logical value")
  if (is.na(verbose)|is.null(verbose)) stop("The argument - verbose - cannot be NA or NULL. It needs to be a logical value")
  if (is.na(pivoting_info)|is.null(pivoting_info)) stop("The argument - pivoting_info - cannot be NA or NULL. It needs to be a string.")
  if (!is.character(names_to)|length(names_to)!=1)stop("The argument - names_to - is invalid. It needs to be a string.")
  if (!is.character(values_to)|length(values_to)!=1)stop("The argument - values_to - is invalid. It needs to be a string.")
  if (!is.logical(verbose) | length(verbose)!=1) stop("Function argument - verbose - needs to be a logical statement (TRUE/FALSE)")
  if (!is.logical(pivoting_info) | length(pivoting_info)!=1) stop("Function argument - pivoting_info - needs to be a logical statement (TRUE/FALSE)")

  # convert input to data.frame
  data <- as.data.frame(x)

  #check column names for countries/years
  temp <- check_wide_format(data)

  # proceed if there is any column to pivot
  if (!is.null(temp)){

    # pivot table if countries or years were found and adjust name
    data <- as.data.frame(tidyr::pivot_longer(data, dplyr::all_of(temp$col_name), names_to = names_to, values_to = values_to))

    # move pivoted keys to front of table
    data <- data[, c(names_to, colnames(data)[colnames(data) != names_to])]

    # convert year to numeric if possible, otherwise add numeric column with detected year in string
    if (colnames(temp)[1] == "year"){
      if (all(grepl('^(?=.)([+-]?([0-9]*)(\\.([0-9]+))?)$', temp$col_name, perl = TRUE))){
        data[, names_to] <- as.numeric(data[, names_to])
      } else {
        data[,"year_pivoted_colnames"] <- temp[fastmatch::fmatch(data[, names_to], temp$col_name), 1]
      }
    }

    # save name of pivoted columns
    pivoted_cols <- as.vector(temp$col_name)

    # issue message to console
    if (verbose) cat(paste0(colnames(temp)[1], " detected in column names, pivoting columns: ", paste(temp$col_name[1:3], collapse=", ", sep = ""), if (nrow(temp)>3){paste0(", ..., ", temp$col_name[nrow(temp)])}, "\n"))
  } else {

    pivoted_cols <- NULL

    if (verbose) cat("No column was pivoted\n")
  }

  # final output
  if (pivoting_info){
    return(list(output = data,
           pivoted_cols = pivoted_cols))
  } else {
    return(data)
  }

}
