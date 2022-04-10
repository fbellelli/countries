#' internal function - Tests whether the value is convertible to a date format
#'
#' Internal function behind \code{is_date()}.
#' @param x A vector of length 1 to be tested
#' @param formats Formats to check for expressed in standard notation.
#' @return Returns a logical value indicating whether the value can be converted to any of the date formats provided. This function is the basis for \code{countries::is_date()}.
#' @seealso \link[countries]{is_date} \link[countries]{find_timecol}
#' @examples
#' is.date("2020-01-01")
#' is.yearcol("a")
is.date <- function(x, formats){any(!is.na(as.Date(as.character(x), format = formats)))}

#_____________________________________________________________
#' Test whether the input is a date
#'
#' This function checks if a value is a date by attempting to convert it to a date format. The user can specify which date formats should be tested with the argument \code{allowed_formats}.
#' @param x A vector of values to be tested
#' @param allowed_formats Date formats to be checked for (expressed in R date notation).
#' @return Returns a logical vector indicating whether the values can be converted to any of the date formats provided. Notice that unless specified, the default allowed formats do not include simple year numbers (e.g. 2022 or 1993) because number vectors could wrongly be identified as dates. Also, notice that testing \code{NA} values will return \code{FALSE}.
#' @export
#' @seealso \link[countries]{find_timecol} \link[countries]{is_countries}
#' @examples
#' is_date(c("2020-01-01","test",2020,"March 2030"))
is_date <- function(x, allowed_formats=c("%Y-%m-%d",
                                         "%y-%m-%d",
                                         "%m-%d-%Y",
                                         "%m-%d-%y",
                                         "%d-%m-%Y",
                                         "%d-%m-%y",
                                         "%Y/%m/%d",
                                         "%y/%m/%d",
                                         "%m/%d/%Y",
                                         "%m/%d/%y",
                                         "%d/%m/%Y",
                                         "%d/%m/%y",
                                         "%Y.%m.%d",
                                         "%y.%m.%d",
                                         "%m.%d.%Y",
                                         "%m.%d.%y",
                                         "%d.%m.%Y",
                                         "%d.%m.%y",
                                         "%d %b %Y",
                                         "%d %B %Y",
                                         "%b %d %Y",
                                         "%B %d %Y",
                                         "%b %d, %Y",
                                         "%B %d, %Y",
                                         "%d%b%Y",
                                         "%d%B%Y",
                                         "%Y%B%d",
                                         "%Y%b%d",
                                         "%b %Y",
                                         "%B %Y",
                                         "%b %y",
                                         "%B %y",
                                         "%m-%Y",
                                         "%Y-%m",
                                         "%m/%Y",
                                         "%Y/%m")){

  #check that - allowed formats - is not empty
  if (!is.character(allowed_formats)) stop("Value provided to argument - allowed_formats - is not valid. It needs to be a character vector.")
  if (is.null(allowed_formats)|all(is.na(allowed_formats))){stop("Value provided to argument - allowed_formats - is empty or NA")}
  allowed_formats <- na.omit(allowed_formats)

  #check that input is a vector
  if (length(x)==0) stop("The argument - x - is empty")
  if (is.data.frame(x)) stop("The input - x - needs to be a vector")

  #check all possible formats for a match
  return(sapply(x, is.date, formats=allowed_formats, USE.NAMES = FALSE))
}




