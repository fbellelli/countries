#' Test whether the input is a date
#'
#' This function checks if a value is a date by attempting to convert it to a date format. The user can specify which date formats should be tested with the argument \code{formats}.
#' @param x A vector of values to be tested
#' @param formats Date formats to be checked for (expressed in R date notation).
#' @returns Returns a logical vector indicating whether the values can be converted to any of the date formats provided. Notice that unless specified, the default allowed formats do not include simple year numbers (e.g. 2022 or 1993) because number vectors could wrongly be identified as dates. Also, notice that testing \code{NA} values will return \code{FALSE}.
#' @export
#' @seealso \link[countries]{find_timecol}, \link[countries]{find_keycol}, \link[countries]{is_country}
#' @examples
#' is_date(c("2020-01-01","test",2020,"March 2030"))
is_date <- function(x, formats=c("%Y-%m-%d",
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
  if (!is.character(formats)) stop("Value provided to argument - formats - is not valid. It needs to be a character vector.")
  if (is.null(formats)|all(is.na(formats))){stop("Value provided to argument - formats - is empty or NA")}
  formats <- na.omit(formats)

  #check that input is a vector
  if (length(x)==0) stop("The argument - x - is empty")
  if (is.data.frame(x)) stop("The input - x - needs to be a vector")

  # convert inputs to character
  x <- as.character(x)

  # check that length is not excessive for a date and keep track of NAs
  output <- !is.na(x) & (nchar(x) < 100)

  #check all possible date formats for a match
  if (any(output)){
    output[output] <- sapply(x[output], function(x, formats){any(!is.na(as.Date(x, format = formats)))}, formats=formats, USE.NAMES = FALSE)
  }

  return(output)
}




