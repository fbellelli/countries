#' Internal function - Tests whether a vector of data could be containing year information
#'
#' @param x A vector to be tested
#' @param limit A minimum and maximum year value \code{c(1800,2200)}
#' @param regularity Require that no gap is present in the year values (e.g. 2005, 2000, 2010, 2020 would return FALSE because 2015 is missing). Default is \code{FALSE} (regularity is not required).
#' @param allow_NA If set to true the date vector is allowed to contain \code{NA} values. If set to \code{FALSE} the function returns a FALSE if any value is NA. Default is \code{TRUE}.
#' @returns Returns a logical vector indicating whether the vector could be containing year information
#' @seealso \link[countries]{is_country}, \link[countries]{find_countrycol}
#' @noRd
#' @keywords Internal
#' @examples
#' countries:::is.yearcol(1990:2020)
#' countries:::is.yearcol(c("a",2000))
is.yearcol <- function(x,
                    limit=c(1800,2200),
                    regularity=FALSE,
                    allow_NA=TRUE){
  #check for NULL
  if (is.null(x)|length(x)==0) stop("Function argument - x - is NULL")

  #check length 2 of limit and return error
  if (!is.vector(limit) & length(limit)!=2 & !is.numeric(limit)) stop("Function argument - limit - needs to be a number vector of length 2")

  #reduce number of elements to check
  x <- unique(x)

  #convert factor to string
  if (is.factor(x)) x <- as.character(x)

  #initiate checks
  #column contains no NA
  if (all(!is.na(x))|allow_NA==TRUE){
    x <- na.omit(x)

    #checks that all values are numeric or convertible to numeric
    if(all(!is.na(suppressWarnings(as.numeric(x))))){

      x <- as.numeric(x)

      #checks vector contains all integer values
      if(all(x %% 1 == 0)){

        #checks that all values are the range specified by limit
        if(all(x >= limit[1]) & all(x <= limit[2])){

          #require time regularity
          if(regularity){length(unique(diff(sort(x)))) == 1}else{TRUE}

        }else{FALSE}
      }else{FALSE}
    }else {FALSE}
  }else{FALSE}
}


