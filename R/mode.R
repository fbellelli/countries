#' Statistical mode of a vector
#'
#' This function returns the mode of vectors. That is to say, for any given vector of values, it returns the value that appears most frequently.
#' The function works with strings, numerical and mixed inputs. \code{NA} values are treated as distinct values.
#'
#' @param x A vector
#' @param na.rm Logical value indicating whether \code{NA} values should be omitted. Default is \code{FALSE}.
#' @param first_only Logical value indicating whether only the first mode should be returned if \code{x} has multiple modes (i.e. there are multiple values with the highest number of observations). Default is FALSE.
#' @returns Returns the mode of the vector \code{x}
#' @export
#' @examples
#' countries::Mode(c("a","a",2,3))
#' countries::Mode(c(1,1,2,3,NA,2))
#' countries::Mode(c(NA,NA,NA,1,1,2))
Mode <- function(x,
                 na.rm = FALSE,
                 first_only = FALSE) {

  # CHECK INPUTS ----

  if (is.null(x)){return(NULL)}
  if (!is.atomic(x)){stop("Argument - x - needs to be a vector")}
  if (!is.logical(na.rm)|any(is.na(na.rm))|is.null(na.rm)){stop("Argument - na.rm - needs to be a logical value: TRUE / FALSE")}
  if (length(na.rm)>1){stop("Argument - na.rm - needs to be of length 1")}
  if (!is.logical(first_only)|any(is.na(first_only))|is.null(first_only)){stop("Argument - first_only - needs to be a logical value: TRUE / FALSE")}
  if (length(first_only)>1){stop("Argument - first_only - needs to be of length 1")}


  # FIND MODE -----

  ux <- unique(x)
  if (na.rm == TRUE){
    ux <- na.omit(ux)
  }
  if (first_only==TRUE){
    return(ux[which.max(tabulate(fastmatch::fmatch(x, ux)))])
  } else {
    tab <- tabulate(match(x, ux))
    return(ux[tab == max(tab)])
  }
}
