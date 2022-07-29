#' Get the mode of a vector
#'
#' This function returns the mode of a distribution. For any given vector of values, it returns the value that appears most frequently.
#' The function works with strings, numerical and mixed inputs. \code{NA} values are treated as a distinct value.
#'
#' @param x A vector
#' @param first_only Logical value indicating whether only the first mode should be returned in \code{x} has multiple modes (i.e. there are multiple values with the highest number of observations). Default is FALSE.
#' @return Returns the mode of the vector \code{x}
#' @export
#' @examples
#' countries::mode(c("a","a",2,3))
#' countries::mode(c(1,1,2,3,NA,2))
mode <- function(x,
                     first_only = FALSE) {
  ux <- unique(x)
  if (first_only==TRUE){
    return(ux[which.max(tabulate(match(x, ux)))])
  } else {
    tab <- tabulate(match(x, ux))
    return(ux[tab == max(tab)])
  }
}
