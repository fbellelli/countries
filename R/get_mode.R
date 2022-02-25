#' Get the mode of a vector
#'
#' This function returns the mode of a distribution. For any given vector of values, it returns the value that appears most frequently.
#' The function works with strings, numerical and mixed inputs. \code{NA} values are treated as distinct value.
#' In case there are two or more values that appear most frequently in the vector, only the first is returned.
#'
#' @param x A vector
#' @return Returns the mode of the vector \code{x}
#' @export
#' @examples
#' get_mode(c("a","a",2,3))
#' get_mode(c(1,1,2,3,NA))
get_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
