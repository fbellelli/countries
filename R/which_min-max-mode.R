#' Return location of minimum, maximum and mode values' index
#'
#' These function return the position (index) of all the minimum, maximum, and mode values of the vector \code{x}. \code{which_min()} and \code{which_max()} only support numeric and logical vectors.
#' These functions are identical to \code{which.min()} and \code{which.max()}, except that ALL minima/maxima are returned instead of only the first one.
#'
#' @param x A numeric or vector
#' @param first_only Logical value indicating whether only the first value should be returned (i.e. if \code{TRUE} the function behaves like \code{which.min()} and \code{which.max()}). Default is FALSE.
#' @returns Returns the position of the minimum, maximum and mode values of a vector \code{x}
#' @seealso \link[countries]{Mode}, \link[base]{which.min}, \link[base]{which.max}
#' @export
#' @examples
#' which_mode(c("a","a",2,3))
#' which_min(c(1,1,2,3,NA,2))
#' which_max(c(NA,NA,NA,1,1,2))
which_min <- function(x, first_only=FALSE){

  #CHECK INPUTS ----
  if (is.null(x)) return(NULL)
  if (!is.atomic(x)|is.character(x)|is.factor(x)){stop("Argument - x - needs to be a numeric or logical vector")}
  if (!is.logical(first_only)|any(is.na(first_only))|is.null(first_only)){stop("Argument - first_only - needs to be a logical value: TRUE / FALSE")}
  if (length(first_only)>1){stop("Argument - first_only - needs to be of length 1")}

  #RETURN POSITON ----
  if (is.null(x)){return(NULL)} else {
    if (first_only == TRUE){
      return(which.min(x))
    } else {
      minimum <- suppressWarnings(min(x, na.rm=TRUE))
      return(as.vector(na.omit(c(1:length(x))[x==minimum])))
    }
  }
}

#' @rdname which_min
#' @export
which_max <- function(x, first_only=FALSE){

  #CHECK INPUTS ----
  if (is.null(x)) return(NULL)
  if (!is.atomic(x)|is.character(x)|is.factor(x)){stop("Argument - x - needs to be a numeric or logical vector")}
  if (!is.logical(first_only)|any(is.na(first_only))|is.null(first_only)){stop("Argument - first_only - needs to be a logical value: TRUE / FALSE")}
  if (length(first_only)>1){stop("Argument - first_only - needs to be of length 1")}

  #RETURN POSITON ----
  if (is.null(x)){return(NULL)} else {
    if (first_only == TRUE){
      return(which.max(x))
    } else {
      maximum <- suppressWarnings(max(x, na.rm=TRUE))
      return(as.vector(na.omit(c(1:length(x))[x==maximum])))
    }
  }
}

#' @rdname which_min
#' @export
#' @importFrom fastmatch %fin%
which_mode <- function(x, first_only=FALSE){

  #CHECK INPUTS ----
  #perfomed by countries::Mode()

  #RETURN POSITON ----

  if (is.null(x)){return(NULL)} else {

    xmode <- countries::Mode(x, first_only = FALSE)
    position <- c(1:length(x))[x %fin% xmode]

    if (first_only == TRUE){
      return(position[1])
    } else {
      return(position)
    }
  }
}
