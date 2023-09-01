#' Internal function - Determine if "invalid multibyte string" error will be triggered due to wrong encoding.
#'
#' @param x String vector to be tested
#' @param return.elements Logical value. If `TRUE`, then the function returns a logical value for each element of x, otherwise a single logical is returned for the entire vector.
#' @noRd
#' @keywords Internal
has.invalid.multibyte.string  <- function(x, return.elements = FALSE)
{
  if (is.null(x))
    return(F)
  if (return.elements)
  {
    n <- length(x)
    out <- rep(F,n)
    for (i in 1:n)
      out[i] <- class(try(toupper(x[i]),silent = T))[1]=="try-error"
  }
  else
    out <- class(try(toupper(x),silent = T))[1]=="try-error"
  return(out)
}

