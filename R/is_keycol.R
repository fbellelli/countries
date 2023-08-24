#' Test whether a set of column could be a data frame key
#'
#' This function takes a data frame and a vector of column names as argument and returns a logical value indicating whether the indicated columns uniquely identify entries in the data frame.
#' If the output is \code{TRUE}, the indicated columns could be the keys of the table.
#' @param x A data frame object
#' @param cols A vector of column names or indices to be tested.
#' @param allow_NA Logical value indicating whether to allow key columns to have \code{NA} values. Default is \code{allow_NA=FALSE}, the function will return \code{FALSE} if any \code{NA} value is present in \code{colnames}.
#' @param verbose Logical value indicating whether messages should be printed on the console. Default is \code{TRUE}.
#' @returns Returns a logical value. If \code{TRUE}, the columns indicated in \code{colnames} uniquely identify the entries in \code{x}.
#' @seealso \link[countries]{find_keycol}, \link[countries]{find_countrycol}, \link[countries]{find_timecol}
#' @export
#' @examples
#' is_keycol(data.frame(a=1:10,b=sample(c("a","b","c"),10, replace=TRUE)), cols="a")
#' is_keycol(data.frame(a=1:10,b=sample(c("a","b","c"),10, replace=TRUE)), cols="b")
#' is_keycol(
#' data.frame(a=c(1:5,1:5),
#' b=sample(c("a","b","c"),10, replace=TRUE),
#' c=c(rep("a",5),rep("b",5))),
#' cols=c("a","c"))
is_keycol <- function(x,
                   cols,
                   allow_NA=FALSE,
                   verbose=TRUE){

  #------- CHECK INPUT VALIDITY --------
  if(!is.logical(allow_NA)|is.null(allow_NA)|length(allow_NA)>1|is.na(allow_NA)) stop("The argument - allow_NA - is either missing or invalid. It needs to be a TRUE/FALSE value.")
  if(!is.logical(verbose)|is.null(verbose)|length(verbose)>1|is.na(verbose)) stop("The argument - verbose - is either missing or invalid. It needs to be a TRUE/FALSE value.")
  if(!is.data.frame(x)) stop("The argument - x - needs to be a data.frame")
  if(!is.character(cols)) stop("The argument - cols - needs to be a character vector containing column names")
  if(!all(cols %in% colnames(x))) stop(paste0("The following column names cannot be found in - x - make sure the spelling is correct: ",paste(cols[!cols %in% colnames(x)], collapse=", ")))

  #------- INITIATE VARS AND PREP INPUTS---------------
  output <- TRUE
  if (is.numeric(cols) | any(is.na(cols))){
    if(any(is.na(cols)) | any(cols<0) | any(cols>ncol(x))){stop("The argument - cols - need to be a valud column name or index")}
    cols <- colnames(x)[cols]
  }

  #------- CHECK FOR NA ----------------
  if (any(is.na(x[,cols]))){
    if (allow_NA == FALSE){
      output <- FALSE
    }
    if (verbose == TRUE){
      message(paste0("\nFound NA values in: ",paste(cols[apply(is.na(x[,cols]), MARGIN=2,FUN=any, simplify=TRUE)], collapse=", ")))
    }
  }

  #------- TEST UNIQUENESS -------------

  #continue if output is TRUE
  if (output == TRUE) {
    if (length(cols)>1){
      #paste together columns and test uniqueness
      keys <- apply(x[,cols],1, paste,collapse="-",simplify=TRUE)
      output <- !any(duplicated(keys))
    } else {
      output <- !any(duplicated(x[,cols]))
    }
  }


  #------- OUTPUT RESULTS --------------
  return(output)
}
