#' Finds columns containing country names
#'
#' This function takes a data frame as argument and returns the column name (or index) of all columns containing country names.
#' For the purpose of this function, a country is any of the 249 territories designated in the ISO standard \code{3166}.
#' @param x A data frame object
#' @param return_index A logical value indicating whether the function should return the index of country columns instead of the column names. Default is FALSE.
#' @return Returns a vector of country names (\code{return_index=FALSE}) or column indices (\code{return_index=TRUE}) of columns containing country names.
#' @seealso \link[countries]{is_country} \link[countries]{country_name}
#' @export
#' @examples
#' find_countrycol(x=data.frame(a=c("Brésil","Tonga","FRA"), b=c(1,2,3)))

find_countrycol <- function(x, return_index=FALSE){

  #check inputs
  if(!is.data.frame(as.data.frame(x))|is.null(x)){stop("Argument - x - needs to be a dataframe")}
  if(!is.logical(return_index)|is.null(return_index)){stop("Argument - return_index - needs to be a a logical value (TRUE/FALSE)")}

  #take sample if table is large
  if (nrow(x)>500){
    x <- na.omit(x)
    x <- x[sample(1:nrow(x), size= min(500 + sqrt(nrow(x)), nrow(x)) ,replace = FALSE), ]
  }

  #initiate empty vector of country indices
  country_cols <- NULL

  #loop over all columns
  if (length(country_cols)==0){
    for (i in 1:ncol(x)){
      #check if it is a character vector
      if(is.character(x[,i])){
        #test if it contains country names
        if(sum(is_country(x[,i])) >= 0.95 * nrow(x)) country_cols <- c(country_cols, i)
      }
    }
  }

  #convert country_cols index to names if requested otherwise return index
  if (return_index){
    return(country_cols)
  } else {
    if (length(country_cols)==0) return(NULL) else return(colnames(x)[country_cols])
  }

}
