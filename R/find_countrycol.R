#' Finds columns containing country names
#'
#' This function takes a data frame as argument and returns the column name (or index) of all columns containing country names.
#' It can be used to automate the search of country columns in data frames.
#' For the purpose of this function, a country is any of the 249 territories designated in the ISO standard \code{3166}.
#' On large datasets (More than 100000 rows) a random sample is used for evaluating the columns.
#' @param x A data frame object
#' @param return_index A logical value indicating whether the function should return the index of country columns instead of the column names. Default is \code{FALSE}, column names are returned.
#' @param allow_NA Logical value indicating whether columns containing \code{NA} values are to be considered as country columns. Default is \code{allow_NA=FALSE}, the function will not return country column containing \code{NA} values.
#' @param min_share A value between \code{0} and \code{1} indicating the minimum share of country names in columns that are returned. A value of \code{0} will return any column containing a country name. A value of \code{1} will return only column whose entries are all country names. Default is \code{0.9}, i.e. at least 90 percent of the column entries need to be country names.
#' @return Returns a vector of country names (\code{return_index=FALSE}) or column indices (\code{return_index=TRUE}) of columns containing country names.
#' @seealso \link[countries]{is_country} \link[countries]{country_name} \link[countries]{find_timecol}
#' @export
#' @examples
#' find_countrycol(x=data.frame(a=c("Brésil","Tonga","FRA"), b=c(1,2,3)))
find_countrycol <- function(x, return_index=FALSE, allow_NA=FALSE, min_share=0.8){

  #check inputs
  if(!is.data.frame(as.data.frame(x))|is.null(x)){stop("Argument - x - needs to be a dataframe")}
  if(!is.logical(return_index)|is.null(return_index)|length(return_index)>1|is.na(return_index)){stop("Argument - return_index - needs to be a logical value (TRUE/FALSE)")}
  if(!is.logical(allow_NA)|is.null(allow_NA)|length(allow_NA)>1|is.na(allow_NA)){stop("Argument - allow_NA - needs to be a logical value (TRUE/FALSE)")}
  if(!is.numeric(min_share)|is.null(min_share)|length(min_share)>1|is.na(min_share)|min_share>1|min_share<0){stop("Argument - min_share - needs to be a numeric value between 0 and 1 indicating the requested minimum share of country names in the output columns")}

  #take sample if table is large
  if (nrow(x)>100000){
    #temp <- na.omit(x)
    #if (nrow(temp)>100000){x <- temp}   #ensures that at least 500 rows are kept
    x <- x[sample(1:nrow(x), size= 100000, replace = FALSE), ]
  }

  #identify columns containing NA values and eliminate them if requested
  cols <- colnames(x)
  if(allow_NA == FALSE){
    cols <- cols[!apply(is.na(x), MARGIN=2,FUN=any, simplify=TRUE)]
    x <- x[,cols]
  }

  #initiate empty vector of country indices
  country_cols <- NULL

  #proceed only if there is at least a column to check
  if (length(cols)>0){


    #transform any factor column to character
    x[sapply(x, is.factor)] <- lapply(x[sapply(x, is.factor)], as.character)

    #loop over all columns
    for (i in 1:ncol(x)){
      #check if it is a character vector
      if(is.character(x[,i])){
        #if there are too many country names, assume it cannot be a country column
        #if (length(unique(x[,i]))<=300/min_share){
        #test if it contains country names
        if(sum(is_country(x[,i]), na.rm=TRUE) >= min_share * length(x[!is.na(x[,i]),i])) country_cols <- c(country_cols, i)
        #}
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
