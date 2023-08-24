#' Finds columns containing country names
#'
#' This function takes a data frame as argument and returns the column name (or index) of all columns containing country names.
#' It can be used to automate the search of country columns in data frames.
#' For the purpose of this function, a country is any of the 249 territories designated in the ISO standard \code{3166}.
#' On large datasets a random sample is used for evaluating the columns.
#' @param x A data frame object
#' @param return_index A logical value indicating whether the function should return the index of country columns instead of the column names. Default is \code{FALSE}, column names are returned.
#' @param allow_NA Logical value indicating whether columns containing \code{NA} values are to be considered as country columns. Default is \code{allow_NA=FALSE}, the function will not return country column containing \code{NA} values.
#' @param min_share A value between \code{0} and \code{1} indicating the minimum share of country names in columns that are returned. A value of \code{0} will return any column containing a country name. A value of \code{1} will return only columns whose entries are all country names. Default is \code{0.9}, i.e. at least 90 percent of the column entries need to be country names.
#' @param sample_size Either \code{NA} or a numeric value indicating the sample size used for evaluating columns. Default is \code{1000}. If \code{NA} is passed, the function will evaluate the full table. The minimum accepted value is \code{100} (i.e. 100 randomly sampled rows are used to evaluate the columns). This parameter can be tuned to speed up computation on long datasets. Taking a sample could result in inexact identification of key columns, accuracy improves with larger samples.
#' @returns Returns a vector of country names (\code{return_index=FALSE}) or column indices (\code{return_index=TRUE}) of columns containing country names.
#' @seealso \link[countries]{is_country}, \link[countries]{country_name}, \link[countries]{find_keycol}, \link[countries]{find_timecol}
#' @export
#' @examples
#' find_countrycol(x=data.frame(a=c("Brésil","Tonga","FRA"), b=c(1,2,3)))
find_countrycol <- function(x, return_index=FALSE, allow_NA=TRUE, min_share=0.8, sample_size=1000){

  #check inputs
  if(!is.data.frame(as.data.frame(x))|is.null(x)){stop("Argument - x - needs to be a dataframe")}
  if(!is.logical(return_index)|is.null(return_index)|length(return_index)>1|is.na(return_index)){stop("Argument - return_index - needs to be a logical value (TRUE/FALSE)")}
  if(!is.logical(allow_NA)|is.null(allow_NA)|length(allow_NA)>1|is.na(allow_NA)){stop("Argument - allow_NA - needs to be a logical value (TRUE/FALSE)")}
  if(!is.numeric(min_share)|is.null(min_share)|length(min_share)>1|is.na(min_share)|min_share>1|min_share<0){stop("Argument - min_share - needs to be a numeric value between 0 and 1 indicating the requested minimum share of country names in the output columns")}
  if(!is.numeric(sample_size)&!is.na(sample_size)|length(sample_size)>1){stop("Argument - sample_size - needs to be a numeric value greater or equal to 100")}
  if(is.numeric(sample_size)&round(sample_size)<100){stop("Argument - sample_size - needs to be a numeric value greater or equal to 100")}
  sample_size <- round(sample_size)

  #initiate empty vector of country indices
  country_cols <- NULL

  # extract all column names
  x <- as.data.frame(x)
  cols <- colnames(x)

  #proceed only if there is at least one column to check
  if (length(cols)>0){

    #take sample if table is large and sample is requested
    if (!is.na(sample_size)){
      x <- as.data.frame(x[sample(1:nrow(x), min(round(sample_size), nrow(x))),])
      colnames(x)<-cols
    }

    #adjust min_share value
    if (min_share==0){min_share <- 1/nrow(x)}

    #loop over all columns
    for (i in cols){
      #check for NAs
      if(allow_NA == FALSE | !any(is.na(x[,i]))){
        #transform factor columns into character
        if(is.factor(x[,i])){ x[,i] <- as.character(x[,i])}
        #proceed only on character vectors
        if(is.character(x[,i])){
          #test if column contains country names
          if(sum(suppressWarnings(is_country(x[,i], fuzzy_match = FALSE)), na.rm=TRUE) >= min_share * length(x[!is.na(x[,i]),i])) country_cols <- c(country_cols, i)
        }
      }
    }
  }

  #convert country_cols index to names if requested otherwise return index
  if (return_index){
    if (length(country_cols)==0) return(NULL) else return(c(1:ncol(x))[colnames(x) %in% country_cols])

  } else {
    return(country_cols)
  }

}
