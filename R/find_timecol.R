#' Finds columns containing date and year data
#'
#' This function takes a data frame as argument and returns the column names (or indices) of all columns containing dates and the most likely column containing year information, if any.
#' It can be used to automate the search of date and year columns in data frames.
#' @param x A data frame object
#' @param return_index A logical value indicating whether the function should return the index of time columns instead of the column names. Default is \code{FALSE}, column names are returned.
#' @param allow_NA Logical value indicating whether to allow time columns to contain \code{NA} values. Default is \code{allow_NA=FALSE}, the function will not return time column containing \code{NA} values.
#' @param sample_size Either \code{NA} or a numeric value indicating the sample size used for evaluating columns. Default is \code{1000}. If \code{NA} is passed, the function will evaluate the full table. The minimum accepted value is \code{100} (i.e. 100 randomly sampled rows are used to evaluate the columns). This parameter can be tuned to speed up computation on long datasets. Taking a sample could result in inexact identification of key columns, accuracy improves with larger samples.
#' @returns Returns a vector of names (\code{return_index=FALSE}) or indices (\code{return_index=TRUE}) of columns containing date or year information. Only the most likely year column is returned.
#' @seealso \link[countries]{is_date}, \link[countries]{find_countrycol}
#' @export
#' @examples
#' find_timecol(x=data.frame(a=1970:2020, year=1970:2020, b=rep("2020-01-01",51),c=sample(1:1000,51)))
find_timecol <- function(x,
                         return_index = FALSE,
                         allow_NA = TRUE,
                         sample_size = 1000){

  #----------- CHECK INPUT VALIDITY ---------------

  if(!is.logical(return_index)|is.null(return_index)|length(return_index)>1|is.na(return_index)) stop("The argument - return_index - is either missing or invalid. It needs to be a TRUE/FALSE value.")
  if(!is.logical(allow_NA)|is.null(allow_NA)|length(allow_NA)>1|is.na(allow_NA)) stop("The argument - allow_NA - is either missing or invalid. It needs to be a TRUE/FALSE value.")
  if(!is.data.frame(x)) stop("The argument - x - needs to be a data.frame")
  if(is.null(x)) stop("The argument - x - is missing or NULL")
  if(!is.numeric(sample_size)&!is.na(sample_size)|length(sample_size)>1){stop("Argument - sample_size - needs to be a numeric value greater or equal to 100")}
  if(is.numeric(sample_size)&round(sample_size)<100){stop("Argument - sample_size - needs to be a numeric value greater or equal to 100")}
  sample_size <- round(sample_size)

  #----------- INITIATE SEARCH -------------------


  if (ncol(x)>0){

    #take sample if table is large and requested
    if (!is.na(sample_size)){
      temp <- colnames(x)
      x <- as.data.frame(x[sample(1:nrow(x), min(round(sample_size), nrow(x))),])
      colnames(x) <- temp
    }

    #initiate empty variable to store column indices
    numeric_cols <- NULL
    date_cols <- NULL
    time_cols <- NULL

    #define interesting column names
    regex <- "([yY][Ee][aA][Rr][Ss]?)|([dD][aA][tT][eE][Ss]?)|(^[tT]$)|(^[tT][iI][Mm][eE]$)|(^[Yy][rR]$)"

    #define allowed date formats
    formats <- c("%Y-%m-%d",
                 "%y-%m-%d",
                 "%m-%d-%Y",
                 "%m-%d-%y",
                 "%d-%m-%Y",
                 "%d-%m-%y",
                 "%Y/%m/%d",
                 "%y/%m/%d",
                 "%m/%d/%Y",
                 "%m/%d/%y",
                 "%d/%m/%Y",
                 "%d/%m/%y",
                 "%Y.%m.%d",
                 "%y.%m.%d",
                 "%m.%d.%Y",
                 "%m.%d.%y",
                 "%d.%m.%Y",
                 "%d.%m.%y",
                 "%d %b %Y",
                 "%d %B %Y",
                 "%b %d %Y",
                 "%B %d %Y",
                 "%b %d, %Y",
                 "%B %d, %Y",
                 "%d%b%Y",
                 "%d%B%Y",
                 "%Y%B%d",
                 "%Y%b%d",
                 "%b %Y",
                 "%B %Y",
                 "%b %y",
                 "%B %y",
                 "%m-%Y",
                 "%Y-%m",
                 "%m/%Y",
                 "%Y/%m")

    #---------- LOOK FOR TIME COLUMNS -------------
    #loop over all columns
    for (i in 1:ncol(x)){

      #change factors to string
      if (is.factor(x[,i])){
        x[,i] <- as.character(x[,i])
      }

      #if it is numeric column search for year
      if (is.numeric(x[,i])){
        #test if it could be a year column
        if(is.yearcol(x[,i], regularity=FALSE, allow_NA=allow_NA)) time_cols <- c(time_cols, i)
      } else {
        # otherwise check for dates formats
        #test if column contains dates
        if(sum(is_date(x[,i], formats)) >= 0.90 * nrow(x)){
          #check for NA (if requested or more than one column)
          if (!any(is.na(x[,i]))|allow_NA == TRUE) {
            #add to list of candidates
            date_cols <- c(date_cols, i)
          }
        }
      }


    }


    #---------- CHECK YEAR COLUMNS ---------------


    #score the year column candidates (if more than one candidate):
    if (length(time_cols)>1){
      score <- rep(0, length(time_cols))

      #column name
      score <- score + 1.5*grepl(pattern = regex, x=colnames(x[time_cols]), perl=TRUE)

      #no NA (only check if vector with NA have not been excluded)
      if (allow_NA == TRUE){
        score <- score + sapply(as.data.frame(!is.na(x[,time_cols])), all)
      }

      #continuity
      x[,time_cols] <- sapply(x[,time_cols], as.numeric)
      temp <- sapply(x[,time_cols], function(x) diff(sort(unique(x))))
      score <- score + sapply(temp, function(x) all(x==1))

      #regularity
      score <- score + sapply(temp, function(x) length(unique(x))==1)

      #range
      score <- score + sapply(x[,time_cols], function(x) sum(x %in% 1970:2030)/length(x))

      #leftmost column among the candidates in table and not beyond column 5
      score <- score + as.numeric(time_cols == min(time_cols) & min(time_cols) <= 5)

      # select the first column with the highest score
      time_cols <- time_cols[which.max(score)]
    }


    #------------ OUTPUT ----------------

    #combine and reorder
    time_cols <- sort(c(date_cols, time_cols))

    #convert time_cols index to names if requested otherwise return index
    if (return_index){
      return(time_cols)
    } else {
      if (length(time_cols)==0) return(NULL) else return(colnames(x)[time_cols])
    }
  }
}
