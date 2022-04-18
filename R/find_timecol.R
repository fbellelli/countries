#' Finds columns containing date and year data
#'
#' This function takes a data frame as argument and returns the column names (or indices) of all columns containing dates and the most likely column containing year information, if any.
#' It can be used to automate the search of date and year columns in data frames.
#' @param x A data frame object
#' @param return_index A logical value indicating whether the function should return the index of time columns instead of the column names. Default is \code{FALSE}, column names are returned.
#' @param allow_NA Logical value indicating whether to allow time columns to contain \code{NA} values. Default is \code{allow_NA=FALSE}, the function will not return time column containing \code{NA} values.
#' @param regularity Logical value indicating whether gaps are allowed in the unique time values of the column. For example, if \code{regularity=TRUE}, a column containing the values \code{c(2000, 2005, 2010, 2020)} would be ignored because \code{2015} is missing. Default is \code{FALSE}, regularity is not required.
#' @return Returns a vector of names (\code{return_index=FALSE}) or indices (\code{return_index=TRUE}) of columns containing date or year information. Only the most likely year column is returned.
#' @seealso \link[countries]{is_date} \link[countries]{find_countrycol}
#' @export
#' @examples
#' find_timecol(x=data.frame(a=1970:2020, year=1970:2020, b=rep("2020-01-01",51),c=sample(1:1000,51)))
find_timecol <- function(x,
                         return_index = FALSE,
                         allow_NA = FALSE,
                         regularity = FALSE){

  #----------- CHECK INPUT VALIDITY ---------------

  if(!is.logical(return_index)|is.null(return_index)|length(return_index)>1|is.na(return_index)) stop("The argument - return_index - is either missing or invalid. It needs to be a TRUE/FALSE value.")
  if(!is.logical(allow_NA)|is.null(allow_NA)|length(allow_NA)>1|is.na(allow_NA)) stop("The argument - allow_NA - is either missing or invalid. It needs to be a TRUE/FALSE value.")
  if(!is.logical(regularity)|is.null(regularity)|length(regularity)>1|is.na(regularity)) stop("The argument - regularity - is either missing or invalid. It needs to be a TRUE/FALSE value.")
  if(!is.data.frame(x)) stop("The argument - x - needs to be a data.frame")
  if(is.null(x)) stop("The argument - x - is missing or NULL")

  #----------- INITIATE SEARCH -------------------

  #change factors to string
  fctr.cols <- sapply(x, is.factor)
  x[, fctr.cols] <- sapply(x[, fctr.cols], as.character)

  #take sample if table is large
  if (nrow(x)>5000){
    x <- x[sample(1:nrow(x), size= min(5000 + sqrt(nrow(x)), nrow(x)) ,replace = FALSE), ]
  }

  #initiate empty variable to store column indices
  date_cols <- NULL
  time_cols <- NULL

  #---------- CHECK DATE COLUMNS -------------
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

  #loop over all columns
  for (i in 1:ncol(x)){
    #test if it contains country names
    if(sum(is_date(x[,i])) >= 0.95 * nrow(x)){
      #check for NA (if requested or more than one column)
      if (!any(is.na(x[,i]))|allow_NA == TRUE) {
        #check for regularity (if requested or more than one column)
        regular <- FALSE
        if (regularity == TRUE){
          temp <- na.omit(diff(as.Date(x[,i], tryFormats = formats)))
          if(all(temp<28)){regular <- length(unique(temp))==1}
          else if(all(temp>=28 & temp<=31)){regular <- TRUE}
          else if(all(temp>31 & temp<364)){regular <- length(unique(round(temp/30)))==1}
          else {regular <- length(unique(round(temp/365)))==1}
        }
        if ((regularity == TRUE & regular == TRUE) | regularity == FALSE){
          #add to list of candidates
          date_cols <- c(date_cols, i)
        }
      }
    }
  }

  #scoring candidates if multiple?
  #-> not needed because we already know these are date columns. There might be more than one

  #---------- CHECK YEAR COLUMNS ---------------

  regex <- "([yY][Ee][aA][Rr][Ss]?)|([dD][aA][tT][eE][Ss]?)|(^[tT]$)|(^[tT][iI][Mm][eE]$)|(^[Yy][rR]$)"

  #if nothing is found, or there are columns with interesting names, check for year columns by looping over all columns
  if (length(date_cols)==0 | any(grepl(pattern = regex, x=colnames(x)[! colnames(x) %in% colnames(x[, date_cols])]))){
    for (i in 1:ncol(x)){
      #test if it could be a year column
      if(countries:::is.yearcol(x[,i], regularity=regularity, allow_NA=allow_NA)) time_cols <- c(time_cols, i)
    }

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

      #regularity (only check if irregular vectors have not been excluded)
      if (regularity == FALSE){
        score <- score + sapply(temp, function(x) length(unique(x))==1)
      }

      #range
      score <- score + sapply(x[,time_cols], function(x) sum(x %in% 1970:2030)/length(x))

      #leftmost column among the candidates in table and not beyond column 5
      score <- score + as.numeric(time_cols == min(time_cols) & min(time_cols) <= 5)

      # select the first column with the highest score
      time_cols <- time_cols[which.max(score)]
    }
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
