#' Find a set of columns that uniquely identifies table entries
#'
#' This function takes a data frame as argument and returns the column names (or indices) of a set of columns that uniquely identify the table entries (i.e. table key). It can be used to automate the search of table keys.
#' Since the function was designed for country data, it will first search for columns containing country names and dates/years. These columns will be given priority in the search for keys. Next, the function prioritises left-most columns in the table.
#' For time efficiency, the function does not test all possible combination of columns, it just tests the most likely combinations. The function will look for the most common country data formats (e.g. cross-sectional, time-series, panel data, dyadic, etc.) and searches for up to 2 additional key columns beyond country and time columns.
#' @param x A data frame object
#' @param return_index A logical value indicating whether the function should return the index of country columns instead of the column names. Default is \code{FALSE}, column names are returned.
#' @param search_only This parameter can be used to restrict the search of table keys to a subset of columns. The default is \code{NA}, which will result in the entire table being searched. Alternatively, users may restrict the search by providing a vector containing the name or the numeric index of columns to check. For example, search could be restricted to the first ten columns by passing \code{1:10}. This could be useful in speeding up the search in wide tables.
#' @param sample_size Either \code{NA} or a numeric value indicating the sample size used for evaluating columns. Default is \code{1000}. If \code{NA} is passed, the function will evaluate the full table. The minimum accepted value is \code{100} (i.e. 100 randomly sampled rows are used to evaluate the columns). This parameter can be tuned to speed up computation on long datasets. Taking a sample could result in inexact identification of key columns, accuracy improves with larger samples.
#' @param allow_NA Logical value indicating whether to allow key columns to have \code{NA} values. Default is \code{allow_NA=FALSE}. If set to \code{TRUE}, \code{NA} is considered as a distinct value.
#' @returns Returns a vector of column names (or indices) that uniquely identify the entries in the table. If no key is found, the function will return \code{NULL}. The output is a named vector indicating whether the identified key columns contain country names (\code{"country"}), year and dates (\code{"time"}), or other type of information (\code{"other"}).
#' @seealso \link[countries]{find_timecol}, \link[countries]{find_countrycol}, \link[countries]{is_keycol}
#' @export
#' @examples
#' example <-data.frame(nation=rep(c("FRA","ALB","JOR"),3),
#'                      year=c(rep(2000,3),rep(2005,3),rep(2010,3)),
#'                      var=runif(9))
#' find_keycol(x=example)
find_keycol <- function(x,
                     return_index=FALSE,
                     search_only=NA,
                     sample_size=1000,
                     allow_NA=FALSE){

  #---- CHECK VALIDITY OF INPUTS ----
  if(!is.data.frame(x)) stop("The argument - x - needs to be a data.frame")
  if(!is.logical(return_index)|is.null(return_index)|length(return_index)>1|is.na(return_index)) stop("The argument - return_index - is either missing or invalid. It needs to be a TRUE/FALSE value.")
  if(!is.numeric(sample_size)&!is.na(sample_size)|length(sample_size)>1){stop("Argument - sample_size - needs to be a numeric value greater or equal to 100")}
  if(is.numeric(sample_size)&round(sample_size)<100){stop("Argument - sample_size - needs to be a numeric value greater or equal to 100")}
  if(!is.logical(allow_NA)|is.null(allow_NA)|length(allow_NA)>1|is.na(allow_NA)) stop("The argument - allow_NA - is either missing or invalid. It needs to be a TRUE/FALSE value.")

  #------- TAKE SAMPLE IF REQUESTED ----------------
  x <- as.data.frame(x)
  if (!is.na(sample_size)){
    x <- x[sample(1:nrow(x), min(round(sample_size), nrow(x))),]
  }

  #------- PREPARE FOR SEARCH ----------------

  #make list of column to search
  if (length(search_only)==1 & any(is.na(search_only))){
    cols <- colnames(x)
  } else if (is.numeric(search_only)){
    #give error if negative index or decimal points
    if(any(search_only%%1>0)|any(search_only<=0)){stop("Argument - search_only - needs to be a vector of positive whole numbers for subsetting column names")}

    #exclude numbers out of bound
    search_only <- search_only[search_only<=ncol(x)]

    #use indices to select column names
    cols <- colnames(x)[search_only]
  } else if (is.character(search_only)){
    #check that all provided names are in the table
    if(!all(search_only %in% colnames(x))){stop("One or more of the names in - search_only - cannot be found among the column names")}
    cols <- search_only
  } else {
    #if argument is not NA, character or numeric, give error:
    stop("Argument - only_search - needs to be a numeric vector or a vector containing column names")
  }

  #initiate status variables
  key_found <- FALSE
  key<-NULL
  country_cols <- NULL
  time_cols <- NULL
  grid_dyad <- NULL


  # --- 1) FIND COUNTRY COLUMN (AND CHECK FOR CROSS-SECTIONAL STRUCTURE)----

  #make a list of country columns
  country_cols <- find_countrycol(x, allow_NA = TRUE, min_share = 0.6)

  #check if any of the country column is key of the table
  if (length(country_cols)>0){
    #check all country columns individually to see if they are key, if more than one, select left-most column and terminate search
    i <- 1
    while (i<=length(country_cols) & key_found==FALSE){
      if (is_keycol(x,country_cols[i], allow_NA= allow_NA, verbose=FALSE)){
        key <- c("country"=country_cols[i])
        key_found <- TRUE
      }
      i<-i+1
    }
  }

  #remove country cols from cols to check
  cols <- cols[!(cols %in% country_cols)]


  # --- 2) LOOK FOR TIME COLUMNS (AND CHECK FOR TIME SERIES STRUCTURE) ----

  if (length(cols)>0 & key_found == FALSE){
    #find time columns
    if (length(cols)==1){
      time_cols <- cols[!is.null(find_timecol(data.frame(x[,cols]), allow_NA = TRUE))] #this conditional statement fixes an issue with naming when there is only one column to evaluate
    } else {
      time_cols <- find_timecol(x[,cols], allow_NA = TRUE)
    }


    #check if time cols are key
    if (length(time_cols)>0 & length(country_cols)==0){
      i <- 1
      while (i<=length(time_cols) & key_found==FALSE){
        if (is_keycol(x,time_cols[i], allow_NA=allow_NA, verbose=FALSE)){
          key <- c("time"=time_cols[i])
          key_found <- TRUE
        }
        i<-i+1
      }
    }

    #remove from cols to check all time cols
    cols <- cols[!(cols %in% time_cols)]
  }

  # --- 3) CHECK FOR PANEL STRUCTURE ----

  if (key_found==FALSE & length(time_cols)>0 & length(country_cols)>0){

    #Make a list of all possible country-time column combination
    grid <- expand.grid(country_cols, time_cols, stringsAsFactors = FALSE)

    #check all combinations and stop if a key is found
    i <- 1
    while (i<=nrow(grid) & key_found==FALSE){
      if (is_keycol(x, c(grid[i,1], grid[i,2]) , allow_NA=allow_NA, verbose=FALSE)){
        key <- c("country"=grid[i,1], "time"=grid[i,2])
        key_found <- TRUE
      }
      i<-i+1
    }
  }


  # --- 5) CHECK FOR DYADIC DATASET
  if (key_found == FALSE & length(country_cols)>1){

    #Make a list of all pairs of possible country column combinations
    grid_dyad <- as.data.frame(t(utils::combn(country_cols,2)))
    grid_dyad$is_different <- FALSE

    #to save time, first test if country pair is identical by taking a sample
    sample_countries <- sample(c(1:nrow(x)),min(100,nrow(x)))

    #check all combinations and stop if a key is found
    i <- 1
    while (i<=nrow(grid_dyad) & key_found==FALSE){
      #check if sample country pair is identical, proceed only if not identical
      if (suppressMessages(suppressWarnings(any(country_name(x[sample_countries,grid_dyad[i,1]], poor_matches=TRUE) != country_name(x[sample_countries, grid_dyad[i,2]], poor_matches=TRUE))))){

        #mark that this country pair is distinct
        grid_dyad$is_different[i] <- TRUE

        #check if it uniquely identifies entries
        if (is_keycol(x, c(grid_dyad[i,1], grid_dyad[i,2]) , allow_NA=allow_NA, verbose=FALSE)){
          key <- c("country"=grid_dyad[i,1], "country"=grid_dyad[i,2])
          key_found <- TRUE
        }

        #check with time variables too
        if (length(time_cols)>0){
          for (j in time_cols){
            if (is_keycol(x, c(grid_dyad[i,1], grid_dyad[i,2], j) , allow_NA=allow_NA, verbose=FALSE)){
              key <- c("country"=grid_dyad[i,1], "country"=grid_dyad[i,2], "time"=j)
              key_found <- TRUE
            }
          }
        }
      }
      i<-i+1
    }
  }


  # --- 6) EVALUATE OTHER IDENTIFIERS ----

  if (key_found==FALSE & length(cols)>0){

  #to reduce search space, restrict combinations to check based on columns already identified:

    #CASES WITH ONE OR LESS DISTINCT COUNTRY COLUMNS
    if ((length(country_cols)<=1 | !any(grid_dyad$is_different))){

      #loop over time columns (while allowing for case with no time cols)
      for (t in min(1, length(time_cols)):length(time_cols)){

        time <- time_cols[t] #this will return NULL if there is no time column

        # 1) check other columns singularly
        i <- 1
        while (i<=length(cols) & key_found==FALSE){
          if (length(country_cols)==0 & length(time_cols)==0){temp <- c("other"=cols[i])}
          if (length(country_cols)>0 & !any(grid_dyad$is_different) & length(time_cols)==0){temp <- c("country"=country_cols[1], "other"=cols[i])}
          if (length(country_cols)==0 & length(time_cols)>0){temp <- c("time"=time, "other"=cols[i])}
          if (length(country_cols)>0 & !any(grid_dyad$is_different) & length(time_cols)==1){temp <- c("country"=country_cols[1], "time"=time, "other"=cols[i])}
          if (is_keycol(x,temp, allow_NA= allow_NA, verbose=FALSE)){
            key <- temp
            key_found <- TRUE
          }
          i<-i+1
        }

        # 2) check other columns two by two
        if (length(cols)>1  & key_found==FALSE){
        grid <- t(utils::combn(cols,2))
        i <- 1
        while (i<=nrow(grid) & key_found==FALSE){
          if (length(country_cols)==0 & length(time_cols)==0){temp <- c("other"=grid[i,1], "other"=grid[i,2])}
          if (length(country_cols)>0 & !any(grid_dyad$is_different) & length(time_cols)==0){temp <- c("country"=country_cols[1], "other"=grid[i,1], "other"=grid[i,2])}
          if (length(country_cols)==0 & length(country_cols)>0){temp <- c("time"=time, "other"=grid[i,1], "other"=grid[i,2])}
          if (length(country_cols)>0 & !any(grid_dyad$is_different) & length(country_cols)==1){temp <- c("country"=country_cols[1], "time"=time, "other"=grid[i,1], "other"=grid[i,2])}
          if (is_keycol(x,temp, allow_NA= allow_NA, verbose=FALSE)){
            key <- temp
            key_found <- TRUE
          }
          i<-i+1
        }
        }
      }
    }



  #CASES WITH MULTIPLE COUNTRY COLUMNS
    if ((length(country_cols)>1 & any(grid_dyad$is_different))){

      #loop over time columns (while allowing for case with no time cols)
      for (t in min(1, length(time_cols)):length(time_cols)){

        time <- time_cols[t] #this will return NULL if there is no time column

        #loop over country pairs
        for (j in 1:nrow(grid_dyad[grid_dyad$is_different,])){

          # 1) check other columns singularly
          i <- 1
          while (i<=length(cols) & key_found==FALSE){
            if (length(time_cols)==0){temp <- c("country"=grid_dyad[grid_dyad$is_different,][j,1], "country"=grid_dyad[grid_dyad$is_different,][j,2], "other"=cols[i])}
            if (length(time_cols)>0){temp <- c("country"=grid_dyad[grid_dyad$is_different,][j,1], "country"=grid_dyad[grid_dyad$is_different,][j,2], "time"=time, "other"=cols[i])}
            if (is_keycol(x,temp, allow_NA= allow_NA, verbose=FALSE)){
              key <- temp
              key_found <- TRUE
            }
            i<-i+1
          }

          # 2) check other columns two by two
          if (length(cols)>1  & key_found==FALSE)
          grid <- t(utils::combn(cols,2))
          i <- 1
          while (i<=nrow(grid) & key_found==FALSE){
            if (length(time_cols)==0){temp <- c("country"=grid_dyad[grid_dyad$is_different,][j,1], "country"=grid_dyad[grid_dyad$is_different,][j,2], "other"=grid[i,1], "other"=grid[i,2])}
            if (length(time_cols)>0){temp <- c("country"=grid_dyad[grid_dyad$is_different,][j,1], "country"=grid_dyad[grid_dyad$is_different,][j,2], "time"=time, "other"=grid[i,1], "other"=grid[i,2])}
            if (is_keycol(x,temp, allow_NA= allow_NA, verbose=FALSE)){
              key <- temp
              key_found <- TRUE
            }
            i<-i+1
          }
        }
      }
    }
  }


  # ---- END) Output keys of table ----

  #Return NAMED vector containing names (or indices) of columns that are the key of the table
  #the name of each entry indicates what type of column it is: country, time, other
  if (return_index){
    key_indx <- match(key,colnames(x))
    names(key_indx)<-names(key)
    return(key_indx)
  }else{
    return(key)
  }

}
