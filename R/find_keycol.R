#finds key of table
#column or set of column that uniquely identify entries in table
#this function is designed for country data: hence, it will first search for columns containing country names and dates/years. These columns will be given priority in the search for table keys.
#It also prioritise left-most columns in table
#' @param x A data frame object
#' @param return_index A logical value indicating whether the function should return the index of country columns instead of the column names. Default is \code{FALSE}, column names are returned.
#' @param sample_size Either \code{NA} or a numeric value greater than indicating the sample size used for evaluating columns. Default is \code{1000}. If \code{NA} is passed, the function will evaluate the entire dataset. The minimum accepted value is \code{100} (i.e. 100 randomly sampled rows are used to evaluate the columns). This parameter can be tuned to speed up computation on long datasets. Taking a sample could result in inexact identification of key columns, accuracy improves with larger samples.
find_keycol <- function(x,
                     return_index=FALSE,
                     sample_size=1000){

  #---- CHECK VALIDITY OF INPUTS ----
  if(!is.data.frame(x)) stop("The argument - x - needs to be a data.frame")
  if(!is.logical(return_index)|is.null(return_index)|length(return_index)>1|is.na(return_index)) stop("The argument - return_index - is either missing or invalid. It needs to be a TRUE/FALSE value.")
  if(!is.numeric(sample_size)&!is.na(sample_size)|length(sample_size)>1){stop("Argument - sample_size - needs to be a numeric value greater or equal to 100")}
  if(is.numeric(sample_size)&round(sample_size)<100){stop("Argument - sample_size - needs to be a numeric value greater or equal to 100")}

  #------- TAKE SAMPLE IF REQUESTED ----------------
  x <- as.data.frame(x)
  if (!is.na(sample_size)){
    x <- x[sample(1:nrow(x), min(round(sample_size), nrow(x))),]
  }

  #------- PREPARE DATA AND CHECK FOR NA ----------------

  #initiate status variables
  initial_cols <- colnames(x)
  cols <- initial_cols
  key_found <- FALSE
  key<-NULL

  # --- 1) FIND COUNTRY COLUMN (AND CHECK FOR CROSS-SECTIONAL STRUCTURE)----

  #make a list of country columns
  country_cols <- find_countrycol(x, allow_NA = FALSE)

  #check if any of the country column is key of the table
  if (length(country_cols)>0){
    #check all country columns individually to see if they are key, if more than one, select left-most column and terminate search
    i <- 1
    while (i<=length(country_cols) & key_found==FALSE){
      if (is_keycol(x,country_cols[i], allow_NA= FALSE, verbose=FALSE)){
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
    time_cols <- find_timecol(x[,cols], allow_NA = FALSE)

    #check if time cols are key
    if (length(time_cols)>0 & length(country_cols)==0){
      i <- 1
      while (i<=length(time_cols) & key_found==FALSE){
        if (is_keycol(x,time_cols[i], allow_NA=FALSE, verbose=FALSE)){
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
      if (is_keycol(x, c(grid[i,1], grid[i,2]) , allow_NA=FALSE, verbose=FALSE)){
        key <- c("country"=grid[i,1], "time"=grid[i,2])
        key_found <- TRUE
      }
      i<-i+1
    }
  }


  # --- 5) CHECK FOR DYADIC DATASET
  if (key_found == FALSE & length(country_cols)>1){

    #Make a list of all pairs of possible country column combinations
    grid_dyad <- t(combn(country_cols,2))
    grid_dyad$is_different <- FALSE

    #to save time, first test if country pair is identical by taking a sample
    sample_countries <- sample(c(1:nrow(x)),min(100,nrow(x)))

    #check all combinations and stop if a key is found
    i <- 1
    while (i<=nrow(grid_dyad) & key_found==FALSE){
      #check if sample country pair is identical, proceed only if not identical
      if (suppressMessages(suppressWarnings(any(country_name(x[sample_countries,grid_dyad[i,1]]) != country_name(x[sample_countries, grid_dyad[i,2]]))))){

        #check if it uniquely identifies entries
        if (is_keycol(x, c(grid_dyad[i,1], grid_dyad[i,2]) , allow_NA=FALSE, verbose=FALSE)){
          key <- c("country"=grid_dyad[i,1], "country"=grid_dyad[i,2])
          key_found <- TRUE
        }

        #check with time variables too
        if (length(time_cols)>0){
          for (j in time_cols){
            if (is_keycol(x, c(grid_dyad[i,1], grid_dyad[i,2], j) , allow_NA=FALSE, verbose=FALSE)){
              key <- c("country"=grid_dyad[i,1], "country"=grid_dyad[i,2], "time"=j)
              key_found <- TRUE
            }
          }
        }

        i<-i+1
      }
    }
  }


  # --- &) EVALUATE OTHER IDENTIFIERS ----

  if (key_found==FALSE){


  # FORSE LIMITARE ALLE PRIME 3/5 VARS IN COLS,
  # OPPURE fare loop che scorre da sinistra a destra provando una per volta (esponenziale!)
  # OPPURE combinare loop + limite 3/5 vars in cols
  check_cols <- c(country_cols,time_cols, cols[1:5])

  #make list of all possible column combination to check
  cols #se no country e no time
  expand.grid(country_cols, cols)  #se country no time, altrimenti salta
  expand.grid(time_cols, cols) # se time no country, altrimenti salta
  expand.grid(country_cols, time_cols, cols) # se  country e time, always check? oppure solo se no dyad: !any(grid_dyad$is_different)
  expand.grid(country_cols, country_cols, cols) #se piu di un country e no time, altrimenti salta (any(grid_dyad$is_different) -> filtrare solo i different)
  expand.grid(country_cols, country_cols, time_cols, cols) #se time, piu country e dyad si. (any(grid_dyad$is_different) -> filtrare solo i different)
  #lo stesso, ma provare con 2 cols
  #altrimenti emettere: message key columns not found



  #rank combination based on the position of the columns they contain.
  #score = sum of rank of all columns involved in the combination, where the column rank is its position in the table (e.g. a combination of column 1 and 2, has a score 3)
  #assign a rank of 0 to all country and time cols.

  #starting from the combination with the lowest score, test every combination to see if they could be a key. Stop as soon as one is found.

  }

  # ---- END) Output keys of table ----

  #Return NAMED vector containing names (or indices) of columns that are the key of the table
  #the name of each entry indicates what type of column it is: country, time, other
  if (return_index){
    key_indx <- match(key,initial_cols)
    names(key_indx)<-names(key)
    return(key_indx)
  }else{
    return(key)
  }







}
