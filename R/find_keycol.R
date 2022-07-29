#finds key of table
#column or set of column that uniquely identify entries in table
#this function is designed for country data: hence, it will first search for column containing country names and dates/years.
#It also prioritise left-most columns in table

find_keycol <- function(x,
                     return_index=FALSE,
                     allow_NA=FALSE){

  #---- CHECK VALIDITY OF INPUTS ----
  if(!is.data.frame(x)) stop("The argument - x - needs to be a data.frame")
  if(!is.logical(allow_NA)|is.null(allow_NA)|length(allow_NA)>1|is.na(allow_NA)) stop("The argument - allow_NA - is either missing or invalid. It needs to be a TRUE/FALSE value.")

  #------- PREPARE DATA AND CHECK FOR NA ----------------

  #initiate status variables
  initial_cols <- colnames(x)
  key_found <- FALSE
  key<-NULL

  #check for columns containing NA and exclude them if requested
  cols <- initial_cols
  if(allow_NA == FALSE){
    cols <- cols[!apply(is.na(x[,cols]), MARGIN=2,FUN=any, simplify=TRUE)]
  }

  #transform any factor column to character
  x <- x[,cols]
  x[sapply(x, is.factor)] <- lapply(x[sapply(x, is.factor)], as.character)


  # --- 1) FIND COUNTRY COLUMN (AND CHECK FOR CROSS-SECTIONAL STRUCTURE)----

  #make a list of country columns
  country_cols <- find_countrycol(x, allow_NA = allow_NA)

  #check if any of the country column is key of the table
  if (length(country_col)>0){
    #check all country columns individually to see if they are key, if more than one, select left-most column and terminate search
    i <- 1
    while (i<=length(country_cols) & key_found==FALSE){
      if (is_keycol(x,country_cols[i], allow_NA=allow_NA, verbose=FALSE)){
        key <- c("country"=country_cols[i])
        key_found <- TRUE
      }
      i<-i+1
    }

    #remove country cols from cols to check
    cols <- cols[!(cols %in% country_cols)]
  }

  # --- 2) LOOK FOR TIME COLUMNS (AND CHECK FOR TIME SERIES STRUCTURE) ----

  if (length(cols)>0 & key_found == FALSE){
    #find time columns
    time_cols <- find_timecol(x[,cols], allow_NA = allow_NA, regularity = FALSE)

    #check if time cols are key
    if (length(time_cols)>0){
      while (i<=length(time_cols) & key_found==FALSE){
        if (is_keycol(x,time_cols[i], allow_NA=allow_NA, verbose=FALSE)){
          key <- c("time"=time_cols[i])
          key_found <- TRUE
        }
        i<-i+1
      }

      #remove from cols all time cols
      cols <- cols[!(cols %in% time_cols)]
    }
  }

  # --- 3) CHECK FOR PANEL STRUCTURE ----

  if (key_found==FALSE & length(time_cols)>0 & length(country_cols)>0){

    #Make a list of all possible country-time column combination
    grid <- expand.grid(country_cols, time_cols, stringsAsFactors = FALSE)

    #rank combinations

    #check all combinations and stop if a key is found

  #to do<------------!!!!!!!!!!! ////&&&&&&%%%%çççççç------!!!!!!!@@@@@@@°°°°°°°°
  }

  # --- 4) EVALUATE ALL COMBINATION OF COLUMNS ----

  #make list of all possible column combination.
  grid <- expand.grid(rep(list(cols), length(cols)), stringsAsFactors = FALSE)
  grid <- grid[!duplicated(t(apply(check_combs, 1, sort))),] #remove permutations of the same combination
  #remove rows with all same cols?

  #rank combination based on the position of the columns they contain.
  #score = sum of rank of all columns involved in the combination, where the column rank is its position in the table (e.g. a combination of column 1 and 2, has a score 3)
  #assign a rank of 0 to all country and time cols.

  #starting from the combination with the lowest score, test every combination to see if they could be a key. Stop as soon as one is found.

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
