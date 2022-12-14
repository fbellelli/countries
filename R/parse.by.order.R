#' Internal function - Parses input merging order for function auto_join
#'
#' @param data list of tables provided to auto_join function
#' @param by by order provided by user to auto_join function
#' @param col_names list of column names in \code{data} tables
#' @return Returns a cleaned \code{by} order and a vector indicating the type of data in each element of the order.
parse.by.order <- function(data, by, col_names){

    if (!is.null(by)){

    # CHECK THAT PROVIDED COLUMN NAMES ARE PRESENT IN TABLES ----

    #check names only if by is a list input (If it is regex conversion guarantees that names are in table)
    if (is.list(by)){
      by_table <- as.data.frame(by)
      for (i in 1:length(data)){
        temp <- unlist(by_table[i,!is.na(by_table[i,])])
        if (!all(temp %in% col_names[[i]])){
          stop(paste0("Function argument - by - is invalid: ", temp[!(temp %in% col_names[[i]])][1], " is not among the column names of table number ", i,"."))
        }
      }
    }

    # TRANSLATE REGEX ORDERS INTO LISTS OF COLUMN NAMES --------
    if (is.atomic(by)){
      new_by <- list()
      for (i in by){ #loop over each regular expression provided

        #identify first column name matching with regex within each table
        temp <- character(0)
        for (j in col_names){
          matches <- grep(i, j, perl=TRUE)
          if (length(matches)>0){
            temp <- c(temp, j[min(matches)])
          } else {
            temp <- c(temp, NA)
          }
        }

        #add identified columns to list preserving original destination name
        new_by[[length(new_by)+1]] <- temp
        names(new_by)[length(new_by)] <- if (!is.null(names(i))) names(i) else temp[!is.na(temp)][1]
      }
      by <- new_by
    }

    # IDENTIFY COUNTRY AND TIME COLUMNS  ---------

    #for each group of variables in by, identify if they are time or country vars
    by_types <- character(0)
    for (i in 1:length(by)){ #loop over each group of merging variables
      temp <- character(0)
      for (j in 1:length(data)){ #loop over each table to check corresponding variable
        col_to_test <- by[[i]][j]
        if (!is.na(col_to_test)){
          if (!is.null(find_countrycol(data[[j]][col_to_test], return_index = TRUE, allow_NA = TRUE))){
            temp <- c(temp,"country")
          } else if (!is.null(find_timecol(data[[j]][col_to_test], return_index = TRUE, allow_NA = TRUE))){
            temp <- c(temp,"time")
          } else {
            temp <- c(temp,"other")
          }
        }
      }
      #for each group of variable select most frequent classification
      by_types[i] <- countries::mode(temp)
    }
  }


  ############################################################
  # GENERATE MERGING ORDER WHEN BY NOT PROVIDED -----------------

  if (is.null(by)){
    key_cols <- list()

    #FIND FIRST TIME AND 1/2 COUNTRY COLUMNS ------------
    for (i in 1:length(data)){

      #find country columns in table
      cols <- find_countrycol(data[[i]], allow_NA = TRUE)
      if (length(cols)==1){
        key_cols[[i]] <- c("country"=cols)
      } else if (length(cols)>1){

        #take data sample of country column names
        temp <- data[[i]][if (nrow(data[[i]])>5000){sample(c(1:nrow(data[[i]])), size = 5000, replace = FALSE)} else {TRUE},cols]

        #convert all to same nomenclature
        temp <- suppressMessages(suppressWarnings(as.data.frame(sapply(temp, country_name))))

        #compare all pairs of columns and eliminate duplicates
        j <- 1
        while (j < ncol(temp)){
          for (k in (j+1):ncol(temp)){
            dupl_cols <- character(0)
            #if more than 90% of the names are identical, assume the column is duplicated, so mark as duplicated the one with more NAs (in case of tie, discard the right-most one)
            if(0.9 < sum(temp[,j] == temp[,k], na.rm=TRUE)/min(sum(!is.na(temp[,j])),sum(!is.na(temp[,k])))){
              if (sum(is.na(temp[,k])) >= sum(is.na(temp[,j]))){
                dupl_cols <- c(dupl_cols,colnames(temp)[k])
              } else {
                dupl_cols <- c(dupl_cols,colnames(temp)[j])
              }
            }
          }
          # remove columns marked as duplicates and move to next column
          temp <- temp[!colnames(temp) %in% dupl_cols]
          j <- j + 1
        }
        #keep only non duplicated columns
        cols <- colnames(temp)

        #save the first 1/2 distinct country column names
        if (length(cols)==1){key_cols[[i]] <- c("country"=cols)}
        if (length(cols)>1){key_cols[[i]]<-c("country"=cols[1], "country"=cols[2])}

      } else {
        #if no country column was detected, then record an empty vector
        key_cols[[i]] <- character(0)
      }

      #find fist time column
      cols <- find_timecol(data[[i]], allow_NA = TRUE)
      if (length(cols)>0) key_cols[[i]] <- c(key_cols[[i]], "time"=cols[1])
    }

    #CREATE MERGING ORDER ----------------
    #initiate variables to store info
    by <- list()
    by_types <- character(0)

    #pair country and time columns across tables
    country <- character(0)
    country2 <- character(0)
    time <- character(0)
    for (i in 1:length(data)){
      types <- names(key_cols[[i]])
      temp <- unname(key_cols[[i]][types=="country"])
      country <- c(country, if (length(temp[1])==0) NA else temp[1])
      country2 <- c(country2, if (length(temp[2])==0) NA else temp[2])
      temp <- unname(key_cols[[i]][types=="time"])
      time <- c(time, if (length(temp[1])==0) NA else temp[1])
    }

    #format result as merging order
    if (!all(is.na(country))){
      by[[length(by)+1]] <- country
      names(by)[length(by)] <- "country"
      by_types[length(by_types)+1] <- "country"
    }
    if (!all(is.na(country2))){
      by[[length(by)+1]] <- country2
      names(by)[length(by)] <- "country2"
      by_types[length(by_types)+1] <- "country"
    }
    if (!all(is.na(time))){
      by[[length(by)+1]] <- time
      names(by)[length(by)] <- "time"
      by_types[length(by_types)+1] <- "time"
    }
  }

  return(list("by"=by, "by_types"=by_types))
}
