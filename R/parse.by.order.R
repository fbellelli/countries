#' Internal function - Parses input merging order for function auto_join
#'
#' @param data list of tables provided to auto_join function
#' @param by by order provided by user to auto_join function
#' @noRd
#' @keywords Internal
#' @returns Returns a cleaned \code{by} order and a vector indicating the type of data in each element of the order.
parse.by.order <- function(data, by = NULL){

  # get column names in data
  col_names <- sapply(data, colnames, simplify = FALSE)

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
        names(new_by)[length(new_by)] <- if (!is.null(names(by))) names(by)[length(new_by)] else temp[!is.na(temp)][1]
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
      by_types[i] <- countries::Mode(temp)
    }
  }


  ############################################################
  # GENERATE MERGING ORDER WHEN BY NOT PROVIDED -----------------

  if (is.null(by)){

    # FIND KEY COLUMNS ------------

    key_cols <- list()
    for (i in 1:length(data)){
      key_cols[[i]] <- find_keycol(data[[i]], allow_NA = TRUE, sample_size = 5000)
    }

    # FIND IF "OTHER" KEYS ARE OVERLAPPING ACROSS TABLES ------------

    # create empty holder for any other column
    other <- list()

    # Check with tables contain a key of type "other"
    tables_other_cols <- sapply(key_cols, function(x){any(names(x) == "other")})

    # if a key of type "other" was detected in more than 1 table, proceed to check if their values are overlapping across tables, to decide whether merging is needed
    if (sum(tables_other_cols) > 1){

      # loop over every combination of tables
      for (i in 1:(sum(tables_other_cols) - 1)){
        for (j in (i+1):sum(tables_other_cols)){

          # get names of "other" key columns in table i and j
          tab_i <- which(tables_other_cols)[i]
          tab_j <- which(tables_other_cols)[j]
          names_i <- key_cols[[tab_i]][names(key_cols[[tab_i]]) == "other"]
          names_j <- key_cols[[tab_j]][names(key_cols[[tab_j]]) == "other"]

          #extract a sample of rows to check overlap
          sample_i <- data.frame(data[[tab_i]][sample(1:(nrow(data[[tab_i]])), size = min(nrow(data[[tab_i]]), 5000), replace = FALSE), names_i])
          sample_j <- data.frame(data[[tab_j]][sample(1:(nrow(data[[tab_j]])), size = min(nrow(data[[tab_j]]), 5000), replace = FALSE), names_j])

          # ensure names stay the same
          colnames(sample_i) <- names_i
          colnames(sample_j) <- names_j

          # loop over every combination of "other" column in the two tables
          for (col_i in names_i){

            # initiate overlap score container
            score <- rep(0, length(names_j))

            # loop over all other columns in table j to calculate the overlap
            if (length(names_j)>0){
              for (col_j in names_j){

                # calculate number of overlapping unique values
                unique_i <- as.character(unique(sample_i[, col_i]))
                unique_j <- as.character(unique(sample_j[, col_j]))
                n_intersect <- sum(unique_i %fin% unique_j)

                # calculate overlap score (weighting penalises series which have very different number of unique values)
                min_length <- min(length(unique_i), length(unique_j))
                max_length <- max(length(unique_i), length(unique_j))
                score[match(col_j, names_j)] <- n_intersect / min_length #(min_length * log2(1 + max_length/min_length))

              }

              # if the two columns share the same name, multiply the score by 4
              score <- score + 3 * score * (tolower(col_i) == tolower(names_j))

              # if overlap score is higher than 50%, then merge the two columns and move to next col_i
              if (any(score > 0.5)){

                # select the column in table j with the highest overlap score
                col_overlap <- names_j[which.max(score)]

                if (length(other) > 0) {
                  # check if col_i has already been merging to something in previous tables
                  merged_to_i <- which(sapply(other, FUN = function(x) col_i %in% x[tab_i]))

                  # check if col_overlap has already been merging to something in previous tables
                  merged_to_j <- which(sapply(other, FUN = function(x) col_overlap %in% x[tab_j]))
                } else {
                  merged_to_i <- character(0)
                  merged_to_j <- character(0)
                }

                if (length(merged_to_i) == 0 & length(merged_to_j) == 0){

                  # if the columns have never been merged to anything yet, create a new entry in merge order

                  # save columns to merge
                  other[[length(other) + 1]] <- rep(NA_character_, length(data))
                  other[[length(other)]][c(tab_i, tab_j)] <- unname(c(col_i, col_overlap))

                  # assign name of column in table i
                  names(other)[[length(other)]] <- col_i

                  # remove col_overlap from names_j
                  names_j <- names_j[names_j != col_overlap]

                } else if (length(merged_to_i) > 0 & length(merged_to_j) == 0){

                  # if col_i is already merged to something, add col_overlap to same merge order only if score is high
                  if (max(score) > 0.7){
                    other[[merged_to_i]][tab_j] <- col_overlap

                    # remove col_overlap from names_j
                    names_j <- names_j[names_j != col_overlap]
                  }

                } else if (length(merged_to_i) == 0 & length(merged_to_j) > 0){

                  # if col_overlap is already merged to something, add col_i to same merge order
                  if (max(score) > 0.7){
                    other[[merged_to_j]][tab_i] <- col_i
                  }
                }

              }

            }

          }

        }
      }

    }


    # CREATE MERGING ORDER ----------------
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
    if (length(other)>0){
      by <- append(by, other)
      by_types <- append(by_types, names(other))
    }
  }

  return(list("by"=by, "by_types"=by_types))
}
