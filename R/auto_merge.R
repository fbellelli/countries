#' Simplified merging of country data tables supporting different country nomenclatures and date formats
#'
#' The aim of this function is to simplify country data merging. The function performs merging of multiple data tables at once and is able to automatically detect country and time columns.  It can further simplify merging by handling differing country naming conventions and date formats.
#' @param ... Data to be merged. Inputs need to be data frames or coercible to data frames
#' @param by A list or a vector indicating the columns to be used for merging the data. If not provided, the function will automatically detect country and time columns and attempt to merge them. Other columns will be merged only if they share the same name. First time column and first two distinct country columns. Vector of regex, or list of column names, name of element in vector or list can be used to decide name in destination table. e.g. c("countries"="(Nation)|(COUNTRY)", "sector"="[Ii]ndustry") or alternatively list("countries"=c("Nation",NA,"COUNTRY","Nation",NA), "sector"=c("Industry","industry",NA,NA,NA)). In case of list the arguments must be of the same length of data tables to merge and names order must correspond to order of datasets. In case of Regex, it matches with first one.
#' @param country_to Nomenclature to which country names should be converted to in the output. Default is \code{simple}. For a description of possible options, refer to the table in the vignette \href{https://fbellelli.github.io/countries/articles/dealing_with_names.html}{Dealing with country names}.
#' @param inner_join Logical value indicating whether to perform an inner join. The default is \code{FALSE}, which results in a full join of the provided tables.
#' @param merging_info Logical value. If \code{TRUE}, the function will output a list containing the merged data and information generated during the merging process, such as the conversion table used for country names or information on table variables.
#' @param verbose Logical value. Print status messages on the console. Default is \code{TRUE}.
#' @import tidyr dplyr fastmatch utils stringr
#' @importFrom lubridate parse_date_time
#' @importFrom knitr kable
#' @export
auto_merge <- function(... , by=NULL, country_to = "ISO3", inner_join = FALSE, merging_info = FALSE, verbose=TRUE){

  ############################################################
  #CAPTURE INPUT DATA ----------------------------------------

  # save initial by call for future reference
  by_init <- by

  # save data in a list as separate data.frames
  data <- list(...)
  data <- lapply(data, as.data.frame)

  # extract column names from tables
  col_names <- sapply(data, colnames, simplify = FALSE)

  # save initial column names for future reference
  col_names_init <- col_names

  ############################################################
  # CHECK INPUTS ---------------------------------------------

  if (length(data)<2) stop("At least two tables need to be provided for merging")
  if (any(sapply(data, ncol)==0) | any(sapply(data, nrow)==0)) stop("Unable to proceed: input data tables need to have at least one column and one row")
  if (!is.atomic(by)&!is.list(by)) stop("Function argument - by - is invalid. It needs to be either a vector of regular expressions, or a list of column names. Refer to the documentation for more information.")
  if (is.list(by)){
    if (!all(sapply(by,is.atomic))) stop("Function argument - by - is invalid. List input needs to contain vectors of column names to merge")
    if (any(sapply(by,function(x){all(is.na(x))}))) stop("Function argument - by - is invalid. One of the name vectors contains all NAs")
    if (length(unique(sapply(by, length))) != 1) stop("Function argument - by - is invalid. Length of name vectors differ from the number of provided tables.")
  }
  if (!is.character(country_to)|length(country_to)!=1|!(country_to %in% colnames(country_reference_list)))stop("The argument - country_to - is invalid. It needs to be one of the nomenclatures names recognised by the function country_name (e.g. ISO3, UN_en, simple, etc...). Refer to the documentation for more information.")
  if (!is.logical(inner_join) | length(inner_join)!=1) stop("Function argument - inner_join - needs to be a logical statement (TRUE/FALSE)")
  if (!is.logical(merging_info) | length(merging_info)!=1) stop("Function argument - merging_info - needs to be a logical statement (TRUE/FALSE)")


  ############################################################
  # CHECK WIDE FORMAT AND PIVOT IF NECESSARY ------------------

  #output status to console
  if (verbose){cat("Identifying columns to merge\n")}

  #loop over all data tables
  pivoted_cols <- list()
  for (i in 1:length(data)){

    #check column names for countries/years
    temp <- countries:::check.wide.format(data[[i]])
    if (!is.null(temp)){
      # pivot table if countries or years were found and adjust name
      pivoted_key_name <- paste0("Table",i,"_pivoted_", colnames(temp)[1])
      data[[i]] <- as.data.frame(pivot_longer(data[[i]], all_of(temp$col_name), names_to = pivoted_key_name, values_to = paste0("Table",i,"_pivoted_data")))

      # move pivoted keys to front of table
      data[[i]] <- data[[i]][, c(pivoted_key_name, colnames(data[[i]])[colnames(data[[i]]) != pivoted_key_name])]

      # convert year to numeric if possible, otherwise add numeric column with detected year in string
      if (colnames(temp)[1] == "year"){
        if (all(grepl('^(?=.)([+-]?([0-9]*)(\\.([0-9]+))?)$', temp$col_name, perl = TRUE))){
          data[[i]][, pivoted_key_name] <- as.numeric(data[[i]][, pivoted_key_name])
        } else {
          data[[i]][,"year detected in column names"] <- temp[fmatch(data[[i]][, pivoted_key_name], temp$col_name), 1]
        }
      }

      # save name of pivoted columns
      pivoted_cols[[length(pivoted_cols)+1]] <- as.vector(temp$col_name)
      names(pivoted_cols)[length(pivoted_cols)] <- paste("Table", i)

      # issue message to console
      if (verbose) cat(paste0(colnames(temp)[1], " detected in column names of Table ", i, ", pivoting columns: ", paste(temp$col_name[1:3], collapse=", ", sep = ""), if (nrow(temp)>3){paste0(", ..., ", temp$col_name[nrow(temp)])}, "\n"))
    }
  }

  # update column names list
  col_names <- sapply(data, colnames, simplify = FALSE)

  ############################################################
  # CLEAN BY ORDER -------------------------------------------

  # identify columns to merge
  temp <- countries:::parse.by.order(data = data, by = by)
  by <- temp$by
  by_types <- temp$by_types



  ############################################################
  #PREPARE DATA FOR MERGING ----------------------------------

  #CONVERT COLUMN NAMES TO DESTINATION NAME ----

  by_table <- as.data.frame(by)

  # merge by name if no merging order is created
  merge_by_name <- FALSE
  if (nrow(by_table) == 0){
    merge_by_name <- TRUE
    if (verbose) cat("No merging keys were found: columns will be merged based on their name\n")
  }

  #loop over every table
  for (i in 1:length(data)){

    #perform check on existing column names to avoid having two columns with destination name
    temp <- !(col_names[[i]] %in% na.omit(unlist(by_table[i,]))) #exclude key cols from names to check
    if (any(names(by) %in% col_names[[i]][temp])){
      colnames(data[[i]])[temp] <- paste0("Table",i,".",col_names[[i]][temp])
    }

    #change key column names to desired destination name
    if (length(by)>0){
      temp <- unlist(by_table[i,])
      names(temp) <- colnames(by_table)
      temp <- na.omit(temp)
      colnames(data[[i]])[match(temp, colnames(data[[i]]))] <- names(temp)
    }
  }


  #CREATE A CONVERSION TABLE FOR COUNTRY NAMES ----

  #make a list of all unique country names
  country_names <- NULL
  if ("country" %in% by_types){

    #message
    if (verbose){cat("Converting country names\n")}

    #extract all country names in the tables
    for (i in which(!is.na(by_table$country))){
      country_names <- unique(append(country_names, unlist(data[[i]]["country"])))
    }

    #check second country column, if present
    if (any("country2" %in% colnames(by_table))){
      for (i in which(!is.na(by_table$country2))){
        country_names <- unique(append(country_names, unlist(data[[i]]["country2"])))
      }
    }

    #Prepare the conversion table with the final country names nomenclature
    country_names <- sort(country_names)
    temp <- suppressMessages(suppressWarnings(country_name(country_names, to=c("simple", country_to))))
    country_conversion <- data.frame(original=country_names,
                                     simple = temp[,1],
                                     final = temp[,2])

    #add flags in conversion table and format final name
    country_conversion$note <- ifelse(!is.na(country_conversion$final), "-",
                                      ifelse(is.na(country_conversion$simple), "name not recognised", paste0("this country has no name in ",country_to," nomenclature")))
    country_conversion$final[is.na(country_conversion$final)] <- country_conversion$original[is.na(country_conversion$final)]

    } else {
    #if there is no country column in the data, then save a blank conversion table
    country_conversion <- NULL
  }



  #CONVERT COUNTRY NAMES ----

  #country columns
  for (i in which(!is.na(by_table$country))){
    data[[i]]$country <- suppressMessages(suppressWarnings(country_name(data[[i]]$country, to = "final", custom_table = country_conversion[,c("original","final")])))
  }

  #country2 columns
  if (any("country2" %in% colnames(by_table))){
    for (i in which(!is.na(by_table$country2))){
      data[[i]]$country2 <- suppressMessages(suppressWarnings(country_name(data[[i]]$country2, to = "final", custom_table = country_conversion[,c("original","final")])))
    }
  }


  #UNIFY FORMAT OF TIME COLUMNS ----

  #proceed only if there are time columns
  time_conversion <- NULL
  if (any("time" %in% colnames(by_table))){

    # Status message
    if (verbose){cat("Checking time columns\n")}

    #check if all time columns are years
    temp <- TRUE
    for (i in which(!is.na(by_table$time))){
      if (countries:::is.yearcol(data[[i]]$time) == FALSE){
        temp <- FALSE
        break
      }
    }

    #if all time columns are years then proceed with merging, otherwise all time columns need to be converted to a standardised format
    if (temp == FALSE){
      time_unformatted <- NULL
      date_formats <- c("dmy","mdy","ymd","y","my","m","dm","md")
      for (i in which(!is.na(by_table$time))){
        time_unformatted <- unique(c(time_unformatted, as.character(data[[i]]$time)))
        data[[i]]$time <- lubridate::parse_date_time(data[[i]]$time, date_formats)
      }

      #save info on time formatting
      time_conversion <- data.frame(time_unformatted = sort(time_unformatted),
                                    time_formatted = lubridate::parse_date_time(sort(time_unformatted), date_formats))
    }
  }


  #CHECK ALL COLUMN NAMES FOR OVERLAPS -------

  #message
  # if (verbose){cat("Preparing for merger")}

  #extract updated column names from tables
  col_names <- sapply(data, colnames, simplify = FALSE)

  #save information on any additional column that will be merged because of its name
  #loop over the column names of each table
  for (i in 1:length(col_names)){

    #check if the names are present in any other table
    temp <- col_names[[i]][col_names[[i]] %in% unlist(col_names[-i])]

    #exclude names that are already captured in by order
    temp <- temp[!(temp %in% names(by))]

    if (merge_by_name){
      #MERGE COLUMNS WITH SAME NAME ACROSS TABLES

      #save in mege table if any recurring name across table
      if (length(temp)>0){
        #loop over every shared name and add it to by_table
        for (j in temp){
            by_table[i,j] <- j
        }
      }
    } else {
      # CHANGE NAME OF COLUMN
      colnames(data[[i]])[colnames(data[[i]]) == temp] <- paste0("Table",i,"_",temp)
    }
  }

  #improve format table
  row.names(by_table) <- paste("Table",row.names(by_table))


  #TABLES WITH NO KEYS -------

  # save table to print
  by_table_for_print <- by_table

  # check if any table has no keys
  n_keys <- rowSums(!is.na(by_table))
  if(any(n_keys == 0)){

    # find the most frequent key
    most_frequent_key <- which.max(apply(by_table, MARGIN = 2, function(x){sum(!is.na(x))}))

    # deal with tables with no keys by adding the most frequent key with NA as inputs
    for (i in which(n_keys == 0)){

      #output message to console
      if (verbose){
        if (verbose) cat(paste0("No merging key found for ", names(n_keys)[i],". Data will be appended. \n"))
      }

      # add key with NA in table to allow merging
      data[[i]][, names(most_frequent_key)] <- NA

      # add key to merging table
      by_table[i, names(most_frequent_key)] <- names(most_frequent_key)

    }
  }

  ############################################################
  #DECIDING MERGER ORDER -------------------------------------

  # decide merging order

  # update list of column names
  col_names <- sapply(data, colnames, simplify = FALSE)

  # count the number of times each key appears in the tables
  n <- apply(by_table, MARGIN = 2, function(x){sum(!is.na(x))})

  # start by merging tables containing the most frequent key
  order_merges <- which(!is.na(by_table[, which.max(n)]))

  # find merging order until all tables are merged
  while (length(order_merges) < nrow(by_table)){

    # count number of merged tables for each key
    n_in_table <- apply(by_table[order_merges,], MARGIN = 2, function(x){sum(!is.na(x))})

    # keep track of keys that are not fully merged (i.e. there are tables with this keys that have not been merged)
    keys_to_merge <- which(n_in_table < n)

    # check which keys are already in the merged table
    keys_in_table <- which(n_in_table > 0)

    # Merge tables with the next incomplete keys
    if (any(keys_to_merge %in% keys_in_table)){

      # select next key
      next_key <- keys_to_merge[keys_to_merge %in% keys_in_table][1]

      # order merger of tables containing the next key to merge
      order_merges <- unique(c(order_merges, which(!is.na(by_table[, next_key]))))

    } else {

      # if there is no table that can be merged, but there are still unmerged keys, it means that there is no chain of keys that allows to merge all tables. To allow merging, create a column of NAs with a bridging key
      # pick one of the remaining keys
      next_key <- keys_to_merge[1]

      # find all tables containing it
      bridge_tabs <- which(!is.na(by_table[, next_key]))

      # add a bridge key to the selected tables with NA values
      bridge_key <- keys_in_table[1]
      for (i in bridge_tabs){
        data[[i]][,names(bridge_key)] <- NA
      }

      # add bridge tables to the merging order
      order_merges <- unique(c(order_merges, bridge_tabs))
    }

  }





  ############################################################
  #PERFORM MERGING -------------------------------------------

  #Print summary to console
  if (verbose){
    temp <- by_table_for_print
    temp[is.na(temp)] <- ""
    cat("The following columns are being merged:")
    print(knitr::kable(temp, "rst"))
    # cat(paste0("Tables will be merged in the following order: ", paste0(order_merges, collapse =", "), "\n"))
  }


  #start merging
  final <- data[[order_merges[1]]]
  data[[order_merges[1]]] <- "memory cleared"
  for (i in order_merges[2:length(order_merges)]){


    #perform merging
    if (inner_join == TRUE){
      final <- suppressMessages(suppressWarnings(inner_join(final, data[[i]])))
    } else {
      final <- suppressMessages(suppressWarnings(full_join(final, data[[i]])))
    }

    #clear from memory original table data
    data[[i]] <- "memory cleared"

    #output status update to console
    if (verbose){
      cat(paste0("\r                                              ",
                 "\rPerformed merges: ",i-1,"/",length(data)-1," "))
    }
  }



  ############################################################
  #FINAL OUTPUT --------------------------------------------

  # final message
  if (verbose) cat(paste0("\r                                              ",
                          "\rMerge complete\n",ifelse(merging_info,"","(Set merging_info to TRUE to save merging details)\n")))

  # Return output
  if (merging_info){
    return(list(merged_table = final,
                info_merged_columns = by_table_for_print,
                info_country_names = country_conversion[,-2],
                info_time_formats = time_conversion,
                pivoted_columns = pivoted_cols,
                call = list(n_data_tables = nrow(by_table),
                            by = by_init,
                            country_to = country_to,
                            inner_join = inner_join)))
  } else {
    return(final)
  }


}




