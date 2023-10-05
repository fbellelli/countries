#' @title Simplified merging supporting different country nomenclatures and date formats
#'
#' @description
#' The aim of this function is to simplify country data merging for quick analyses. Compared to a normal merge function \code{auto_merge()}:
#' \itemize{
#'  \item Is able to perform the merging of multiple data tables at once.
#'  \item Supports automatic detection of columns to merge.
#'  \item It is able to handle different country naming conventions and date formats. For example, it will be able to recognise that "Italy" and "ITA" refer to the same country and will merge the two entries across tables.
#'  \item It detects if data is in a wide format with country names or years in the column names and will automatically pivot the data.
#' }
#'
#' @param ... Data to be merged. Inputs need to be data frames or coercible to data frame. Tables can also be provided into a single list e.g. \code{tab1, tab2, tab3} or \code{list(tab1, tab2, tab3)}.
#' @param by A list or a vector indicating the columns to be used for merging the data. \emph{If not provided, the function will try to automatically detect columns to be merged}. For more information, refer to the details sections.
#' @param country_to Nomenclature to which country names should be converted to in the output. Default is \code{ISO3}. For a description of possible options, refer to the table in the vignette \href{https://fbellelli.github.io/countries/articles/dealing_with_names.html}{Dealing with country names}.
#' @param inner_join Logical value indicating whether to perform an inner join. The default is \code{FALSE}, which results in a full join of the provided tables.
#' @param merging_info Logical value. If \code{TRUE}, the function will output a list containing the merged data and information generated during the merging process, such as information on columns that have been merged or the conversion table used for country names. The default is \code{FALSE}, which results into a single merged table being returned.
#' @param verbose Logical value indicating whether to print status messages on the console. Default is \code{TRUE}.
#' @param auto_melt Logical value indicating whether to automatically pivot country names or years present in the column names. Default is \code{TRUE}. When at least 3 country names or years are found in the column names, the function will automatically transform the table from a wide to a long format by pivoting the country/year columns.
#' @returns If \code{merging_info = FALSE} a single merged table is returned. If \code{merging_info = TRUE}, a list object is returned, containing the merged table (\code{merged_table}), a table summarising which columns have been merged (\code{info_merged_columns}), a table summarising the conversion of country names (\code{info_country_names}), a table summarising the conversion of time columns to a common format (\code{info_time_formats}), a list of all the columns that have been pivoted when wide tables with country or years in column names were detected (\code{pivoted_columns}), a list recapitulating the inputs passed to the function (\code{call}).
#' @seealso \link[countries]{country_name}, \link[countries]{find_keycol}
#' @importFrom fastmatch %fin%
#' @export
#' @details
#' \strong{Automatic detection of columns to merge}.
#'  The automatic detection process starts by first identifying the key of each table, i.e. a set of variables identifying the entries in the table. This process is optimised for common formats of country data.
#'  The function will then try to match key columns across tables based on their values.
#'  Columns containing country names and time information are identified and are processed to take into account different nomenclatures and time formats.
#'  This automatic process works for the most common dataset structures, but it is not foolproof. Therefore, we always advise to check the columns that are being merged by setting \code{verbose = TRUE} and reading the printout.
#'  Moreover, users should be aware that this automatic detection process can increase the overall merging time considerably. This can be especially long for tables containing many columns or when a large number of tables is being merged.
#'
#' \strong{Formatting of \code{by} argument}
#' If an argument is provided to \code{by}, it needs to be either 1) a list of column names, or 2) a vector of regular expressions. The format requirements are the following:
#' \enumerate{
#' \item In case a \strong{list} is passed, each element of the list must be a vector of length equal to the number of tables being merged (i.e., if 3 tables are being merged, the list needs to contain all vectors of length 3). The vectors should contain the names of columns to be merged in each table, \code{NA} can be inserted for tables that do not contain the variable, and names should be ordered in the same order of the tables that are being merged (i.e. the first column name should be present in the first table being merged). The name of the merged columns can be modified by assigning a name to the elements of the list. For example, \code{list("countries"=c("Nation",NA,"COUNTRY"), "sector"=c("Industry","industry",NA))} is requesting to merge the columns \code{tab1$Nation} and \code{tab3$COUNTRY}, and the columns \code{tab1$Industry} and \code{tab2$industry}. These two merged columns will be named \code{"countries"} and \code{"sector"} in the output, as requested by the user.
#' \item In case a \strong{vector} is passed, each element is interpreted as a regular expression to be used for matching the columns to be merged. For example, the same order provided in the list example could be written as \code{c("countries"="Nation|COUNTRY", "sector"="[Ii]ndustry")}. This will merge the first column in each table whose name matches the pattern described by the regular expression and will name the two resulting columns as \code{"countries"} and \code{"sector"} respectively.
#' }
#'
#' @examples
#' # sample data
#' tab1 <- data.frame(Industry = c(1, 1, 2, 2), Nation = c("ITA", "FRA", "ITA", "FRA"), tot = runif(4))
#' tab2 <- data.frame(industry = 1:4, rate = runif(1:4))
#' tab3 <- data.frame(COUNTRY = c("United States", "France", "India"), national_avg = runif(3))
#'
#' # examples of merging orders
#' auto_merge(tab1, tab2, tab3)
#' auto_merge(list(tab1, tab2, tab3))
#' auto_merge(tab1, tab2, tab3, by = c("countries"="Nation|COUNTRY", "sector"="[Ii]ndustry"))
#' auto_merge(tab1, tab2, tab3, country_to = "UN_fr")
auto_merge <- function(... , by=NULL, country_to = "ISO3", inner_join = FALSE, merging_info = FALSE, verbose=TRUE, auto_melt = TRUE){

  ############################################################
  #CAPTURE INPUT DATA ----------------------------------------

  # save initial by call for future reference
  by_init <- by

  # save data in a list as separate data.frames
  data <- list(...)

  # check if input is a list, if so extract content
  if (length(data) == 1 & methods::is(data, "list")){
    if (length(data[[1]]) > 1) data <- data[[1]] else stop("Please provide multiple tables as inputs for merging")
  }

  # convert all elements in list into data.frames
  data <- lapply(data, as.data.frame)

  # extract column names from tables
  col_names <- sapply(data, colnames, simplify = FALSE)

  # save initial column names for future reference
  col_names_init <- col_names

  ############################################################
  # CHECK INPUTS ---------------------------------------------

  if (length(data)<2) stop("At least two tables need to be provided for merging")
  if (any(sapply(data, ncol)<2) | any(sapply(data, nrow)==0)) stop("Unable to proceed: input data tables need to have at least two columns and one row")
  if (!(is.atomic(by)||is.null(by))& !is.list(by)) stop("Function argument - by - is invalid. It needs to be either a vector of regular expressions, or a list of column names. Refer to the documentation for more information.")
  if (is.list(by)){
    if (!all(sapply(by, is.atomic))) stop("Function argument - by - is invalid. List input needs to contain vectors of column names to merge")
    if (any(sapply(by,function(x){all(is.na(x))}))) stop("Function argument - by - is invalid. One of the name vectors contains all NAs")
    if (length(unique(sapply(by, length))) != 1) stop("Function argument - by - is invalid. Length of name vectors differ from the number of provided tables.")
  }
  if (!is.character(country_to)|length(country_to)!=1|!(country_to %in% colnames(country_reference_list)))stop("The argument - country_to - is invalid. It needs to be one of the nomenclatures names recognised by the function country_name (e.g. ISO3, UN_en, simple, etc...). Refer to the documentation for more information.")
  if (!is.logical(inner_join) | length(inner_join)!=1) stop("Function argument - inner_join - needs to be a logical statement (TRUE/FALSE)")
  if (!is.logical(merging_info) | length(merging_info)!=1) stop("Function argument - merging_info - needs to be a logical statement (TRUE/FALSE)")
  if (!is.logical(verbose) | length(verbose)!=1) stop("Function argument - verbose - needs to be a logical statement (TRUE/FALSE)")

  ############################################################
  # CHECK WIDE FORMAT AND PIVOT IF NECESSARY ------------------

  #output status to console
  if (verbose){cat("Identifying columns to merge\n")}

  #loop over all data tables
  pivoted_cols <- list()
  if (auto_melt){

    for (i in 1:length(data)){

      # check if there is need to pivot the table and pivot it
      temp <- auto_melt(data[[i]],
                        names_to = paste0("Table", i, "_pivoted_colnames"),
                        values_to = paste0("Table", i, "_pivoted_values"),
                        verbose = FALSE,
                        pivoting_info = TRUE)

      if(!is.null(temp$pivoted_cols)){
        # update the data with pivoted table
        data[[i]] <- temp$output

        # save name of pivoted columns as record
        pivoted_cols[[length(pivoted_cols)+1]] <- temp$pivoted_cols
        names(pivoted_cols)[length(pivoted_cols)] <- paste("Table", i)

        # print message to console
        if(verbose){

          # check if countries were detected
          temp$is_country <- any(is_country(temp$pivoted_cols))

          # message
          cat(paste0(
            "Table ", i, " - ",
            if(temp$is_country) "countries" else "years", " detected in column names, pivoting columns: ",
            paste(temp$pivoted_cols[1:3], collapse=", ", sep = ""), if (length(temp$pivoted_cols)>3){paste0(if (length(temp$pivoted_cols)>4) ", ..., " else ", ", temp$pivoted_cols[length(temp$pivoted_cols)])},
            "\n"))
        }
      }

    }

    # update column names list
    col_names <- sapply(data, colnames, simplify = FALSE)
  }


  ############################################################
  # CLEAN BY ORDER -------------------------------------------

  # identify columns to merge
  temp <- parse.by.order(data = data, by = by)
  by <- temp$by
  by_types <- temp$by_types



  ############################################################
  #PREPARE DATA FOR MERGING ----------------------------------


  # SAVE TABLE THAT WILL BE USED FOR PRINTING OUT MERGER DETAILS ---

  # convert by order into a table
  by_table <- as.data.frame(by)

  # save a copy of the table to print on screen
  by_table_for_print <- by_table



  #CONVERT COLUMN NAMES TO DESTINATION NAME ----

  # update column name list
  col_names <- sapply(data, colnames, simplify = FALSE)

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

  #CHECK ALL COLUMN NAMES FOR OVERLAPS -------

  # update column name list
  col_names <- sapply(data, colnames, simplify = FALSE)

  # merge by name if no merging order is created (backup strategy)
  merge_by_name <- FALSE
  if (nrow(by_table) == 0){
    merge_by_name <- TRUE
    if (verbose) cat("No merging keys were found. Will try merging all columns with matching names!\n")
  }

  #save information on any additional column that will be merged because of its name
  #loop over the column names of each table
  for (i in 1:length(col_names)){

    #check if the names are present in any other table
    temp <- col_names[[i]][col_names[[i]] %in% unlist(col_names[-i])]

    #exclude names that are already captured in by order
    temp <- temp[!(temp %in% names(by))]

    if (length(temp)>0){
      if (merge_by_name){

        #MERGE COLUMNS WITH SAME NAME ACROSS TABLES

        #save in merge table any recurring name across table by looping over every shared name and adding it to by_table
        for (j in temp){
          by_table[i,j] <- j
          by_table_for_print[i,j] <- j

        }
      } else {

        # CHANGE NAME OF COLUMN
        colnames(data[[i]])[colnames(data[[i]]) == temp] <- paste0("Table",i,"_",temp)
      }
    }
  }

  #improve format by_table
  if (nrow(by_table)>0){
    row.names(by_table) <- paste("Table", row.names(by_table))
    row.names(by_table_for_print) <- paste("Table", row.names(by_table_for_print))

  } else {

    if (verbose) cat("MERGING ABORTED\n")

    # if no merging key is created at this tage, terminate execution here
    stop("No columns to merge were found. Please provide a -by- argument and check the data inputs.")
  }


  # ENFORCE SAME DATA TYPE ON MERGED COLUMNS ---

  # check that data type is the same for merged columns, if not, convert to character
  for (i in colnames(by_table)){

    # extract number of tables containing column i
    tabs <- which(!is.na(by_table[,i]))

    # loop over every table to extract data type of column i
    temp <- NULL
    for (j in tabs){
      temp <- c(temp, class(data[[j]][,i]))
    }

    # if type is not the same in all columns, convert to character
    if (any(temp != temp[1])){

      # issue message
      if (verbose) cat(paste0("Data type for variable ", i, " differ across tables - converting to character\n"))

      # convert all to character to allow merging
      for (j in tabs){
        data[[j]][,i] <- as.character(data[[j]][,i])
      }
    }
  }


  #CREATE A CONVERSION TABLE FOR COUNTRY NAMES ----

  #make a list of all unique country names
  country_names <- NULL
  if ("country" %in% by_types){

    # get final name of first country column
    country_dest_name <- names(by)[by_types == "country"][1]

    #message
    if (verbose){cat("Converting country names\n")}

    #extract all country names in the tables
    for (i in which(!is.na(by_table[, country_dest_name]))){
      country_names <- unique(append(country_names, unlist(data[[i]][country_dest_name])))
    }

    #check second country column, if present
    if (sum(by_types == "country")>1){

      # get name of second country column
      country2_dest_name <- names(by)[by_types == "country"][2]

      # extract names
      for (i in which(!is.na(by_table[, country2_dest_name]))){
        country_names <- unique(append(country_names, unlist(data[[i]][country2_dest_name])))
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



    #CONVERT COUNTRY NAMES ----

    #country columns
    for (i in which(!is.na(by_table[, country_dest_name]))){
      data[[i]][, country_dest_name] <- suppressMessages(suppressWarnings(country_name(data[[i]][, country_dest_name], to = "final", custom_table = country_conversion[,c("original","final")])))
    }

    #country2 columns
    if (sum(by_types == "country")>1){

      for (i in which(!is.na(by_table[, country2_dest_name]))){
        data[[i]][, country2_dest_name] <- suppressMessages(suppressWarnings(country_name(data[[i]][, country2_dest_name], to = "final", custom_table = country_conversion[,c("original","final")])))
      }
    }

  } else {
    #if there is no country column in the data, then save a blank conversion table
    country_conversion <- NULL
  }





  #UNIFY FORMAT OF TIME COLUMNS ----

  #proceed only if there are time columns
  time_conversion <- NULL
  if (any("time" %in% by_types)){

    # get final name of time column
    time_dest_name <- names(by)[by_types == "time"][1]

    # Status message
    if (verbose){cat("Checking time columns\n")}

    #check if all time columns are years
    temp <- TRUE
    for (i in which(!is.na(by_table[,time_dest_name]))){
      if (is.yearcol(data[[i]][,time_dest_name]) == FALSE){
        temp <- FALSE
        break
      }
    }

    #if all time columns are years then proceed with merging, otherwise all time columns need to be converted to a standardised format
    if (temp == FALSE){
      time_unformatted <- NULL
      date_formats <- c("dmy","mdy","ymd","y","my","m","dm","md")
      for (i in which(!is.na(by_table[,time_dest_name]))){
        time_unformatted <- unique(c(time_unformatted, as.character(data[[i]][,time_dest_name])))
        data[[i]][,time_dest_name] <- as.character(lubridate::parse_date_time(data[[i]][,time_dest_name], date_formats))
      }

      #save info on time formatting
      time_conversion <- data.frame(time_unformatted = sort(time_unformatted),
                                    time_formatted = lubridate::parse_date_time(sort(time_unformatted), date_formats))
    }
  }




  # DEAL WITH TABLES WITH NO KEYS -------

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
      data[[i]][, names(most_frequent_key)] <- NA_character_

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
        data[[i]][,names(bridge_key)] <- NA_character_
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
      final <- suppressMessages(suppressWarnings(dplyr::inner_join(final, data[[i]])))
    } else {
      final <- suppressMessages(suppressWarnings(dplyr::full_join(final, data[[i]])))
    }

    #clear from memory original table data
    data[[i]] <- "memory cleared"

    #output status update to console
    if (verbose){
      cat(paste0("\r                                              ",
                 "\rPerforming merge: ",i-1,"/",length(data)-1," "))
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




