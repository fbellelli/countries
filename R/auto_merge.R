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
auto_merge <- function(... , by=NULL, country_to="ISO3", inner_join = FALSE , merging_info = FALSE, verbose=TRUE){

  ############################################################
  #CAPTURE INPUT DATA ----------------------------------------

  data <- list(...)
  data <- lapply(data, as.data.frame)

  #extract column names from tables
  col_names <- sapply(data,colnames)


  ############################################################
  # CHECK INPUTS ---------------------------------------------

  if (length(data)<2) stop("At least two tables need to be provided for merging")
  if (any(sapply(data, ncol)==0) | any(sapply(data, nrow)==0)) stop("Unable to proceed: input data tables need to have at least one column and one row")
  if (!is.atomic(by)&!is.list(by)) stop("Function argument - by - is invalid. It needs to be either a vector of regular expressions, or a list of column names. Refer to the documentation for more information.")
  if (is.list(by)){
    if (!all(sapply(by,is.atomic))) stop("Function argument - by - is invalid. List input needs to contain vectors of column names to merge")
    if (any(sapply(by,function(x){all(is.na(x))}))) stop("Function argument - by - is invalid. One of the name vectors contains all NAs")
    if (sapply(by,length)) stop("Function argument - by - is invalid. Length of name vectors differ from the number of provided tables.")
  }
  if (!is.character(country_to)|length(country_to)!=1|!(country_to %in% colnames(country_reference_list)))stop("The argument - country_to - is invalid. It needs to be one of the nomenclatures names recognised by the function country_name (e.g. ISO3, UN_en, simple, etc...). Refer to the documentation for more information.")
  if (!is.logical(inner_join) | length(inner_join)!=1) stop("Function argument - inner_join - needs to be a logical statement (TRUE/FALSE)")
  if (!is.logical(merging_info) | length(merging_info)!=1) stop("Function argument - merging_info - needs to be a logical statement (TRUE/FALSE)")


  ############################################################
  # CHECK WIDE FORMAT AND PIVOT IF NECESSARY ------------------

  #output status to console
  if (verbose){cat("Cleaning data and identifying columns to merge")}

  #loop over all data tables
  for (i in 1:length(data)){

    #check format of table and pivot if necessary
    temp <- check.wide.format(data[[i]])
    if (!is.null(temp)){
      data[[i]] <- pivot_longer(data[[i]], all_of(temp$col_name), names_to = paste0("Table",i,"_pivoted_column_name"), values_to = paste0("Table",i,"_pivoted_column_value"))
      data[[i]][,paste0("detected_",colnames(temp)[1])] <- temp[fmatch(data[[i]][,paste0("Table",i,"_pivoted_column_name")], temp$col_name), 1]
    }
  }


  ############################################################
  # CLEAN BY ORDER -------------------------------------------

  temp <- parse.by.order(data=data, by=by, col_names=col_names)
  by <- temp$by
  by_types <- temp$by_types


  ############################################################
  #PREPARE DATA FOR MERGING ----------------------------------

  #CONVERT COLUMN NAMES TO DESTINATION NAME ----

  by_table <- as.data.frame(by)

  #loop over every data table
  for (i in 1:length(data)){

    #perform check on existing column names to avoid having two columns with destination name
    temp <- !col_names[[i]] %in% na.omit(unlist(by_table[i,])) #exclude key cols from names to check
    if (any(names(by) %in% col_names[[i]][temp])){
      colnames(data[[i]])[temp] <- paste0("X.",col_names[[i]][temp])
    }

    #change key column names to desired destination name
    temp <- na.omit(unlist(by_table[i,]))
    colnames(data[[i]])[match(temp, colnames(data[[i]]))] <- names(temp)
  }


  #CREATE A CONVERSION TABLE FOR COUNTRY NAMES ----

  #make a list of all unique country names
  country_names <- NULL
  if ("country" %in% by_types){

    #extract data in country columns
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


  #CHECK ALL COLUMN NAMES FOR OVERLAPS ----

  #extract updated column names from tables
  col_names <- sapply(data,colnames)

  #save information on any additional column that will be merged because of its name
  #loop over the column names of each table
  for (i in 1:length(col_names)){

    #check if the names are present in any other table
    temp <- col_names[[i]][col_names[[i]] %in% unlist(col_names[-i])]

    #exclude names that are already captured in by order
    temp <- temp[!(temp %in% names(by))]

    #save if any
    if (length(temp)>0){
      #loop over every shared name and add it to by_table
      for (j in temp){
          by_table[i,j] <- j
      }
    }
  }

  #improve format table
  row.names(by_table)<-paste("Table",row.names(by_table))




  ############################################################
  #PERFORM MERGING -------------------------------------------


  #allow for inner / full merging
  final <- data[[1]]
  data[[1]] <- "memory cleared"
  for (i in 2:length(data)){

    #output status update to console
    if (verbose){
      cat(paste0("\r                                              ",
                 "\rPerforming merging: ",i-1,"/",length(data)-1," "))
    }


    #perform merging
    if (inner_join == TRUE){
      final <- suppressMessages(inner_join(final, data[[i]]))
    } else {
      final <- suppressMessages(full_join(final, data[[i]]))
    }

    #clear from memory original table data
    data[[i]] <- "memory cleared"
  }

  #clean up console
  cat("\r                                            ")



  ############################################################
  #PREPARE OUTPUT --------------------------------------------

  #Print summary to console
  if (verbose){
    temp <- by_table
    temp[is.na(temp)] <- ""
    cat(paste0("\rMerge complete - the following columns were merged:",ifelse(merging_info,"","\n(Set merging_info to TRUE to save details)")))
    print(knitr::kable(temp, "rst"))
  }


  #Return output
  if (merging_info){
    return(list(merged_table = final,
                info_merged_columns = by_table,
                info_country_names = country_conversion[,-2],
                info_time_formats = time_conversion))
  } else {
    return(final)
  }


}




