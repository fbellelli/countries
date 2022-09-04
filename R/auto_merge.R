#' Simplified merging of country data tables supporting different country nomenclatures and date formats
#'
#' The aim of this function is to simplify country data merging. The function performs merging of multiple data tables at once and is able to automatically detect country and time columns.  It can further simplify merging by handling differing country naming conventions and date formats.
#' @param ... Data to be merged. Inputs need to be data frames or coercible to data frames
#' @param by A list or a vector indicating the columns to be used for merging the data. If not provided, the function will automatically detect country and time columns and attempt to merge them. Other columns will be merged only if they share the same name. First time column and first two distinct country columns.
#' @param country_to Nomenclature to which country names should be converted to in the output. Default is \code{simple}. For a description of possible options, refer to the table in the vignette \href{https://fbellelli.github.io/countries/articles/dealing_with_names.html}{Dealing with country names}.
#' @param inner_join Logical value indicating whether to perform an inner join. The default is \code{FALSE}, which results in a full join of the provided tables.
#' @param verbose Logical value indicating if status messages should be printed to the console. Default is \code{TRUE}.
#' @param merging_info Logical value. If \code{TRUE}, the function will output a list containing the merged data and all the information generated during the merging process, such as the conversion table used for country names or information on table variables.
#' @import tidyr dplyr fastmatch utils stringr
auto_join <- function(... , by=NULL, country_to="ISO3", coalesce=FALSE, inner_join = FALSE , verbose=TRUE, merging_info = TRUE){

  ############################################################
  #CAPTURE INPUT DATA ----------------------------------------

  data <- list(...)
  data <- lapply(data, as.data.frame)

  #extract column names from tables
  col_names <- sapply(data,colnames)


  ############################################################
  # CHECK INPUTS ---------------------------------------------

  if (any(sapply(data, ncol)==0) | any(sapply(data, nrow)==0)) stop("Unable to proceed: input data tables need to have at least one column and one row")
  if (!is.atomic(by)&!is.list(by)) stop("Function argument - by - is invalid. It needs to be either a vector of regular expressions, or a list of column names. Refer to the documentation for more information.")
  if (is.list(by)){
    if (!all(sapply(by,is.atomic))) stop("Function argument - by - is invalid. List input needs to contain vectors of column names to merge")
    if (any(sapply(by,function(x){all(is.na(x))}))) stop("Function argument - by - is invalid. One of the name vectors contains all NAs")
    if (sapply(by,length)) stop("Function argument - by - is invalid. Length of name vectors differ from the number of provided tables.")
  }
  if (!is.character(country_to)|length(country_to)!=1|!(country_to %in% colnames(country_reference_list)))stop("The argument - country_to - is invalid. It needs to be one of the nomenclatures names recognised by the function country_name (e.g. ISO3, UN_en, simple, etc...). Refer to the documentation for more information.")
  if (!is.logical(simplify) | length(simplify)!=1) stop("Function argument - simplify - needs to be a logical statement (TRUE/FALSE)")


  ############################################################
  # CHECK WIDE FORMAT AND PIVOT IF NECESSARY ------------------

  #loop over all data tables
  for (i in 1:length(data)){

    #check format of table and pivot if necessary
    temp <- check.wide.format(data[[i]])
    if (!is.null(temp)){
      data[[i]] <- pivot_longer(data[[i]], all_of(temp$col_name), names_to = "column_name")
      data[[i]][,paste0("detected_",colnames(temp)[1])] <- temp[fmatch(data[[i]]$column_name, temp$col_name), 1]
    }
  }


  ############################################################
  # CLEAN BY ORDER -------------------------------------------

  temp<-parse.by.order(data=data, by=by, col_names=col_names)
  by <- temp$by
  by_types <- temp$by_types


  ############################################################
  #PREPARE FOR MERGING ---------------------------------------

  #CONVERT COLUMN NAME TO DESTINATION NAME ----

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


  #CONVERT COUNTRY NOMENCLATURE IF REQUESTED ----

  #OBBIETTIVO FINALE: UNA TABELLA

  #CREATE SUPPORT FOR COUNTRIES
  #convert country names and make a list of all country names (merging support for country data)
  #trovare un modo per consentire a utente di forzare a non cambiare nomi paesi: esempio inserendo NA/NULL in country_to
  #invece di convertire country name on the spot, forse meglio aggiungere colonna con nome convertito e usare quello per merging? oppure rischio di trovarmi con troppe colonne di paesi?

  #convert country names, if requested save match tables and merge in one table, then include in output
  #- Think of how to deal with non-recognised countries or entities that are not ISO countries.
  #- think of how to deal with matching with low confidence

  #CREATE SUPPORT FOR TIME


  ############################################################
  #PERFORM MERGING -------------------------------------------

  #allow for inner / full merging


  ############################################################
  #PREPARE OUTPUT --------------------------------------------

  #dare opzione una tabella sola?
  #simplify -> rinominare in "coalesce"
  #in tal caso dare opzione scelta livello aggregation?

  #se merging_info TRUE, append:
  #conversion table used for country names
  #key_cols (generato se - by - non fornito)
  #by iniziale/finale usato per il merging
  #by_types
  #info su paesi/time?
  #warnings?


  return(list(by, by_types))
}
