#' Easily visualise country data with a map
#'
#' \code{quick_map()} allows to plot country \href{https://en.wikipedia.org/wiki/Choropleth_map}{chloropleth maps} with one line of code.
#' The only inputs required are a \code{data.frame} object and the name of the column to plot.
#' The function uses \code{country_name()}'s capabilities to automatically match country names to one of the territories in the \href{https://en.wikipedia.org/wiki/ISO_3166-1}{ISO standard 3166-1}. This allows fuzzy matching of country names in multiple languages and nomenclatures.
#' For some map examples, see \href{https://fbellelli.github.io/countries/articles/quick_map.html}{this article}.
#' @param data Table (data.frame) containing the data to plot. Each row in the table should correspond to a country. One of the columns should contain country names.
#' @param plot_col Name of the column to plot.
#' @param theme A numeric value or name identifying one of the predefined visual themes for the map. Can be a number between 1 and 11, or one of the predefined theme's names: \code{c("Default", "Greyscale", "Candy", "RedBlue", "Dark", "Reds", "Blues", "Greens", "Viridis", "Cividis", "Distinct", "Distinct2", "Paired")}. If \code{0} or \code{"NoTheme"} is passed, no theme will be applied (default `ggplot2`'s settings are used).
#' @param zoom This argument defines the zoom applied to the map. It can be either a string identifying one of the predefined zoom boxes (\code{"Default", "World", "Africa", "Asia", "Europe", "SEAsia", "NAmerica", "CAmerica", "SAmerica", "Oceania"}). Alternatively, the user may provide a numeric vector of length 4 describing the min/max longitude and latitude (e.g. \code{c(-80, -35, -55, 10)} defines a zoom on South America).
#' @param verbose Logical value indicating whether to print messages to the console. Default is \code{FALSE}.
#' @param save_to Path to the file where the plot is to be saved. This need to be in an existing directory. The default is \code{NULL}, which does not save the plot.
#' @param width_plot  Width (in cm) when plot is saved to a file. The ratio between height and width is fixed. This argument is only relevant if \code{save_to} is different from \code{NULL}. Default is \code{30}. For custom saving options the function \code{ggsave()} can be used.
#' @param name_legend String giving the name to be used for the plotted variable in the legend of the map. If nothing is provided, the default is to use the name in \code{plot_col}.
#' @param reverse_palette Logical value indicating whether to reverse the order of the colours in the palette. Default is \code{FALSE}.
#' @param col_breaks Only relevant for numeric data. This argument allows the user to provide manual breaks for the colour scale. Needs to be a numeric vector (\code{c(0, 100, 500, 1000)}). Default is \code{NULL}, which will result in breaks being automatically selected by the function. Note that data with 6 or less unique values will be treated as factor by the function.
#' @param col_border Colour of border line separating countries and landmasses. Default is \code{"black"}.
#' @param col_na Colour for countries with missing data (NAs). Default is \code{"grey97"}.
#' @param width_border Numeric value giving the width of the border lines between countries. Default is `0.1`.
#' @returns ggplot object
#' @export
#' @import ggplot2
#' @details
#' \strong{Good to know}
#'
#' \code{quick_map()} only allows plotting of territories in the ISO standard 3166-1. It does not support plotting of other regions.
#' The output of the function is a ggplot object. This means means that users can then customise the look of the output by applying any of ggplot's methods.
#'
#' \strong{Disclaimer}
#'
#' Territories' borders and shapes are intended for illustrative purpose. They might be outdated and do not imply the expression of any opinion on the part of the package developers.
#' @examples
#' # creating some sample data to plot
#' example_data <- data.frame(country = random_countries(100), population = runif(100))
#'
#' # make a map
#' quick_map(example_data, "population")
#'
#' # The function provides several predefined themes
#' quick_map(example_data, "population", theme = 3)
#' quick_map(example_data, "population", theme = "Reds")
#'
#' # provide breaks for the colour scale
#' quick_map(example_data, "population", col_breaks = c(0, 1e5, 1e6, 1e7, 1e8, 1e9))
quick_map <- function(data, plot_col,
                      theme = 1,
                      zoom = "Default",
                      verbose = FALSE,
                      save_to = NULL,
                      width_plot = 30,
                      name_legend = NULL,
                      reverse_palette = FALSE,
                      col_breaks = NULL,
                      col_border = "black",
                      col_na = "grey97",
                      width_border = 0.1){



  # PERFORM CHECKS ON INPUTS ---------------------

  if (!is.data.frame(as.data.frame(data))|is.null(data)){stop("Argument - data - needs to be a dataframe")}
  if (is.na(plot_col)|length(plot_col)>1|!is.character(plot_col)) stop("Invalid input for - plot_col - it needs to be a single column name in character format")
  if (!plot_col %in% colnames(data)) stop(paste0("The column ", plot_col, " is not in the table - data -"))
  if (is.na(verbose)|is.null(verbose)) stop("The argument - verbose - cannot be NA or NULL. It needs to be a logical value")
  if (!is.logical(verbose) | length(verbose)!=1) stop("Function argument - verbose - needs to be a logical statement (TRUE/FALSE)")
  if (!is.logical(reverse_palette) | length(reverse_palette)!=1) stop("Function argument - reverse_palette - needs to be a logical statement (TRUE/FALSE)")
  if (is.na(reverse_palette)|is.null(reverse_palette)) stop("The argument - reverse_palette - cannot be NA or NULL. It needs to be a logical value")
  if (is.null(zoom)) stop("NULL is an invalid input for - zoom - needs to be a vector of lat/long values or a name of one of the predefined regions. Use - 'Default' - instead?")
  if (!is.atomic(zoom)) stop("invalid input for - zoom - needs to be a vector of lat/long values or a name of one of the predefined regions")
  if (!methods::is(width_plot, "numeric") | length(width_plot) != 1 | any(is.na(width_plot))) stop("invalid input for - width_plot - needs to be a positive numeric value")
  if (width_plot <= 0) stop("invalid input for - width_plot - needs to be a positive numeric value")
  if (!is.null(save_to)){
    if (!methods::is(save_to, "character") | length(save_to) > 1 | any(is.na(save_to))) stop("invalid input for - save_to - needs to be a string containing the path to where you wish to save the map")
  }
  if (is.null(col_border)) stop("Input - col_na - cannot be NULL")
  if (is.na(col_border)| !(is.atomic(col_border) || is.null(col_border)) | !methods::is(col_border, "character") |is.logical(col_border)) stop("invalid input for - col_border - it needs to be a single colour name")
  if (is.null(col_na)) stop("Input - col_na - cannot be NULL")
  if (is.na(col_na)|!is.atomic(col_na)| !methods::is(col_na, "character")|is.logical(col_na)|length(col_na)>1) stop("invalid input for - col_na - it needs to be a single colour name")
  if (!is.null(col_breaks)){
    if (!is.atomic(col_breaks)| !methods::is(col_breaks,"numeric")) stop("invalid input for - col_breaks - please provide numberic breaks for the colour scale")
    if (any(is.na(col_breaks))) stop("invalid input for - col_break - no NAs allowed")
  }
  if (!is.null(name_legend)){
    if (!is.character(name_legend)) stop("Argument - name_legend - needs to be a string")
    if (length(name_legend) > 1) stop("Argument - name_legend - needs to be a single string")
  }

  # CHECK THEME NAME OR NUMBER

  # predefined themes
  theme_numbers <- 1:13
  theme_names <- c("Default", "Greyscale", "Candy", "RedBlue", "Dark", "Reds", "Blues", "Greens", "Viridis", "Cividis", "Distinct", "Distinct2", "Paired")
  if (is.null(theme)) stop("Input - theme - cannot be NULL. Use 1 instead?")
  if (!is.atomic(theme)|is.logical(theme)|length(theme)>1) stop("invalid input for - theme - it needs to be a valid theme name or integer")
  if (!(theme %in% c(0, theme_numbers) | theme %in% c("NoTheme", theme_names))) stop("invalid input for - theme - it needs to be a valid theme name or integer")


  # CHECK ZOOM INPUT VALUE

  # Predefined zoom windows
  zoom_windows <- list(
    Default = c(-155, 175,-50, 78),
    World = c(-180, 180, -90, 90),
    Africa = c(-18, 50, -33, 35),
    Asia = c(30, 155, -10, 75),
    SEAsia = c(90, 155, -15, 30),
    Europe = c(-20, 45, 35, 70),
    NAmerica = c(-165, -50, 10, 78),
    CAmerica = c(-110, -55, 0 , 30),
    SAmerica = c(-80, -35, -55, 10),
    Oceania = c(110, 180, -50, 10)
  )

  # check format for numeric inputs
  if (all(is.numeric(zoom)) & length(zoom) == 4){

    # Check value of provided zoom window
    if (!all(abs(zoom) <= c(180, 180, 90, 90))) stop("Values passed to - zoom - are out of range. Longitude cannot exceed +-180 and latitude cannot exceed +-90.")
    if (zoom[1]>=zoom[2]) stop("In values passed to - zoom -, the minimum longitude needs to be smaller than the maximum longitude")
    if (zoom[3]>=zoom[4]) stop("In values passed to - zoom -, the minimum latitude needs to be smaller than the than maximum latitude")

    # if checks are passed, take user input as zoom for the plot
    zoom_plot <- zoom

  } else if (length(zoom) == 1 & is.character(zoom) & zoom %in% names(zoom_windows)) {

    # If valid region is requested, retrieve predefined zoom values
    zoom_plot <- zoom_windows[[zoom]]

  } else {

    # Otherwise, give an error when format of zoom is incorrect
    stop("Input - zoom - is invalid. Check that the format or spelling is correct.")

  }



  # CHECK DATA KEYS FOR PLOTTING

  # convert input to data.frame
  data <- as.data.frame(data)

  # Find keys of the table
  keys <- find_keycol(data, allow_NA = T)

  # check that there is a country column
  if (! "country" %in% names(keys)) stop("No country column found in the table")

  # Stop execution if there are multiple keys (i.e. multiple data point per country)
  if (length(keys)>1) stop("Multiple data points per country were found in - data -. Please provide a table with one input per country.")



  # IDENTIFY COUNTRIES FOR PLOTTING --------------------

  # extract name of country column
  country_col <- keys["country"]

  # convert country names to ISO3 code for plotting
  country_col_ISO <- paste0(country_col, "_ISO3")
  data[, country_col_ISO] <- country_name(data[,country_col], poor_matches = TRUE, verbose = verbose)

  # check if plot_col is identical to one of the columns in world table
  plot_col_original <- plot_col
  if (plot_col %in% colnames(countries::world)){
    plot_col <- paste0("DATA_", plot_col)
    colnames(data)[colnames(data) == plot_col_original] <- plot_col
  }

  # Merge plotting data with world map
  world <- dplyr::left_join(countries::world, data[,c(country_col_ISO, plot_col)],
                            by = c("ISO3" = country_col_ISO))



  # DISCRETISE COLOURS FOR CONTINOUS VARIABLES -------------------

  # make a list of unique data values
  values <- na.omit(unique(unlist(world[,plot_col])))
  n_values <- length(values)

  # Bucketise continous scale
  if (theme != 0 & theme != "NoTheme"){

    if (is.numeric(values) & n_values > 6){

      # make a copy of original column values
      world[, paste0("original_", plot_col)] <- world[, plot_col]

      # choose good colour breaks if user didn't provide them
      if (length(col_breaks) > 0){

        # user definer breaks for the colour
        buckets <- col_breaks
        n_values <- length(col_breaks)

      } else {

        # number of buckets
        n_values <- 6

        # decide breaks for the plot_col variable
        buckets <- stats::quantile(x = data[, plot_col], probs = seq(0, 1, length.out = n_values + 1), na.rm = TRUE)
        buckets <- buckets[1:n_values]

        # choose rounder values for the bucket breaks
        diff_breaks <- buckets[2:n_values] - buckets[1:(n_values - 1)]
        digits <- ifelse(buckets == 0, 0, floor(log10(abs(buckets))))
        digits_diff <- floor(log10(abs(diff_breaks)))
        rounding_lvl <- digits - c(0, digits[2:n_values] - digits_diff)
        buckets <- 10^(rounding_lvl)*floor(buckets/10^(rounding_lvl))

      }

      # format numbers nicely for legend
      buckets_formatted <- paste0(">= ", trimws(format(buckets, scientific = FALSE, big.mark = " ")))

      # bucketise data
      world[, plot_col] <- NA_character_
      for (i in 1:length(buckets)){
        world[, plot_col] <- ifelse(world[, paste0("original_", plot_col)] >= buckets[i],
                                    buckets_formatted[i],
                                    world[, plot_col])
      }

      # transform into a factor variable
      world[, plot_col] <- factor(world[, plot_col], levels = buckets_formatted)

    } else if (is.numeric(values) & n_values <= 6)(

      # transform into a factor variable
      world[, plot_col] <- as.factor(world[, plot_col])

    )

  }




  # MAKE BASE PLOT ----------------------------

  # produce base plot
  p <- ggplot(world, aes(x = .data$long, y = .data$lat, group = .data$group, fill = .data[[plot_col]], linetype = is.na(.data[[plot_col]])))+
    geom_polygon(colour = col_border, linewidth = width_border)+
    scale_linetype_manual(guide = "none", values = c("solid", "dotted"))+
    labs(x="", y="", fill = plot_col)+
    coord_fixed(xlim = zoom_plot[1:2], ylim = zoom_plot[3:4]) # Allows zoom while plotting points outside window (like coord_cartesian). coord fixed also maintains the ratio to avoid distortions

  # NAME OF VARIABLE IN LEGEND ---------------

  # if provided, change name of variable in legend
  if (!is.null(name_legend)){
    p <- p + labs(fill = name_legend)
  } else {
    p <- p + labs(fill = plot_col_original)
  }


  # APPLY THEMES ------------------------------



  # apply selected theme
  if (theme %in% theme_numbers | theme %in% theme_names){
    p <-  p + themes_countries(theme = theme) +
      scale_fill_manual(values = palettes_countries(n = n_values, theme = theme, reverse = reverse_palette),
                        na.value = col_na)
  }


  # SAVE MAP ------------------------------

  if (!is.null(save_to)){

    # if not ".png" or ".jpeg" at the end of path, add it
    if(!grepl(pattern = "\\.(png)|(jpg)|(jpeg)|(svg)$", x = save_to, perl = TRUE)) save_to <- paste0(save_to, ".png")

    # Find suitable ratio for plot
    legend_on_side <- if(theme == 0) { TRUE } else { if (themes_countries(theme)$legend.position %in% c("right", "left")) TRUE else FALSE}
    print_ratio <- ifelse(legend_on_side, 0.95 , 1.3) * (zoom_plot[4] - zoom_plot[3])/(zoom_plot[2] - zoom_plot[1])

    # save
    ggsave(plot = p, filename = save_to, width = width_plot, height = width_plot * print_ratio, units = "cm", dpi = 1000)

    if (verbose) message(paste0("Map saved to: ", save_to))
  }



  return(p)
}
