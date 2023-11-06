#' Internal function - return n spaced indices for picking colours from a palette
#'
#' @param n Desired Number of colours desired
#' @param n_colours_palette Total number of colours in the palette
#' @returns Returns n indices that can be used to select colours from a palette of n_colours_palette length
#' @seealso \link[countries]{palettes_countries}
#' @noRd
#' @keywords Internal
#' @examples
#' countries:::pick_colours(5, 10)
pick_colours <- function(n, n_colours_palette){

  if (n > n_colours_palette){
    warning("Not enough colours in the palette - colours will be repeated. Please try another theme or provide colours manually.")
    return(c(1:n_colours_palette, rep(1, n - n_colours_palette)))

  } else{

    if (n <= n_colours_palette / 2){
      index <- floor(seq(2, n_colours_palette - 1, length.out = n))
    } else {
      index <- floor(seq(1, n_colours_palette, length.out = n))
    }

    return(index)
  }

}



#' Discrete colour palettes
#'
#' This function provides access to the discrete colours palettes used in this packages' 11 themes.
#' @param n Number of desired colours
#' @param theme A numeric value or name identifying the theme's colours. Can be a number between 1 and 11, or one of the theme's names: \code{c("Default", "Greyscale", "Candy", "RedBlue", "Dark", "Reds", "Blues", "Greens", "Viridis", "Cividis", "Distinct", "Distinct2", "Paired")}.
#' @param reverse Logical value indicating whether to reverse the order of the palette's colours Default is FALSE.
#' @returns Returns n colours from the requested theme
#' @seealso \link[countries]{quick_map}
#' @export
#' @examples
#' palettes_countries(5, theme = 1)
palettes_countries <- function(n, theme = 1, reverse = FALSE){

  # perform checks on inputs
  if (!is.numeric(n)|length(n)!=1|is.na(n)) stop("invalid input for - n - it needs to be a number")
  if (is.null(theme)) stop("NULL is an invalid input for - theme - it needs to be one of the themes' names or an integer value")
  if (!is.atomic(theme)|is.logical(theme)|length(theme)>1) stop("invalid input for - theme - it needs to be a valid theme name or integer")
  if (!is.logical(reverse) | length(reverse)!=1) stop("Function argument - reverse - needs to be a logical statement (TRUE/FALSE)")
  if (is.na(reverse)|is.null(reverse)) stop("The argument - reverse - cannot be NA or NULL. It needs to be a logical value")

  # select colours from theme
  if (theme == 1 | theme == "Default"){
    colours <- c("#cb997e", "#ddbea9", "#ffe8d6", "#b7b7a4", "#888b75", "#545849")
    colours <- colours[pick_colours(n, length(colours))]

  } else if (theme == 2 | theme == "Greyscale"){
    colours <- paste0("grey", floor(seq(85, 15, length.out = n)))

  } else if (theme == 3 | theme == "Candy"){
    colours <- c("#207474", "#72AB8E", "#B1C7B3", "#F1EAC8", "#E5B9AD", "#D98994", "#D0587E")
    colours <- colours[pick_colours(n, length(colours))]

  } else if (theme == 4 | theme == "RedBlue"){
    colours <- c("#65010c","#cb1b16","#ef3c2d","#f26a4f","#f29479","#fedfd4","#9dcee2","#4091c9","#1368aa","#033270")
    colours <- colours[pick_colours(n, length(colours))]

  } else if (theme == 5 | theme == "Dark"){
    colours <- paste0("grey", ceiling(seq(20, 65, length.out = n)))

  } else if (theme == 6 | theme == "Reds"){
    reds <- grDevices::colorRampPalette(c("#F1EAC8", "#ba181b"))
    colours <- reds(n)

  } else if (theme == 7 | theme == "Blues"){
    blues <- grDevices::colorRampPalette(c("#caf0f8", "#03045e"))
    colours <- blues(n)

  } else if (theme == 8 | theme == "Greens"){
    greens <- grDevices::colorRampPalette(c("#e9f5db", "#31572c"))
    colours <- greens(n)

  } else if (theme == 9 | theme == "Viridis"){
    colours <- viridis::viridis(n)

  } else if (theme == 10 | theme == "Cividis"){
    colours <- viridis::cividis(n)

  } else if (theme == 11 | theme == "Distinct"){
    colours <- c('#e6194b', '#3cb44b', '#ffe119', '#4363d8', '#f58231', '#911eb4', '#46f0f0', '#f032e6', '#bcf60c', '#fabebe', '#008080', '#e6beff', '#9a6324', '#fffac8', '#800000', '#aaffc3', '#808000', '#ffd8b1', '#808080', '#000075')
    colours <- colours[pick_colours(n, length(colours))]

  } else if (theme == 12 | theme == "Distinct2"){
    colours <- c("grey20", "#6ea8ffff", "red","grey75", "#ffcccc","#ffe599", "#b6d7a8", "#5b6b54", "#b4a7d6")
    colours <- colours[pick_colours(n, length(colours))]

  } else if (theme == 13 | theme == "Paired"){
    colours <- c("#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99", "#E31A1C", "#FDBF6F" ,"#FF7F00" ,"#CAB2D6", "#6A3D9A", "#FFFF99", "#B15928")
    colours <- colours[pick_colours(n, length(colours))]

  } else {
    warning("The provided theme name or number is not available")
    colours <- NULL
  }

  # reverse colours if requested
  if (reverse == TRUE) colours <- rev(colours)

  # final output
  return(colours)
}






#' Internal function - ggplot themes for quick_map
#'
#' @param theme A numeric value or name identifying the theme's colours. Can be a number between 1 and 11, or one of the theme's names: \code{c("Default", "Greyscale", "Candy", "RedBlue", "Dark", "Reds", "Blues", "Greens", "Viridis", "Cividis", "Distinct")}.
#' @returns Returns n indices that can be used to select colours from a palette of n_colours_palette length
#' @seealso \link[countries]{quick_map}
#' @noRd
#' @keywords Internal
#' @examples
#' countries:::themes_countries(theme = 1)
themes_countries <- function(theme = 1){

  # base design
  output <- theme(plot.background = element_rect(fill = "transparent", colour = NA),
                        panel.border = element_blank(),
                        panel.background = element_rect(fill = "transparent", colour = NA),
                        legend.background = element_rect(fill = "transparent", colour = NA),
                        panel.grid = element_blank(),
                        axis.text = element_blank(),
                        axis.ticks = element_blank(),
                        legend.position = "bottom",
                        legend.box.margin = margin(0,0,0,0))

  # Add theme's customisations and colours
  if (theme == 0 | theme == "NoTheme"){
    output <- NULL

  } else if (theme == 1 | theme == "Default"){
    output <- output +
      theme(panel.background = element_rect(fill = "#D0E8FA", colour = "grey30"))

  } else if (theme == 2 | theme == "Greyscale"){
    output <- output +
      theme(legend.background = element_rect(fill = "transparent", colour = "black"))

  } else if (theme == 3 | theme == "Candy"){
    # ---

  } else if (theme == 4 | theme == "RedBlue"){
    output <- output +
      theme(plot.background = element_rect(fill = "#FFF5EC"),
            legend.position = "right")

  } else if (theme == 5 | theme == "Dark"){
      output <- output +
        theme(plot.background = element_rect(fill = "grey10"),
              legend.position = "right",
              legend.background = element_rect(fill = "grey10"),
              legend.text = element_text(color= "white"),
              legend.title = element_text(color = "white", face = "bold"),
              legend.key = element_rect(fill = "grey10", colour = "black"))

  } else if (theme == 6 | theme == "Reds"){
    output <- output +
      theme(legend.position = "right")

  } else if (theme == 7 | theme == "Blues"){
    output <- output +
      theme(panel.background = element_rect(fill = "#FFF5EC"),
            legend.position = "right")

  } else if (theme == 8 | theme == "Greens"){
    output <- output +
      theme(panel.background = element_rect(fill = "#D0E8FA"),
            legend.position = "right")

  } else if (theme %in% c(9, 10) | theme %in% c("Viridis", "Cividis")){
    output <- output +
      theme(legend.position = "right")

  } else if (theme == 11 | theme == "Distinct"){
    output <- output +
      theme(legend.background = element_rect(fill = "transparent", colour = "black"))

  } else if (theme == 12 | theme == "Distinct2"){
    output <- output +
      theme(legend.background = element_rect(fill = "transparent"))

  } else if (theme == 13 | theme == "Paired"){
    output <- output +
      theme(legend.background = element_rect(fill = "transparent", colour = "black"))

  } else {
    warning("The provided theme name or number does not exist")
  }

  return(output)
}
