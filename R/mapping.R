#' Plot a travel zone map with optional layers
#'
#' Plots the travel zone along with optional layers like green spaces, blue spaces, roads, and route.
#' Each layer can be toggled with a logical flag. Layers are clipped to the zone extent.
#'
#' @param zone An `sf` POLYGON object representing the travel zone.
#' @param greens Optional `sf` object of green space polygons.
#' @param blues Optional `sf` object of blue space polygons or lines.
#' @param roads Optional `sf` object of road geometries (usually lines).
#' @param route Optional `sf` LINESTRING representing the path to a destination.
#' @param show_greens Logical. Should green spaces be plotted? Default `TRUE`.
#' @param show_blues Logical. Should blue spaces be plotted? Default `TRUE`.
#' @param show_roads Logical. Should roads be plotted? Default `TRUE`.
#' @param show_route Logical. Should the travel route be plotted? Default `TRUE`.
#' @param title Title to show at the top of the plot. Default is "Travel Zone Map".
#' @param palette Default is "normal", "deuteranopia", "protanopia", "tritanopia" are also possible.
#'
#' @return A base R plot showing the specified layers.
#' @export

plot_travel_map <- function(
    zone,
    greens = NULL,
    blues = NULL,
    roads = NULL,
    route = NULL,
    show_greens = TRUE,
    show_blues = TRUE,
    show_roads = TRUE,
    show_route = TRUE,
    title = "Travel Zone Map",
    palette = "normal"
) {
  # Added a fallback in case input palette is incorrect
  if (!(palette %in% c("normal", "deuteranopia", "protanopia", "tritanopia"))) {
    warning("Invalid palette selected. Falling back to 'normal'.")
    palette <- "normal"
  }
  # Set color values depending on the palette
  if (palette == "normal") {
    color_greens <- "green"
    color_blues  <- "blue"
    color_roads  <- "grey"
    color_route  <- "purple"
    color_zone   <- "red"
  }
  # Set color values for Deuteranopia
  if (palette == "deuteranopia"){
    color_greens <- "#009E73"
    color_blues <- "#56B4E9"
    color_roads <- "grey"
    color_route <- "#CC79A7"
    color_zone <- "#E69F00"
  }
  # Set color values for Protanopia
  if (palette == "protanopia"){
    color_greens <- "#009E73"
    color_blues <- "#0072B2"
    color_roads <- "grey"
    color_route <- "#E69F00"
    color_zone <- "#D55E00"
  }
  # Set color values for Tritanopia
  if (palette == "tritanopia"){
    color_greens <- "#009E73"
    color_blues <- "#E69F00"
    color_roads <- "grey"
    color_route <- "#0072B2"
    color_zone <- "#D55E00"
  }



  # Plot base zone for context
  plot(sf::st_geometry(zone), col = NA, border = color_zone, lwd = 3, main = title)

  if (show_greens && !is.null(greens)) {
    plot(sf::st_geometry(greens), col = color_greens, add = TRUE)
  }

  if (show_blues && !is.null(blues)) {
    clipped_blues <- sf::st_intersection(blues, zone)
    plot(sf::st_geometry(clipped_blues), col = color_blues, add = TRUE)
  }

  if (show_roads && !is.null(roads)) {
    clipped_roads <- sf::st_intersection(roads, zone)
    plot(sf::st_geometry(clipped_roads), col = color_roads, add = TRUE)
  }

  if (show_route && !is.null(route)) {
    plot(sf::st_geometry(route), col = color_route, lwd = 3, add = TRUE)
  }
}

