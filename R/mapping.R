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
    title = "Travel Zone Map"
) {
  # Always show the zone to give geographic context
  plot(sf::st_geometry(zone), col = NA, border = "red", lwd = 3, main = title)

  # Add green spaces if available and requested (for highlighting natural areas)
  if (show_greens == TRUE && !is.null(greens)) {
    plot(sf::st_geometry(greens), col = "green", add = TRUE)
  }

  # Add blue spaces, clipped to zone to avoid stretching the plot extent
  if (show_blues == TRUE && !is.null(blues)) {
    clipped_blues <- sf::st_intersection(blues, zone)
    plot(sf::st_geometry(clipped_blues), col = "blue", add = TRUE)
  }

  # Add roads, clipped to zone to maintain focus on the reachable area
  if (show_roads == TRUE && !is.null(roads)) {
    clipped_roads <- sf::st_intersection(roads, zone)
    plot(sf::st_geometry(clipped_roads), col = "grey", add = TRUE)
  }

  # Add route line if provided, to visualize navigation path
  if (show_route == TRUE && !is.null(route)) {
    plot(sf::st_geometry(route), col = "purple", lwd = 3, add = TRUE)
  }
}

