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


  # Empty vectors containing legend item as well as color information
  legend_items <- c()
  legend_colors <- c()

  # Plot base zone for context
  plot(sf::st_geometry(zone), col = NA, border = color_zone, lwd = 3, main = title)

  if (show_greens && !is.null(greens)){
    legend_items <- c(legend_items, "Green Spaces")
    legend_colors <- c(legend_colors, color_greens)
    plot(sf::st_geometry(greens), col = color_greens, add = TRUE)
  }

  if (show_blues && !is.null(blues)) {
    legend_items <- c(legend_items, "Blue Spaces")
    legend_colors <- c(legend_colors, color_blues)
    clipped_blues <- sf::st_intersection(blues, zone)
    plot(sf::st_geometry(clipped_blues), col = color_blues, add = TRUE)
  }

  if (show_roads && !is.null(roads)) {
    legend_items <- c(legend_items, "Roads")
    legend_colors <- c(legend_colors, color_roads)
    clipped_roads <- sf::st_intersection(roads, zone)
    plot(sf::st_geometry(clipped_roads), col = color_roads, add = TRUE)
  }

  if (show_route && !is.null(route)) {
    legend_items <- c(legend_items, "Route")
    legend_colors <- c(legend_colors, color_route)
    plot(sf::st_geometry(route), col = color_route, lwd = 3, add = TRUE)
  }

  # Making the map look pretty
  legend("bottomright", legend = legend_items, fill = legend_colors, border = "black", cex = 0.8, bg = "white", title = "Legend")
  mapsf::mf_scale(pos = "bottomleft", lwd = 1.5)
  mapsf::mf_arrow(pos = "topright", cex = 1.5)

}

# Define color palettes for different vision types
palette_colors <- list(
  normal = list(
    fill = c("Green Spaces" = "green", "Blue Spaces" = "blue"),
    color = c("Roads" = "grey", "Route" = "purple", "Zone" = "red", "Start" = "black")
  ),
  deuteranopia = list(
    fill = c("Green Spaces" = "#009E73", "Blue Spaces" = "#56B4E9"),
    color = c("Roads" = "grey", "Route" = "#CC79A7", "Zone" = "#E69F00", "Start" = "black")
  ),
  protanopia = list(
    fill = c("Green Spaces" = "#009E73", "Blue Spaces" = "#0072B2"),
    color = c("Roads" = "grey", "Route" = "#E69F00", "Zone" = "#D55E00", "Start" = "black")
  ),
  tritanopia = list(
    fill = c("Green Spaces" = "#009E73", "Blue Spaces" = "#E69F00"),
    color = c("Roads" = "grey", "Route" = "#0072B2", "Zone" = "#D55E00", "Start" = "black")
  )
)


#' Plot travel map using ggplot2
#'
#' Replaces base R plotting with ggplot2 for better styling and customization.
#' Allows for visualizing green/blue spaces, roads, routes, and the travel zone itself.
#'
#' @param zone An `sf` POLYGON object representing the travel zone.
#' @param greens Optional `sf` object of green space polygons (parks, gardens, etc.).
#' @param blues Optional `sf` object of blue space polygons or lines (rivers, lakes, etc.).
#' @param roads Optional `sf` object of road geometries (LINESTRING).
#' @param route Optional `sf` LINESTRING representing the route to a target.
#' @param start_location A tibble with `lat` and `lon` columns representing the geocoded start point.
#' @param show_greens Logical. If `TRUE`, green spaces are added to the map. Default is `TRUE`.
#' @param show_blues Logical. If `TRUE`, blue spaces are added to the map. Default is `TRUE`.
#' @param show_roads Logical. If `TRUE`, roads are added to the map. Default is `TRUE`.
#' @param show_route Logical. If `TRUE`, the route is added to the map. Default is `TRUE`.
#' @param title Title to be displayed at the top of the plot. Default is `"Travel Zone Map (ggplot2)"`.
#' @param palette Name of the color palette to use. Options: `"normal"`, `"deuteranopia"`, `"protanopia"`, `"tritanopia"`.
#'
#' @return A ggplot object showing the map with specified features.
#' @export
plot_travel_map_gg <- function(
    zone,
    greens = NULL,
    blues = NULL,
    roads = NULL,
    route = NULL,
    start_location = NULL,
    show_greens = TRUE,
    show_blues = TRUE,
    show_roads = TRUE,
    show_route = TRUE,
    title = "Travel Zone Map (ggplot2)",
    palette = "normal"
) {
  # Required packages
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("ggplot2 is required.")
  if (!requireNamespace("sf", quietly = TRUE)) stop("sf is required.")

  # Palette setup
  if (!(palette %in% names(palette_colors))) {
    warning("Invalid palette selected. Falling back to 'normal'.")
    palette <- "normal"
  }

  # Use centralized color palette definitions
  color_greens <- as.character(palette_colors[[palette]]$fill["Green Spaces"])
  color_blues  <- as.character(palette_colors[[palette]]$fill["Blue Spaces"])
  color_roads  <- as.character(palette_colors[[palette]]$color["Roads"])
  color_route  <- as.character(palette_colors[[palette]]$color["Route"])
  color_zone   <- as.character(palette_colors[[palette]]$color["Zone"])
  color_start  <- as.character(palette_colors[[palette]]$color["Start"])

  # Convert start_location tibble to sf point if provided
  if (!is.null(start_location)) {
    if (!all(c("lat", "lon") %in% names(start_location))) {
      stop("start_location must include 'lat' and 'lon' columns.")
    }
    start_sf <- sf::st_as_sf(start_location, coords = c("lon", "lat"), crs = 4326)
  }

  # Initialize ggplot with zone layer
  # Using aes(color = ...) here creates a named group to trigger a legend entry
  p <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = zone, ggplot2::aes(color = "Zone"), fill = NA, linewidth = 1.2) +
    ggplot2::ggtitle(title)

  # Optional layers as well as clipping of roads and blues to the zone
  if (show_greens && !is.null(greens)) {
    # Using fill = "..." mapped through aes to enable legend appearance for this layer
    p <- p + ggplot2::geom_sf(data = greens, ggplot2::aes(fill = "Green Spaces"), color = NA)
  }

  if (show_blues && !is.null(blues)) {
    # Clipping features to the zone boundary for visual clarity and consistency
    clipped_blues <- sf::st_intersection(blues, zone)
    p <- p + ggplot2::geom_sf(data = clipped_blues, ggplot2::aes(fill = "Blue Spaces"), color = NA)
  }

  if (show_roads && !is.null(roads)) {
    clipped_roads <- sf::st_intersection(roads, zone)
    # Color is mapped through aes for legend visibility
    p <- p + ggplot2::geom_sf(data = clipped_roads, ggplot2::aes(color = "Roads"), linewidth = 0.4)
  }

  if (show_route && !is.null(route)) {
    # Explicit linewidth for emphasis, and legend handled by aes color mapping
    p <- p + ggplot2::geom_sf(data = route, ggplot2::aes(color = "Route"), linewidth = 1.2)
  }

  if (!is.null(start_location)) {
    # Using shape=21 and fill=... mapped through aes gives visual styling and legend entry
    p <- p + ggplot2::geom_sf(
      data = start_sf,
      ggplot2::aes(fill = "Start"),
      shape = 21,
      color = "white",
      size = 3,
      stroke = 1
    )
  }

  # Add manual color/fill scales to ensure legend colors match the intended palette
  # These map the aesthetic names used in aes(...) to actual color values
  p <- p +
    ggplot2::scale_fill_manual(
      name = "Spaces & Start",
      values = c("Green Spaces" = color_greens,
                 "Blue Spaces"  = color_blues,
                 "Start"        = color_start),
      na.translate = FALSE
    ) +
    ggplot2::scale_color_manual(
      name = "AOI Features",
      values = c("Zone"  = color_zone,
                 "Roads" = color_roads,
                 "Route" = color_route),
      na.translate = FALSE
    )

  # Fixes Legend alignment issues
  p <- p +
    ggplot2::guides(
      color = ggplot2::guide_legend(order = 1),
      fill  = ggplot2::guide_legend(order = 1)
    ) +
    ggplot2::theme(legend.spacing.y = ggplot2::unit(0, "pt"))

  # Add north arrow and scale bar if available
  # ggspatial tools enhance orientation and spatial context on static maps
  if (requireNamespace("ggspatial", quietly = TRUE)) {
    p <- p +
      ggspatial::annotation_scale(location = "bl", width_hint = 0.3) +
      ggspatial::annotation_north_arrow(location = "tr", which_north = "true",
                                        style = ggspatial::north_arrow_fancy_orienteering)
  }

  # Enabling lat/lon grid on outer edge of map for spatial reference
  # this messes up and is very janky, try to fix in future
  #p <- p + ggplot2::coord_sf(datum = NA)

  return(p)
}

