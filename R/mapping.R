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
#' @param route_distance Numeric. Distance in meters to the destination, used in caption.
#' @param target_type String. Either "green" or "blue", describing type of target shown in caption.
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
#' @importFrom ggplot2 ggplot geom_sf aes scale_fill_manual scale_color_manual labs guide_legend theme unit guides
#' @importFrom ggspatial annotation_scale annotation_north_arrow north_arrow_fancy_orienteering
#'
#' @return A ggplot object showing the map with specified features.
#' @examples
#' location <- geocode_address("Kiliansplatz, Wuerzburg")
#' zone <- travel_zone(location, 5)
#' \dontrun{
#' result <- navigate_to_target(location, zone, greens, blues)
#' plot_travel_map_gg(
#'   zone = zone,
#'   greens = greens,
#'   blues = blues,
#'   roads = roads,
#'   route = result$route,
#'   route_distance = result$length_m,
#'   start_location = location,
#'   target_type = "green"
#' )
#' }
#'
#'
#' @export
plot_travel_map_gg <- function(
    zone,
    greens = NULL,
    blues = NULL,
    roads = NULL,
    route = NULL,
    route_distance = NULL,
    start_location = NULL,
    show_greens = TRUE,
    show_blues = TRUE,
    show_roads = TRUE,
    show_route = TRUE,
    title = "Travel Zone Map (ggplot2)",
    palette = "normal",
    target_type = NULL
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

  # Dynamic caption text that shows the route_distance as well
  if (!is.null(route_distance)) {
    caption_text <- paste0(
      "This map shows how far you are from a ", target_type,
      " space that can be used for recreational activities. You are ",
      round(route_distance), " meters away from the selected destination."
    )
  } else {
    caption_text <- NULL
  }

  # Initialize ggplot with zone layer
  # Using aes(color = ...) here creates a named group to trigger a legend entry
  p <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = zone, ggplot2::aes(color = "Zone"), fill = NA, linewidth = 1.2) +
    ggplot2::labs(title = title, caption = caption_text)

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

  return(p)
}

