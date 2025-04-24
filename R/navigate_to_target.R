#' Navigate to a green or blue space based on user preference
#'
#' Prioritizes either the closest or largest destination within a given travel zone,
#' calculates the route to it, and returns both the route and its travel distance.
#'
#' @param location A tibble with `lat` and `lon` columns (from geocoding)
#' @param zone An `sf` POLYGON representing the travel zone
#' @param greens An `sf` object of green space geometries (e.g. parks)
#' @param blues An `sf` object of blue space geometries (e.g. rivers)
#' @param target_type One of `"green"` or `"blue"` to select destination type
#' @param preference One of `"closest"` or `"largest"` to define selection criteria
#'
#' @return A list with two elements:
#' \describe{
#'   \item{route}{An `sf` LINESTRING object representing the road network route}
#'   \item{length_m}{A numeric value giving the route length in meters}
#' }
#' @examples
#' location <- geocode_address("Kiliansplatz, Wuerzburg")
#' zone <- travel_zone(location, 5)
#' \dontrun{
#' # assuming you have loaded `greens` and `blues` as sf objects
#' result <- navigate_to_target(location, zone, greens, blues, target_type = "green", preference = "closest")
#' }
#'
#' @export
navigate_to_target <- function(location, zone, greens, blues, target_type = "green", preference = "closest") {
  if (!requireNamespace("osrm", quietly = TRUE)) stop("Package 'osrm' is required.")
  if (!requireNamespace("sf", quietly = TRUE)) stop("Package 'sf' is required.")

  # Only use relevant destination types to avoid unnecessary checks or filtering later
  target_sf <- switch(target_type,
                      green = greens,
                      blue = blues,
                      stop("Invalid target_type: must be 'green' or 'blue'"))

  # Exit early if there’s nothing to navigate to
  if (is.null(target_sf) || nrow(target_sf) == 0) {
    message("No features of selected type available.")
    return(NULL)
  }

  # Choose destination feature based on spatial logic (closest or largest)
  target_geom <- switch(preference,
                        closest = {
                          # Avoid attribute warnings by working only with geometry
                          target_centroids <- sf::st_centroid(sf::st_geometry(target_sf))
                          origin_pt <- sf::st_sfc(sf::st_point(c(location$lon, location$lat)), crs = 4326)
                          dists <- sf::st_distance(target_centroids, origin_pt)
                          target_sf[which.min(dists), ]
                        },
                        largest = {
                          # Larger areas often reflect more significant or accessible destinations
                          areas <- sf::st_area(target_sf)
                          target_sf[which.max(areas), ]
                        },
                        stop("Invalid preference: must be 'closest' or 'largest'")
  )

  # OSRM expects plain data frames for source and destination
  src <- data.frame(lon = location$lon, lat = location$lat)
  rownames(src) <- "start"

  dst_pt <- sf::st_centroid(sf::st_geometry(target_geom))
  dst_coords <- sf::st_coordinates(dst_pt)
  dst <- data.frame(lon = dst_coords[1], lat = dst_coords[2])
  rownames(dst) <- "target"

  # Force car profile since public OSRM only supports it
  options(osrm.server = "https://router.project-osrm.org/")
  options(osrm.profile = "car")

  # Route computation based on shortest path along road network
  route <- osrm::osrmRoute(src = src, dst = dst)

  # Reprojection of route to CRS that uses meters
  reprojected_route <- sf::st_transform(route, 3857)

  length_m <- sf::st_length(reprojected_route)
  route_length <- as.numeric(length_m)


  list(route = route, length_m = route_length)
}

#' Calculate and save the length of a route
#'
#' @param route An sf LINESTRING object representing a route, from "navigate_to_target()"
#'
#' @return The length of the route in a numeric unit (meters)
#' @examples
#' location <- geocode_address("Kiliansplatz, Wuerzburg")
#' zone <- travel_zone(location, 5)
#' \dontrun{
#' result <- navigate_to_target(location, zone, greens, blues)
#' route_length(result$route)
#' }
#'
#' @export
route_length <- function(route){
  # Reprojection of CRS in case still using degrees/mins
  reprojected_route <- sf::st_transform(route, 3857)

  length_m <- sf::st_length(reprojected_route)

  # Saving as.numeric to use for labelling/plotting
  return(as.numeric(length_m))
}
