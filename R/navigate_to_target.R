#' Navigate to a green or blue space based on preference
#'
#' @param location A tibble with `lat` and `lon` columns (from geocoding)
#' @param zone An sf polygon representing the travel zone
#' @param greens sf object of green spaces
#' @param blues sf object of blue spaces
#' @param target_type "green" or "blue"
#' @param preference "closest" or "largest"
#'
#' @return An sf LINESTRING object representing the route, or NULL if not found
#' @export
navigate_to_target <- function(location, zone, greens, blues, target_type = "green", preference = "closest") {
  if (!requireNamespace("osrm", quietly = TRUE)) stop("Package 'osrm' is required.")
  if (!requireNamespace("sf", quietly = TRUE)) stop("Package 'sf' is required.")

  # Choose the relevant dataset based on the user’s destination type
  target_sf <- switch(target_type,
                      green = greens,
                      blue = blues,
                      stop("Invalid target_type: must be 'green' or 'blue'"))

  # Exit early if no destinations are available
  if (is.null(target_sf) || nrow(target_sf) == 0) {
    message("No features of selected type available.")
    return(NULL)
  }

  # Determine which feature the user wants to go to
  target_geom <- switch(preference,
                        closest = {
                          # Closest: spatially shortest option
                          target_centroids <- sf::st_centroid(target_sf)
                          origin_pt <- sf::st_sfc(sf::st_point(c(location$lon, location$lat)), crs = 4326)
                          dists <- sf::st_distance(target_centroids, origin_pt)
                          target_sf[which.min(dists), ]
                        },
                        largest = {
                          # Largest: more spacious or natural experience
                          areas <- sf::st_area(target_sf)
                          target_sf[which.max(areas), ]
                        },
                        stop("Invalid preference: must be 'closest' or 'largest'")
  )

  # Prepare routing input — OSRM expects data.frames, not sf
  src <- data.frame(lon = location$lon, lat = location$lat)
  rownames(src) <- "start"

  dst_pt <- sf::st_centroid(target_geom)
  dst_coords <- sf::st_coordinates(dst_pt)
  dst <- data.frame(lon = dst_coords[1], lat = dst_coords[2])
  rownames(dst) <- "target"

  # Use car routing — only supported profile on public server
  options(osrm.server = "https://router.project-osrm.org/")
  options(osrm.profile = "car")

  # Calculate route from origin to destination
  route <- osrm::osrmRoute(src = src, dst = dst, returnclass = "sf")

  return(route)
}

#' Calculate and save the length of a route
#'
#' @param route An sf LINESTRING object representing a route, from "navigate_to_target()"
#'
#' @return The length of the route in a numeric unit (meters)
#' @export
route_length <- function(route){
  #Reprojection of CRS in case still using degrees/mins
  reprojected_route <- sf::st_transform(route, 3857)

  length_m <- sf::st_length(reprojected_route)

  #Saving as.numeric to use for labelling/plotting
  return(as.numeric(length_m))
}
