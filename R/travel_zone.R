#' Generate a travel zone polygon based on distance
#'
#' Returns a polygon representing the area reachable from a given location,
#' based on a straight-line buffer (for short distances) or an OSRM-based isochrone (for longer ones).
#'
#' @param location A tibble from geocode_address() or geocode_coords(), with `lat` and `lon` columns.
#' @param distance_km Travel distance in kilometers.
#'
#' @return An `sf` polygon of the travel zone.
#' @export
travel_zone <- function(location, distance_km) {
  if (!requireNamespace("sf", quietly = TRUE)) stop("Package 'sf' is required.")
  if (!requireNamespace("osrm", quietly = TRUE)) stop("Package 'osrm' is required.")

  # Validate coordinates
  lat <- location$lat
  lon <- location$lon
  if (is.null(lat) || is.null(lon) || is.na(lat) || is.na(lon)) {
    stop("Latitude and/or longitude are missing or invalid.")
  }

  # Use circular buffer for small distances (< 2 km)
  if (distance_km < 2) {
    origin <- sf::st_sfc(sf::st_point(c(lon, lat)), crs = 4326)
    buffer <- sf::st_transform(origin, 3857) |>
      sf::st_buffer(distance_km * 1000) |>
      sf::st_transform(4326)
    return(buffer)
  }

  # Set up OSRM routing (car profile only)
  options(osrm.server = "https://router.project-osrm.org/")
  options(osrm.profile = "car")

  # Estimate travel time at car speed (50 kph)
  adjusted_time_min <- distance_km / 50 * 60

  # Run isochrone query
  loc_df <- data.frame(lon = lon, lat = lat)
  rownames(loc_df) <- "start"

  iso <- osrm::osrmIsochrone(loc = loc_df, breaks = c(0, adjusted_time_min), res = 40)
  iso <- sf::st_make_valid(iso)
  iso <- sf::st_union(iso) |> sf::st_make_valid()

  # Keep only the largest polygon (drop islands)
  iso_parts <- sf::st_cast(iso, "POLYGON")
  largest <- iso_parts[which.max(sf::st_area(iso_parts))]

  return(largest)
}
