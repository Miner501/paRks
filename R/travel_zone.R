#' Generate a travel zone polygon from a location
#'
#' Returns a polygon showing the area you can reach from a starting location,
#' based on travel time or distance, using the `osrm` package.
#'
#' @param location A tibble from geocode_address() or geocode_coords(), containing lat and lon columns.
#' @param mode Travel mode: "walk", "bike", or "car".
#' @param distance_km Distance in kilometers (optional if time_min is given).
#' @param time_min Travel time in minutes (optional if distance_km is given).
#'
#' @return An `sf` polygon of the travel zone.
#' @examples
#' loc <- geocode_address("Würzburg")
#' travel_zone(loc, mode = "walk", distance_km = 1)
#' @export

# travel_zone <- function(location, mode = "walk", distance_km = NULL, time_min = NULL) {
#   if (!requireNamespace("sf", quietly = TRUE)) stop("Package 'sf' is required.")
#   if (!requireNamespace("osrm", quietly = TRUE)) stop("Package 'osrm' is required.")
#
#   lat <- location$lat
#   lon <- location$lon
#
#   if (is.null(lat) || is.null(lon) || is.na(lat) || is.na(lon)) {
#     stop("Latitude and/or longitude are missing or invalid.")
#   }
#
#   # Base travel speed assumptions (used only for conversions)
#   speed_kph <- switch(mode,
#                       walk = 6.25,
#                       bike = 15,
#                       car = 50,
#                       stop("Invalid mode.")
#   )
#
#   if (!is.null(distance_km) && is.null(time_min)) {
#     time_min <- distance_km / speed_kph * 60
#   } else if (!is.null(time_min) && is.null(distance_km)) {
#     # Convert travel time at user mode to equivalent car driving time
#     car_equivalent_time <- time_min * (speed_kph / 50)
#     time_min <- car_equivalent_time
#   } else if (is.null(distance_km) && is.null(time_min)) {
#     stop("Please provide either distance_km or time_min.")
#   }
#
#   loc_df <- data.frame(lon = lon, lat = lat)
#   rownames(loc_df) <- "start"
#
#   # Use only the car profile (OSRM limitation)
#   options(osrm.server = "https://router.project-osrm.org/")
#   options(osrm.profile = "car")
#
#   iso <- osrm::osrmIsochrone(loc = loc_df, breaks = c(0, time_min), res = 40)
#   iso <- sf::st_make_valid(iso)
#
#   return(iso)
# }



# the old version

#' #' Generate a travel zone polygon from a location
#' #'
#' #' Returns a polygon showing the area you can reach from a starting location,
#' #' based on travel time or distance, using the `osrm` package.
#' #'
#' #' @param location A tibble from geocode_address() or geocode_coords(), containing lat and lon columns.
#' #' @param mode Travel mode: "walk", "bike", or "car".
#' #' @param distance_km Distance in kilometers (optional if time_min is given).
#' #' @param time_min Travel time in minutes (optional if distance_km is given).
#' #'
#' #' @return An `sf` polygon of the travel zone.
#' #' @examples
#' #' loc <- geocode_address("Würzburg")
#' #' travel_zone(loc, mode = "walk", distance_km = 1)
#' #' @export
#'
#' travel_zone <- function(location, mode = "walk", distance_km = NULL, time_min = NULL) {
#'   if (!requireNamespace("sf", quietly = TRUE)) {
#'     stop("Package 'sf' is required. Please install it using install.packages('sf').")
#'   }
#'
#'   if (!requireNamespace("osrm", quietly = TRUE)) {
#'     stop("Package 'osrm' is required. Please install it using install.packages('osrm').")
#'   }
#'
#'   # Extract coordinates
#'   lat <- location$lat
#'   lon <- location$lon
#'
#'   # Validate coordinates
#'   if (is.null(lat) || is.null(lon) || is.na(lat) || is.na(lon)) {
#'     stop("Latitude and/or longitude are missing or invalid. Check geocode_address() output.")
#'   }
#'
#'   # Travel speed lookup
#'   speed_kph <- switch(mode,
#'                       walk = 5,
#'                       bike = 15,
#'                       car = 50,
#'                       stop("Invalid mode. Use 'walk', 'bike', or 'car'."))
#'
#'   # Convert between distance and time
#'   if (!is.null(distance_km) && is.null(time_min)) {
#'     time_min <- distance_km / speed_kph * 60
#'   } else if (!is.null(time_min) && is.null(distance_km)) {
#'     distance_km <- time_min / 60 * speed_kph
#'   } else if (is.null(distance_km) && is.null(time_min)) {
#'     stop("Please provide either distance_km or time_min.")
#'   }
#'
#'   # Prepare input for osrmIsochrone
#'   loc_df <- data.frame(lon = lon, lat = lat)
#'   rownames(loc_df) <- "start"
#'   print(loc_df)
#'
#'   # Run isochrone
#'   iso <- osrm::osrmIsochrone(
#'     loc = loc_df,
#'     breaks = c(0, time_min),
#'     res = 40
#'   )
#'
#'   return(iso)
#' }







#' #' Generate a travel zone polygon from a location
#' #'
#' #' Returns a polygon showing the area you can reach from a starting location,
#' #' based on travel time or distance, using the OSRM (car-only) profile.
#' #'
#' #' @param location A tibble from geocode_address() or geocode_coords(), containing lat and lon columns.
#' #' @param mode Travel mode: "walk", "bike", or "car".
#' #' @param distance_km Distance in kilometers (optional if time_min is given).
#' #' @param time_min Travel time in minutes (optional if distance_km is given).
#' #'
#' #' @return An `sf` polygon of the travel zone.
#' #' @examples
#' #' loc <- geocode_address("Würzburg")
#' #' travel_zone(loc, mode = "walk", distance_km = 1)
#' #' @export
#' #' Generate a travel zone polygon from a location
#' #'
#' #' Returns a polygon showing the area you can reach from a starting location,
#' #' based on travel time or distance, using the `osrm` package.
#' #'
#' #' Uses a precise circular buffer for small distances (<2 km),
#' #' and falls back to OSRM routing for longer or time-based ranges.
#' #'
#' #' @param location A tibble from geocode_address() or geocode_coords(), containing lat and lon columns.
#' #' @param mode Travel mode: "walk", "bike", or "car".
#' #' @param distance_km Distance in kilometers (optional if time_min is given).
#' #' @param time_min Travel time in minutes (optional if distance_km is given).
#' #'
#' #' @return An `sf` polygon of the travel zone.
#' #' @export
#' travel_zone <- function(location, mode = "walk", distance_km = NULL, time_min = NULL) {
#'   if (!requireNamespace("sf", quietly = TRUE)) stop("Package 'sf' is required.")
#'   if (!requireNamespace("osrm", quietly = TRUE)) stop("Package 'osrm' is required.")
#'
#'   lat <- location$lat
#'   lon <- location$lon
#'
#'   if (is.null(lat) || is.null(lon) || is.na(lat) || is.na(lon)) {
#'     stop("Latitude and/or longitude are missing or invalid.")
#'   }
#'
#'   speed_kph <- switch(mode,
#'                       walk = 5,
#'                       bike = 15,
#'                       car = 50,
#'                       stop("Invalid mode: must be 'walk', 'bike', or 'car'")
#'   )
#'
#'   if (!is.null(distance_km) && is.null(time_min)) {
#'     time_min <- distance_km / speed_kph * 60
#'   } else if (!is.null(time_min) && is.null(distance_km)) {
#'     distance_km <- time_min / 60 * speed_kph
#'   } else if (is.null(distance_km) && is.null(time_min)) {
#'     stop("Please provide either distance_km or time_min.")
#'   }
#'
#'   # Use circular buffer for small distances
#'   if (distance_km < 2) {
#'     origin <- sf::st_sfc(sf::st_point(c(lon, lat)), crs = 4326)
#'     buffer <- sf::st_transform(origin, 3857) |> sf::st_buffer(distance_km * 1000) |> sf::st_transform(4326)
#'     return(buffer)
#'   }
#'
#'   # OSRM setup
#'   options(osrm.server = "https://router.project-osrm.org/")
#'   options(osrm.profile = "car")
#'
#'   adjusted_time_min <- distance_km / 50 * 60
#'
#'   loc_df <- data.frame(lon = lon, lat = lat)
#'   rownames(loc_df) <- "start"
#'
#'   iso <- osrm::osrmIsochrone(loc = loc_df, breaks = c(0, adjusted_time_min), res = 40)
#'   iso <- sf::st_make_valid(iso)
#'   iso <- sf::st_union(iso) |> sf::st_make_valid()
#'
#'   # Drop islands: only keep largest polygon
#'   iso_parts <- sf::st_cast(iso, "POLYGON")
#'   areas <- sf::st_area(iso_parts)
#'   largest <- iso_parts[which.max(areas)]
#'
#'   return(largest)
#' }



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
