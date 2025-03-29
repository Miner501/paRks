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

travel_zone <- function(location, mode = "walk", distance_km = NULL, time_min = NULL) {
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("Package 'sf' is required. Please install it using install.packages('sf').")
  }

  if (!requireNamespace("osrm", quietly = TRUE)) {
    stop("Package 'osrm' is required. Please install it using install.packages('osrm').")
  }

  # Extract coordinates
  lat <- location$lat
  lon <- location$lon

  # Validate coordinates
  if (is.null(lat) || is.null(lon) || is.na(lat) || is.na(lon)) {
    stop("Latitude and/or longitude are missing or invalid. Check geocode_address() output.")
  }

  # Travel speed lookup
  speed_kph <- switch(mode,
                      walk = 5,
                      bike = 15,
                      car = 50,
                      stop("Invalid mode. Use 'walk', 'bike', or 'car'."))

  # Convert between distance and time
  if (!is.null(distance_km) && is.null(time_min)) {
    time_min <- distance_km / speed_kph * 60
  } else if (!is.null(time_min) && is.null(distance_km)) {
    distance_km <- time_min / 60 * speed_kph
  } else if (is.null(distance_km) && is.null(time_min)) {
    stop("Please provide either distance_km or time_min.")
  }

  # Prepare input for osrmIsochrone
  loc_df <- data.frame(lon = lon, lat = lat)
  rownames(loc_df) <- "start"
  print(loc_df)

  # Run isochrone
  iso <- osrm::osrmIsochrone(
    loc = loc_df,
    breaks = c(0, time_min),
    res = 40
  )

  return(iso)
}
