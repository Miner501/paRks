#' Geocode an address using tidygeocoder
#'
#' This function takes a street address and returns latitude and longitude using the Nominatim API.
#'
#' @param address A character string of a single address. Special characters such as Umlauts or "ß" should work.
#'
#' @return A tibble with latitude and longitude columns.
#' @examples
#' location <- geocode_address("Kiliansplatz, Wuerzburg")
#' @export
geocode_address <- function(address) {
  if (!requireNamespace("tidygeocoder", quietly = TRUE)) {
    stop("The package 'tidygeocoder' is required. Please install it using install.packages('tidygeocoder').")
  }

  input_df <- data.frame(address = address)
  coords <- tidygeocoder::geocode(.tbl = input_df, address = address, method = "osm", verbose = FALSE)

  # "tidygeocoder" uses "long". Converted to "lon" instead, as that is used in "travel_zone.R".
  coords <- dplyr::rename(coords, lon = long)
  return(coords)
}


#' Convert coordinates into a geocoded point
#'
#' This function wraps a user-input pair of latitude and longitude into a named tibble.
#'
#' @param lat Latitude
#' @param lon Longitude
#' @return A tibble with lat and lon columns.
#' @examples
#' location <- geocode_coords(49.793, 9.932)
#' @export
geocode_coords <- function(lat, lon){
  if (!requireNamespace("tibble", quietly = TRUE)) {
    stop("The package 'tibble' is required. Please install it using install.packages('tibble').")
  }
  tibble::tibble(lat = lat, lon = lon)
}
