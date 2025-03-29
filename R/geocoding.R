#' Geocode an address using tidygeocoder
#'
#' This function takes a street address and returns latitude and longitude using the Nominatim API.
#'
#' @param address A character string of a single address. Special characters such as Umlauts or "ß" should work.
#'
#' @return A tibble with latitude and longitude columns.
#' @examples
#' geocode_address("John-Skilton-Straße 4a, 97074 Würzburg")
#' @export
geocode_address <- function(address) {
  if (!requireNamespace("tidygeocoder", quietly = TRUE)) {
    stop("The package 'tidygeocoder' is required. Please install it using install.packages('tidygeocoder').")
  }

  input_df <- data.frame(address = address)
  tidygeocoder::geocode(.tbl = input_df, address = address, method = "osm", verbose = FALSE)
}


#' Convert coordinates into a geocoded point
#'
#' This function wraps a user-input pair of latitude and longitude into a named tibble.
#'
#' @param lat Latitude
#' @param lon Longitude
#' @return A tibble with lat and lon columns.
#' @examples
#' geocode_coords(49.7873, 9.9786)
#' @export
geocode_coords <- function(lat, lon){
  if (!requireNamespace("tibble", quietly = TRUE)) {
    stop("The package 'tibble' is required. Please install it using install.packages('tibble').")
  }
  tibble::tibble(lat = lat, lon = lon)
}
