#' Geocode an address using tidygeocoder
#'
#' This function takes a street address and returns latitude and longitude using the Nominatim API.
#'
#' @param address A character string of a single address.
#'
#' @return A tibble with latitude and longitude columns.
#' @examples
#' geocode_address("10 Downing Street, London")
#' @export
geocode_address <- function(address) {
  if (!requireNamespace("tidygeocoder", quietly = TRUE)) {
    stop("The package 'tidygeocoder' is required. Please install it using install.packages('tidygeocoder').")
  }

  input_df <- data.frame(address = address)
  tidygeocoder::geocode(.tbl = input_df, address = address, method = "osm", verbose = FALSE)
}

