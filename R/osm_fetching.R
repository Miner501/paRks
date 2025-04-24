#' Clean geometries safely in projected space
#'
#' Projects to EPSG:3857, repairs, then transforms back to EPSG:4326
#' @param x An sf object
#' @return A cleaned sf object
clean_geometry <- function(x) {
  if (nrow(x) == 0 || is.null(x)) return(x)

  # Transform to planar coordinates for reliable geometry operations
  x <- sf::st_transform(x, 3857)

  # Use different repair strategies depending on shape type
  geom_type <- unique(sf::st_geometry_type(x))

  if (any(geom_type %in% c("POLYGON", "MULTIPOLYGON"))) {
    x <- sf::st_make_valid(x)
    x <- sf::st_set_precision(x, 1e6)
    x <- sf::st_buffer(x, dist = 0)
  } else if (any(geom_type %in% c("LINESTRING", "MULTILINESTRING"))) {
    # Buffering lines can distort them, skip that step for line features
    x <- sf::st_set_precision(x, 1e6)
    x <- sf::st_make_valid(x)
  }

  # Return to geographic coordinates for compatibility with mapping functions
  sf::st_transform(x, 4326)
}

#' Get green spaces within a travel zone
#'
#' Returns parks, gardens, forests, meadows, and other green spaces within the travel zone.
#'
#' @param zone An `sf` polygon (from `travel_zone()`)
#' @param mode "standard" or "broad"
#' @param min_area_ha Minimum area in hectares (optional)
#' @return An `sf` object of green polygons intersecting the zone
#' @examples
#' \dontrun{
#' location <- geocode_address("Kiliansplatz, Wuerzburg")
#' zone <- travel_zone(location, 5)
#' greens <- get_greenspaces(zone, mode = "standard")
#' }
#'
#' @export
get_greenspaces <- function(zone, mode = "standard", min_area_ha = NULL) {
  if (!requireNamespace("osmdata", quietly = TRUE)) stop("Package 'osmdata' is required.")

  bbox <- sf::st_bbox(zone)

  # Ensures geometries are valid and clipped to the travel zone (avoids rendering issues and keeps plot focused)
  safe_filter <- function(sfobj) {
    sfobj <- clean_geometry(sfobj)
    if (sf::st_crs(sfobj) != sf::st_crs(zone)) {
      zone <- sf::st_transform(zone, sf::st_crs(sfobj))
    }
    idx <- sf::st_intersects(sfobj, zone, sparse = FALSE)
    sfobj[apply(idx, 1, any), ]
  }

  # Chosen to reflect commonly accessible recreational spaces
  if (mode == "standard") {
    query <- osmdata::opq(bbox = bbox) |>
      osmdata::add_osm_feature(key = "leisure", value = c("park", "garden", "recreation_ground"))
    result <- osmdata::osmdata_sf(query)
    greens <- result$osm_polygons
  }
    # Captures a wider definition of green space, even if not all are publicly accessible
    else if (mode == "broad") {
    leisure_q <- osmdata::opq(bbox = bbox) |>
      osmdata::add_osm_feature(key = "leisure")
    leisure <- osmdata::osmdata_sf(leisure_q)$osm_polygons
    if (!is.null(leisure) && nrow(leisure) > 0) leisure <- clean_geometry(leisure)

    landuse_q <- osmdata::opq(bbox = bbox) |>
      osmdata::add_osm_feature(key = "landuse", value = c(
        "forest", "meadow", "grass", "village_green", "allotments", "flowerbed", "recreation_ground"
      ))
    landuse <- osmdata::osmdata_sf(landuse_q)$osm_polygons
    if (!is.null(landuse) && nrow(landuse) > 0) landuse <- clean_geometry(landuse)

    natural_q <- osmdata::opq(bbox = bbox) |>
      osmdata::add_osm_feature(key = "natural", value = c("wood", "scrub", "grassland"))
    natural <- osmdata::osmdata_sf(natural_q)$osm_polygons
    if (!is.null(natural) && nrow(natural) > 0) natural <- clean_geometry(natural)

    greens <- dplyr::bind_rows(
      list(leisure, landuse, natural)[sapply(list(leisure, landuse, natural), function(x) !is.null(x))]
    )
  } else {
    stop("Invalid mode. Use 'standard' or 'broad'.")
  }

  if (!is.null(greens) && nrow(greens) > 0) {
    # Discards green features that fall completely outside the travel zone
    greens <- safe_filter(greens)

    # Filter by area in hectares (optional)
    if (!is.null(min_area_ha)) {
      greens <- sf::st_transform(greens, 3857)  # Project for area calculation
      greens$area_m2 <- sf::st_area(greens)
      greens <- greens[as.numeric(greens$area_m2) >= (min_area_ha * 10000), ]
      greens <- sf::st_transform(greens, 4326)
    }

    message("Green features returned: ", nrow(greens))
    return(greens)
  } else {
    message("No green features found.")
    return(NULL)
  }
}


#' Get blue spaces within a travel zone
#'
#' Fetches and filters OpenStreetMap features related to natural water and waterway features
#' (e.g., rivers, streams, canals, lakes, wetlands) within a given travel zone.
#'
#' @param zone An `sf` polygon object representing the travel zone, typically returned by `travel_zone()`.
#'
#' @return An `sf` object containing blue space geometries (`LINESTRING` and/or `POLYGON`) that intersect the zone,
#'         or `NULL` if no such features are found.
#'
#'@examples
#' \dontrun{
#' location <- geocode_address("Kiliansplatz, Wuerzburg")
#' zone <- travel_zone(location, 5)
#' blues <- get_bluespaces(zone)
#' }
#'
#' @importFrom sf st_bbox st_filter st_crs st_transform
#' @importFrom osmdata opq add_osm_feature osmdata_sf
#' @importFrom dplyr bind_rows
#'
#' @export
get_bluespaces <- function(zone) {
  if (!requireNamespace("osmdata", quietly = TRUE)) stop("Package 'osmdata' is required.")

  bbox <- sf::st_bbox(zone)

  # Different keys in OSM capture different types of water bodies, both are needed to be comprehensive
  query_natural <- osmdata::opq(bbox = bbox) |>
    osmdata::add_osm_feature(key = "natural", value = c("water", "wetland"))

  query_waterway <- osmdata::opq(bbox = bbox) |>
    osmdata::add_osm_feature(key = "waterway", value = c("river", "stream", "canal"))

  result_natural <- osmdata::osmdata_sf(query_natural)
  result_waterway <- osmdata::osmdata_sf(query_waterway)

  # Merge polygon and line geometries so both still water and flowing water are included
  polys <- dplyr::bind_rows(
    result_natural$osm_polygons,
    result_waterway$osm_polygons
  )
  lines <- dplyr::bind_rows(
    result_natural$osm_lines,
    result_waterway$osm_lines
  )

  # Limit result to features that actually overlap the travel zone
  safe_filter <- function(x) {
    x <- clean_geometry(x)
    if (sf::st_crs(x) != sf::st_crs(zone)) {
      zone <- sf::st_transform(zone, sf::st_crs(x))
    }
    sf::st_filter(x, zone, .predicate = sf::st_intersects)
  }

  polys <- if (!is.null(polys) && nrow(polys) > 0) safe_filter(polys) else NULL
  lines <- if (!is.null(lines) && nrow(lines) > 0) safe_filter(lines) else NULL

  if (!is.null(polys) && !is.null(lines)) {
    blues <- dplyr::bind_rows(polys, lines)
  } else if (!is.null(polys)) {
    blues <- polys
  } else if (!is.null(lines)) {
    blues <- lines
  } else {
    message("No blue features found.")
    return(NULL)
  }

  message("Blue features returned: ", nrow(blues))
  return(blues)
}



#' Get road network within a travel zone
#'
#' Returns roads (highways) that intersect a travel zone.
#'
#' @param zone An `sf` polygon from `travel_zone()`
#' @return An `sf` object of road lines or NULL if none found
#' @examples
#' \dontrun{
#' location <- geocode_address("Kiliansplatz, Wuerzburg")
#' zone <- travel_zone(location, 5)
#' roads <- get_roads(zone)
#' }
#' @export
get_roads <- function(zone) {
  if (!requireNamespace("osmdata", quietly = TRUE)) stop("Package 'osmdata' is required.")

  # Limit OSM query to zone extent to avoid fetching unnecessary road data
  bbox <- sf::st_bbox(zone)

  # Fetches all types of roads/highways defined in OSM and provides network context for routing and visuals
  query <- osmdata::opq(bbox = bbox) |>
    osmdata::add_osm_feature(key = "highway")

  result <- osmdata::osmdata_sf(query)
  roads <- result$osm_lines

  if (!is.null(roads) && nrow(roads) > 0) {
    # Ensure roads are spatially valid to prevent rendering or analysis errors
    roads <- clean_geometry(roads)

    # Coordinate systems must match for spatial filtering to work
    if (sf::st_crs(roads) != sf::st_crs(zone)) {
      zone <- sf::st_transform(zone, sf::st_crs(roads))
    }

    # Only keep road segments that actually intersect the travel zone
    roads <- sf::st_filter(roads, zone, .predicate = sf::st_intersects)
    message("Road features returned: ", nrow(roads))
    return(roads)
  } else {
    message("No road features found.")
    return(NULL)
  }
}
