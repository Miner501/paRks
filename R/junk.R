# This is just a test script to see if the functions etc in the package run as expected.
# devtools::document()
# devtools::load_all()

#address <- geocode_address("Heinestraße 11, Würzburg")
#tz <- travel_zone(address, mode = "walk", distance_km = 1)
#plot(sf::st_geometry(tz))


# location <- geocode_address("Heinestraße 11, Würzburg")
# zone <- travel_zone(location, distance_km = 1)
#
# greens <- get_greenspaces(zone)
# blues  <- get_bluespaces(zone)
# roads  <- get_roads(area, zone)
#
# plot(sf::st_geometry(zone))
# plot(sf::st_geometry(greens), col = "green", add = TRUE)
# plot(sf::st_geometry(blues), col = "blue", add = TRUE)
# plot(sf::st_geometry(roads), col = "gray", add = TRUE)
#
#
#
#
# # Test greens without intersection
# test_bbox <- sf::st_bbox(zone)
#
# query <- osmdata::opq(bbox = test_bbox) |>
#   osmdata::add_osm_feature(key = "leisure", value = c("park", "garden", "recreation_ground"))
#
# result <- osmdata::osmdata_sf(query)
# greens_raw <- result$osm_polygons
#
# nrow(greens_raw)  # How many rows? > 0?
# plot(sf::st_geometry(greens_raw), col = "green")
#
#
#
# greens <- sf::st_make_valid(greens_raw)
# greens <- sf::st_set_precision(greens, 1e6)
# greens <- sf::st_buffer(greens, dist = 0)
# greens <- sf::st_filter(greens, zone, .predicate = sf::st_intersects)
# plot(sf::st_geometry(zone))
# plot(sf::st_geometry(greens), col = "blue", add = TRUE)


# Step 1: Geocode an address
location <- geocode_address("Heinestraße 11, Würzburg")
# Step 2: Create a travel zone (1km walk)
zone <- travel_zone(location, mode = "walk", distance_km = 1)

# Step 3: Fetch greenspaces, bluespaces, and roads
greens <- get_greenspaces(zone, mode = "broad")
blues  <- get_bluespaces(zone)
roads  <- get_roads("Würzburg", zone)
# Step 4: Plot everything
plot(sf::st_geometry(zone), main = "paRks Travel Zone", col = NA, border = "black")

if (!is.null(greens)) plot(sf::st_geometry(greens), col = "green", add = TRUE)
if (!is.null(blues))  plot(sf::st_geometry(blues), col = "blue", add = TRUE)
if (!is.null(roads))  plot(sf::st_geometry(roads), col = "gray", add = TRUE)
# #
#
# bbox <- sf::st_bbox(zone)
#
# query <- osmdata::opq(bbox = bbox) |>
#   osmdata::add_osm_feature(key = "landuse")
#
# result <- osmdata::osmdata_sf(query)
# landuse_raw <- result$osm_polygons
#
# nrow(landuse_raw)
# plot(sf::st_geometry(landuse_raw), col = "green")
# plot(sf::st_geometry(zone), border = "red", lwd = 2, add = TRUE)
