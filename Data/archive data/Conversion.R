library(sf)

# For KML to GeoJSON
data <- st_read("chennai_basin_waterbodies.kml")
st_write(data, "chennai_basin_waterbodies.geojson")

data <- st_read("gcc-divisions-latest.kml")
st_write(data, "gcc-divisions-latest.geojson")
