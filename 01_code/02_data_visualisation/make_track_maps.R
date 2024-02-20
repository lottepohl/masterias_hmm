# Load necessary libraries
library(leaflet)
library(lubridate)

# Sample data: coordinates and time steps
coords <- matrix(c(
  -73.98513, 40.7589,
  -73.98508, 40.75886,
  -73.98488, 40.75884,
  -73.98456, 40.75884,
  -73.98439, 40.75886
), byrow = TRUE, ncol = 2)

timestamps <- ymd_hms(c(
  "2024-02-16 08:00:00",
  "2024-02-16 08:02:00",
  "2024-02-16 08:04:00",
  "2024-02-16 08:06:00",
  "2024-02-16 08:08:00"
))

# Create Leaflet map
map <- leaflet() %>%
  addTiles()

# Add segments with different colors based on time steps
for (i in 1:(length(coords)/2 - 1)) {
  color <- colorNumeric(
    palette = "plasma",
    domain = as.numeric(timestamps)
  )
  map <- map %>% addPolylines(
    lng = c(coords[i, 1], coords[i+1, 1]),
    lat = c(coords[i, 2], coords[i+1, 2]),
    color = color(as.numeric(timestamps[i])),
    opacity = 1
  )
}

# Print the map
print(map)
