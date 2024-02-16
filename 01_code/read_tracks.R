library(arrow)
library(leaflet)

# Replace the path with the path to your Parquet file
parquet_file_path <- paste0(getwd(), "/03_results_tracks/SN1293304/mean.parquet")
parquet_file_path
# parquet_file_path <- "path/to/your/file.parquet"

# Read the Parquet file
SN1293304_mean_track <- arrow::read_parquet(parquet_file_path)
SN1293304_mean_track_old <- mustelus_all_tracks %>% dplyr::filter(tag_serial_number == "1293304")

col_pal <- leaflet::colorNumeric(palette = "inferno", domain = SN1293304_mean_track$time)
col_pal_2 <- leaflet::colorNumeric(palette = "viridis", domain = SN1293304_mean_track_old$date_time)

map_SN1293304 <- 
  leaflet() %>%
  addTiles() %>%
  # old track
  addPolylines(data = mustelus_all_tracks %>% dplyr::filter(tag_serial_number == "1293304"),
               lat = ~detection_latitude_mean,
               lng = ~detection_longitude_mean,
               color = "black",
               weight = 3,
               group = "old track") %>%
  addCircleMarkers(data = mustelus_all_tracks %>% dplyr::filter(tag_serial_number == "1293304"),
                   lat = ~detection_latitude_mean,
                   lng = ~detection_longitude_mean,
                   label = ~paste0("time: ", time),
                   radius = 8,
                   weight = 0,
                   color = ~col_pal_2(date_time),
                   fillOpacity = 1,
                   group = "old track") %>%
  # new track
  addPolylines(data = SN1293304_mean_track,
               lat = ~latitude,
               lng = ~longitude,
               color = "black",
               weight = 3,
               group = "new track") %>%
  addCircleMarkers(data = SN1293304_mean_track,
                   lat = ~latitude,
                   lng = ~longitude,
                   label = ~paste0("time: ", time),
                   radius = 8,
                   weight = 0,
                   color = ~col_pal(time),
                   fillOpacity = 1,
                   group = "new track") %>%
  addLayersControl(overlayGroups = c("old track", "new track"),
                   options = layersControlOptions(collapsed = FALSE))

map_SN1293304
