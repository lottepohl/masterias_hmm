library(arrow)
library(readr)
library(leaflet)
setwd('C:/Users/lotte/HiDrive/Work/Research_stays/20240213_Ifremer_Woillez_geolocation_modelling/github/')
getwd()
# setwd('C:/Users/lotte.pohl/HiDrive/Work/Research_stays/20240213_Ifremer_Woillez_geolocation_modelling/')

# colnames <- c("ADST DATA LOG", "1.0.0", "AdstConverter-1.0.5", "...4", "...5", "...6", "...7", "...8", "...9", "...10", "...11")
data_mustelus_path <- paste0(getwd(), '/00_data/data_mustelus_asterias_raw/')

mustelus_all_tracks <- readr::read_csv(paste0(data_mustelus_path, 'mustelus_asterias_all_geolocation_outputs.csv'), show_col_types = FALSE)

##### track 304 ########

# Replace the path with the path to your Parquet file
parquet_file_path <- paste0("C:/Users/lotte/HiDrive/Work/Research_stays/20240213_Ifremer_Woillez_geolocation_modelling/github/02_results_tracks/SN1293304/mean.parquet")
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
                   label = ~paste0("time: ", date_time),
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

##### track 310 ########

# Replace the path with the path to your Parquet file
parquet_file_path <- paste0(getwd(), "/02_results_tracks/SN1293310/mean.parquet")
parquet_file_path
# parquet_file_path <- "path/to/your/file.parquet"

# Read the Parquet file
SN1293310_mean_track <- arrow::read_parquet(parquet_file_path)
SN1293310_mean_track_old <- mustelus_all_tracks %>% dplyr::filter(tag_serial_number == "1293310")

col_pal <- leaflet::colorNumeric(palette = "inferno", domain = SN1293310_mean_track$time)
col_pal_2 <- leaflet::colorNumeric(palette = "viridis", domain = SN1293310_mean_track_old$date_time)

map_SN1293310 <- 
  leaflet() %>%
  addTiles() %>%
  # old track
  addPolylines(data = mustelus_all_tracks %>% dplyr::filter(tag_serial_number == "1293310"),
               lat = ~detection_latitude_mean,
               lng = ~detection_longitude_mean,
               color = "black",
               weight = 3,
               group = "old track") %>%
  addCircleMarkers(data = mustelus_all_tracks %>% dplyr::filter(tag_serial_number == "1293310"),
                   lat = ~detection_latitude_mean,
                   lng = ~detection_longitude_mean,
                   label = ~paste0("time: ", date_time),
                   radius = 8,
                   weight = 0,
                   color = ~col_pal_2(date_time),
                   fillOpacity = 1,
                   group = "old track") %>%
  # new track
  addPolylines(data = SN1293310_mean_track,
               lat = ~latitude,
               lng = ~longitude,
               color = "black",
               weight = 3,
               group = "new track") %>%
  addCircleMarkers(data = SN1293310_mean_track,
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

map_SN1293310

##### track 312 ########

# Replace the path with the path to your Parquet file
parquet_file_path <- paste0(getwd(), "/02_results_tracks/SN1293312/mean.parquet")
parquet_file_path
# parquet_file_path <- "path/to/your/file.parquet"

# Read the Parquet file
SN1293312_mean_track <- arrow::read_parquet(parquet_file_path)
SN1293312_mean_track_old <- mustelus_all_tracks %>% dplyr::filter(tag_serial_number == "1293312")

col_pal <- leaflet::colorNumeric(palette = "inferno", domain = SN1293312_mean_track$time)
col_pal_2 <- leaflet::colorNumeric(palette = "viridis", domain = SN1293312_mean_track_old$date_time)

map_SN1293312 <- 
  leaflet() %>%
  addTiles() %>%
  # old track
  addPolylines(data = mustelus_all_tracks %>% dplyr::filter(tag_serial_number == "1293312"),
               lat = ~detection_latitude_mean,
               lng = ~detection_longitude_mean,
               color = "black",
               weight = 3,
               group = "old track") %>%
  addCircleMarkers(data = mustelus_all_tracks %>% dplyr::filter(tag_serial_number == "1293312"),
                   lat = ~detection_latitude_mean,
                   lng = ~detection_longitude_mean,
                   label = ~paste0("time: ", date_time),
                   radius = 8,
                   weight = 0,
                   color = ~col_pal_2(date_time),
                   fillOpacity = 1,
                   group = "old track") %>%
  # new track
  addPolylines(data = SN1293312_mean_track,
               lat = ~latitude,
               lng = ~longitude,
               color = "black",
               weight = 3,
               group = "new track") %>%
  addCircleMarkers(data = SN1293312_mean_track,
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

map_SN1293312
