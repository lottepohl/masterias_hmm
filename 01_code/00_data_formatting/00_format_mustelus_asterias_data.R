#### LIBRARIES ####
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(plotly)
library(jsonlite)

#### SETUP WORKSPACE ####
setwd('../')
# setwd('C:/Users/lotte.pohl/HiDrive/Work/Research_stays/20240213_Ifremer_Woillez_geolocation_modelling/')

# colnames <- c("ADST DATA LOG", "1.0.0", "AdstConverter-1.0.5", "...4", "...5", "...6", "...7", "...8", "...9", "...10", "...11")
data_mustelus_path <- paste0(getwd(), '/00_data/data_mustelus_asterias_raw/')

#### 0. death dates ####
death_dates <- tibble(
  tag_serial_number = c("1293295", "1293319", "1293322", "1293304", "1293310", "1293312", "1293308", "1293321"),
  death_date = c("2018-08-20 01:00:00", "2018-08-08 01:00:00", "2018-08-08 01:00:00", "2019-07-21 01:00:00", "2019-08-13 01:00:00", "2019-08-07 01:00:00", "2019-08-03 01:00:00", "2019-11-15 01:00:00"))


#### 1. make dst.csv ####
# load master depth temp log
mustelus_all_depth_temp <- readr::read_csv(paste0(data_mustelus_path, 'masterias_depth_temp.csv'), show_col_types = FALSE)
mustelus_all_tracks <- readr::read_csv(paste0(data_mustelus_path, 'mustelus_asterias_all_geolocation_outputs.csv'), show_col_types = FALSE)

make_dst <- function(tag_serial_num){
  # format the file correctly
  dst <- mustelus_all_depth_temp %>%
    dplyr::filter(tag_serial_number == tag_serial_num) %>%
    dplyr::select(date_time, temp_c, depth_m) %>%
    dplyr::rename(time = date_time, temperature = temp_c, pressure = depth_m)
  
  #save csv
  readr::write_csv(dst, file = paste0(getwd(), '/00_data/data_mustelus_asterias_modelling/SN', tag_serial_num, '/dst.csv'))
  return(dst)
}

# loop over all tag_serial_numbers, create a df and assign a unique name
for(tag in death_dates$tag_serial_number){
  dst <- make_dst(tag_serial_num = tag)
  assign(paste0('dst_', tag), dst)
}

mustelus_295_combined_log_raw <- readr::read_csv(paste0(data_mustelus_path, 'SN1293295/SN1293295_combined_log.csv'), show_col_types = FALSE)
mustelus_295_pressure_log_raw <- readr::read_csv(paste0(data_mustelus_path, 'SN1293295/SN1293295_pressure_log.csv'), show_col_types = FALSE)
mustelus_295_temperature_log_raw <- readr::read_csv(paste0(data_mustelus_path, 'SN1293295/SN1293295_temperature_log.csv'), show_col_types = FALSE)

mustelus_295_temperature_log <- 
  mustelus_295_temperature_log_raw %>%
  dplyr::filter(`ADST DATA LOG` %in% c('PRESSURE', 'TEMPERATURE')) %>% 
  dplyr::select(`ADST DATA LOG`, ...4, ...6) %>%
  dplyr::filter(!is.na(...4)) %>%
  tidyr::pivot_wider(names_from = `ADST DATA LOG`, values_from = ...6) %>%
  dplyr::rename(time = ...4, temperature = TEMPERATURE) %>%
  dplyr::mutate(time = time %>% lubridate::mdy_hms() %>% format("%Y-%m-%d %H:%M:%S") %>% as.POSIXct())

mustelus_295_pressure_log <- 
  mustelus_295_pressure_log_raw %>%
  dplyr::filter(`ADST DATA LOG` %in% c('PRESSURE', 'TEMPERATURE')) %>% 
  dplyr::select(`ADST DATA LOG`, ...4, ...6) %>%
  dplyr::filter(!is.na(...4)) %>%
  tidyr::pivot_wider(names_from = `ADST DATA LOG`, values_from = ...6) %>%
  dplyr::rename(time = ...4, pressure = PRESSURE) %>%
  dplyr::mutate(time = time %>% lubridate::mdy_hms() %>% format("%Y-%m-%d %H:%M:%S") %>% as.POSIXct())

# wrangle data log
mustelus_295_combined_log <- 
  mustelus_295_combined_log_raw %>%
  dplyr::filter(`ADST DATA LOG` %in% c('PRESSURE', 'TEMPERATURE')) %>% 
  dplyr::select(`ADST DATA LOG`, ...4, ...6) %>%
  dplyr::filter(!is.na(...4)) %>%
  tidyr::pivot_wider(names_from = `ADST DATA LOG`, values_from = ...6) %>%
  tidyr::drop_na() %>% #for now: drop NAs, improvement = take lead and lag temp val and set NA temp val to the middle
  dplyr::rename(time = ...4, temperature = TEMPERATURE, pressure = PRESSURE) %>%
  dplyr::select(time, temperature, pressure) %>%
  dplyr::mutate(time = time %>% lubridate::mdy_hms() %>% format("%Y-%m-%d %H:%M:%S") %>% as.POSIXct(),
                temperature = temperature %>% as.numeric(),
                pressure = pressure %>% as.numeric())
# export data log as csv
readr::write_csv(mustelus_295_combined_log, file = paste0(getwd(), '/00_data/data_mustelus_asterias_modelling/SN1293295/dst.csv'))
# quick plot --> for tag 295 this does not look right....:/
ggplot(data = mustelus_295_combined_log, mapping = aes(x = time, y = temperature)) +
  geom_line() +
  theme_bw()

ggplot(data = mustelus_295_combined_log, mapping = aes(x = time, y = -pressure)) +
  geom_line() +
  theme_bw()

#### 2. make acoustic.csv ####
# get acoustic_tag_ids
tags <- readr::read_csv(paste0(data_mustelus_path, 'acoustic_detections/tags.csv'), show_col_types = FALSE)
# # inspect acoustic_tag_ids
# tags %>% dplyr::select(tag_serial_number, acoustic_tag_id) %>% unique() %>% View() # two acoustic_tag_ids per tag_serial_number because ADST == combination tag

# load detections master file
detections <- 
  readr::read_csv(paste0(data_mustelus_path, 'acoustic_detections/detections.csv'), show_col_types = FALSE) %>%
  dplyr::select(deployment_id, date_time, acoustic_tag_id) %>%
  dplyr::rename(time = date_time)
# load deployments master file
deployments <- 
  readr::read_csv(paste0(data_mustelus_path, 'acoustic_detections/deployments.csv'), show_col_types = FALSE) %>%
    dplyr::select(deployment_id, deploy_latitude, deploy_longitude) %>%
    dplyr::rename(latitude = deploy_latitude, longitude = deploy_longitude)

make_acoustic <- function(tag_serial_num){
  acoustic_tag_ids <- 
    tags %>%
      dplyr::filter(tag_serial_number == tag_serial_num) %>%
      dplyr::select(acoustic_tag_id)
  
  acoustic <- 
    detections %>%
      dplyr::filter(acoustic_tag_id %in% acoustic_tag_ids$acoustic_tag_id) %>%
      dplyr::left_join(deployments, by = join_by(deployment_id)) %>%
      dplyr::select(time, deployment_id, longitude, latitude)
  
  #save csv
  readr::write_csv(acoustic, file = paste0(getwd(), '/00_data/data_mustelus_asterias_modelling/SN', tag_serial_num, '/acoustic.csv'))
  return(acoustic)
}

# loop over all tag_serial_numbers, create a df and assign a unique name
for(tag in death_dates$tag_serial_number){
  acoustic <- make_acoustic(tag_serial_num = tag)
  assign(paste0('acoustic_', tag), acoustic)
}

#### 3. make tagging_events.csv ####
animals <- readr::read_csv(paste0(data_mustelus_path, 'acoustic_detections/animals.csv'), show_col_types = FALSE) %>%
  dplyr::mutate(acoustic_tag_id = gsub(",.*","", animals$acoustic_tag_id)) # get rid of double acoustic_tag_id per column
  
make_tagging_events <- function(tag_serial_num){
  acoustic_tag_ids <- 
    tags %>%
    dplyr::filter(tag_serial_number == tag_serial_num) %>%
    dplyr::select(acoustic_tag_id)
  
  tagging_events <- 
    animals %>%
    dplyr::filter(acoustic_tag_id %in% acoustic_tag_ids$acoustic_tag_id) %>%
    dplyr::rename(latitude = release_latitude, longitude = release_longitude, time = release_date_time) %>%
    dplyr::mutate(event_name = 'release') %>%
    dplyr::select(event_name, time, longitude, latitude) %>%
    dplyr::add_row(event_name = 'fish_death',
                   time = death_dates %>% dplyr::filter(tag_serial_number == tag_serial_num) %>% dplyr::select(death_date) %>% dplyr::pull() %>% as.POSIXct(format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
                   longitude = mustelus_all_tracks %>% dplyr::filter(tag_serial_number == tag_serial_num, date_time == time %>% format("%Y-%m-%d") %>% as.POSIXct(tz = "UTC")) %>%
                     dplyr::select(detection_longitude_mode) %>% pull(),
                   latitude = mustelus_all_tracks %>% dplyr::filter(tag_serial_number == tag_serial_num, date_time == time %>% format("%Y-%m-%d") %>% as.POSIXct(tz = "UTC")) %>%
                     dplyr::select(detection_latitude_mode) %>% pull())
  
  #save csv
  readr::write_csv(tagging_events, file = paste0(getwd(), '/00_data/data_mustelus_asterias_modelling/SN', tag_serial_num, '/tagging_events.csv'))
  return(tagging_events)
}

# loop over all tag_serial_numbers, create a df and assign a unique name
for(tag in death_dates$tag_serial_number){
  tagging_events <- make_tagging_events(tag_serial_num = tag)
  assign(paste0('tagging_events_', tag), tagging_events)
}

#### 4. make stations.csv ####
# load deployments master file
deployments <- readr::read_csv(paste0(data_mustelus_path, 'acoustic_detections/deployments.csv'), show_col_types = FALSE) %>%
  dplyr::rename(deploy_time = deploy_date_time, recover_time = recover_date_time)

make_stations <- function(tag_serial_num){
  acoustic_tag_ids <- 
    tags %>%
      dplyr::filter(tag_serial_number == tag_serial_num) %>%
      dplyr::select(acoustic_tag_id) %>%
      dplyr::pull()
  
  deployment_ids <- 
    detections %>% 
      dplyr::filter(acoustic_tag_id %in% acoustic_tag_ids) %>% 
      dplyr::select(deployment_id) %>% 
      dplyr::pull()
  
  stations <-
    deployments %>% 
      dplyr::filter(deployment_id %in% deployment_ids) %>%
      dplyr::select(deployment_id, station_name, deploy_time, deploy_longitude, deploy_latitude, recover_time, recover_longitude, recover_latitude)
  
  #save csv
  readr::write_csv(stations, file = paste0(getwd(), '/00_data/data_mustelus_asterias_modelling/SN', tag_serial_num, '/stations.csv'))
  
  return(stations)
}

# loop over all tag_serial_numbers, create a df and assign a unique name
for(tag in death_dates$tag_serial_number){
  stations <- make_stations(tag_serial_num = tag)
  assign(paste0('stations_', tag), stations)
}

#### 5. make metadate.json ####

# Define the metadata as a list
data <- list(
  project = "ADST-Shark",
  scientific_name = "Mustelus asterias",
  common_name = "Starry smooth-hound"
)

# loop over all tag_serial_numbers, create a df and assign a unique name
for(tag in death_dates$tag_serial_number){
  # Convert the data to JSON and write to a file
  jsonlite::write_json(data, path = paste0(getwd(), '/00_data/data_mustelus_asterias_modelling/SN', tag, '/metadata.json'))
}

#### 6. NOT YET make behavioural states dst.csv ####

##### TAG 308 #####
# mustelus_308_behaviour <- readr::read_csv(paste0(data_mustelus_path, 'behavioural_states/female_behaviour_df.csv'), show_col_types = FALSE) #not needed
mustelus_308_behaviour_day <- 
  readr::read_csv(paste0(data_mustelus_path, 'behavioural_states/female_behaviour_day_df.csv'), show_col_types = FALSE) %>%
    dplyr::mutate(behaviour = ifelse(!migrating %>% is.na(), 3, 
                                     ifelse(!feeding %>% is.na(), 2, 1)) # categorize behaviours into resting = 1, feeding = 2, migrating = 3
      )

# wrangle data log
mustelus_308_combined_log_behaviour <- 
  mustelus_308_combined_log_raw %>%
  dplyr::filter(`ADST DATA LOG` %in% c('PRESSURE', 'TEMPERATURE')) %>% 
  dplyr::select(`ADST DATA LOG`, ...4, ...6) %>%
  tidyr::pivot_wider(names_from = `ADST DATA LOG`, values_from = ...6) %>%
  tidyr::drop_na() %>% #for now: drop NAs, improvement = take lead and lag temp val and set NA temp val to the middle
  dplyr::rename(time = ...4, pressure = PRESSURE, water_temperature = TEMPERATURE) %>%
  dplyr::mutate(time = time %>% lubridate::mdy_hms() %>% format("%Y-%m-%d %H:%M:%S"),
                date = lubridate::date(time)) %>%
  dplyr::left_join(mustelus_308_behaviour_day %>% dplyr::select(date, behaviour), by = join_by(date))
# export data log as csv
readr::write_csv(mustelus_308_combined_log_behaviour, file = paste0(getwd(), '/00_data/data_mustelus_asterias_modelling/SN1293308/dst_behaviour.csv'))

##### TAG 321 #####
# mustelus_321_behaviour <- readr::read_csv(paste0(data_mustelus_path, 'behavioural_states/female_behaviour_df.csv'), show_col_types = FALSE) #not needed
mustelus_321_behaviour_day <- 
  readr::read_csv(paste0(data_mustelus_path, 'behavioural_states/male_behaviour_day_df.csv'), show_col_types = FALSE) %>%
  dplyr::mutate(behaviour = ifelse(!migrating %>% is.na(), 3, 
                                   ifelse(!feeding %>% is.na(), 2, 1)) # categorize behaviours into resting = 1, feeding = 2, migrating = 3
  )

# wrangle data log
mustelus_321_combined_log_behaviour <- 
  mustelus_321_combined_log_raw %>%
  dplyr::filter(`RECORD TYPE` %in% c('PRESSURE', 'TEMPERATURE')) %>% 
  dplyr::select(`RECORD TYPE`, FIELD...4, FIELD...6) %>%
  tidyr::pivot_wider(names_from = `RECORD TYPE`, values_from = FIELD...6) %>%
  tidyr::drop_na() %>% #for now: drop NAs, improvement = take lead and lag temp val and set NA temp val to the middle
  dplyr::rename(time = FIELD...4, pressure = PRESSURE, water_temperature = TEMPERATURE) %>%
  dplyr::mutate(time = time %>% lubridate::mdy_hms() %>% format("%Y-%m-%d %H:%M:%S"),
                date = lubridate::date(time)) %>%
  dplyr::left_join(mustelus_321_behaviour_day %>% dplyr::select(date, behaviour), by = join_by(date))
# export data log as csv
readr::write_csv(mustelus_321_combined_log_behaviour, file = paste0(getwd(), '/00_data/data_mustelus_asterias_modelling/SN1293321/dst_behaviour.csv'))

##### old ####


# TAG 295
dst_295_new <- make_dst(tag_serial_num = '1293295')

dst_295 <- 
  mustelus_all_depth_temp %>%
  dplyr::filter(tag_serial_number == "1293295") %>%
  dplyr::select(date_time, temp_c, depth_m) %>%
  dplyr::rename(time = date_time, temperature = temp_c, pressure = depth_m)

mustelus_295_combined_log_raw <- readr::read_csv(paste0(data_mustelus_path, 'SN1293295/SN1293295_combined_log.csv'), show_col_types = FALSE)
# wrangle data log
mustelus_295_combined_log <- 
  mustelus_295_combined_log_raw %>%
  dplyr::filter(`ADST DATA LOG` %in% c('PRESSURE', 'TEMPERATURE')) %>% 
  dplyr::select(`ADST DATA LOG`, ...4, ...6) %>%
  dplyr::filter(!is.na(...4)) %>%
  tidyr::pivot_wider(names_from = `ADST DATA LOG`, values_from = ...6) %>%
  tidyr::drop_na() %>% #for now: drop NAs, improvement = take lead and lag temp val and set NA temp val to the middle
  dplyr::rename(time = ...4, temperature = TEMPERATURE, pressure = PRESSURE) %>%
  dplyr::select(time, temperature, pressure) %>%
  dplyr::mutate(time = time %>% lubridate::mdy_hms() %>% format("%Y-%m-%d %H:%M:%S") %>% as.POSIXct(),
                temperature = temperature %>% as.numeric(),
                pressure = pressure %>% as.numeric())
# export data log as csv
readr::write_csv(mustelus_295_combined_log, file = paste0(getwd(), '/00_data/data_mustelus_asterias_modelling/SN1293295/dst.csv'))
# quick plot --> for tag 295 this does not look right....:/
ggplot(data = mustelus_295_combined_log, mapping = aes(x = time, y = -pressure)) +
  geom_point() +
  theme_bw()

# TAG 304 
mustelus_304_combined_log_raw <- readr::read_csv(paste0(data_mustelus_path, 'SN1293304/SN1293304_combined_log.csv'), show_col_types = FALSE)
# wrangle data log
mustelus_304_combined_log <- 
  mustelus_304_combined_log_raw %>%
  dplyr::filter(`ADST DATA LOG` %in% c('PRESSURE', 'TEMPERATURE')) %>% 
  dplyr::select(`ADST DATA LOG`, ...4, ...6) %>%
  dplyr::filter(!is.na(...4)) %>%
  tidyr::pivot_wider(names_from = `ADST DATA LOG`, values_from = ...6) %>%
  tidyr::drop_na() %>% #for now: drop NAs, improvement = take lead and lag temp val and set NA temp val to the middle
  dplyr::rename(time = ...4, temperature = TEMPERATURE, pressure = PRESSURE) %>%
  dplyr::select(time, temperature, pressure) %>%
  dplyr::mutate(time = time %>% lubridate::mdy_hms() %>% format("%Y-%m-%d %H:%M:%S") %>% as.POSIXct(),
                temperature = temperature %>% as.numeric(),
                pressure = pressure %>% as.numeric())
# export data log as csv
readr::write_csv(mustelus_304_combined_log, file = paste0(getwd(), '/00_data/data_mustelus_asterias_modelling/SN1293304/dst.csv'))

# quick plot #death date 304: 2019-07-23
p <- ggplot(data = mustelus_304_combined_log, mapping = aes(x = time, y = -pressure)) +
  geom_point() +
  theme_bw()
p %>% plotly::ggplotly()

# TAG 308
mustelus_308_combined_log_raw <- readr::read_csv(paste0(data_mustelus_path, 'SN1293308/SN1293308_combined_log.csv'), show_col_types = FALSE)
# wrangle data log
mustelus_308_combined_log <- 
  mustelus_308_combined_log_raw %>%
  dplyr::filter(`ADST DATA LOG` %in% c('PRESSURE', 'TEMPERATURE')) %>% 
  dplyr::select(`ADST DATA LOG`, ...4, ...6) %>%
  tidyr::pivot_wider(names_from = `ADST DATA LOG`, values_from = ...6) %>%
  tidyr::drop_na() %>% #for now: drop NAs, improvement = take lead and lag temp val and set NA temp val to the middle
  dplyr::rename(time = ...4, pressure = PRESSURE, temperature = TEMPERATURE) %>%
  dplyr::select(time, temperature, pressure) %>%
  dplyr::mutate(time = time %>% lubridate::mdy_hms() %>% format("%Y-%m-%d %H:%M:%S") %>% as.POSIXct(),
                temperature = temperature %>% as.numeric(),
                pressure = pressure %>% as.numeric())
# export data log as csv
readr::write_csv(mustelus_308_combined_log, file = paste0(getwd(), '/00_data/data_mustelus_asterias_modelling/SN1293308/dst.csv'))

# quick plot #death date 308: 2019-08-03
p <- ggplot(data = dst_1293295, mapping = aes(x = time, y = -pressure)) +
  geom_point() +
  theme_bw()
p %>% plotly::ggplotly()

# TAG 310
mustelus_310_combined_log_raw <- readr::read_csv(paste0(data_mustelus_path, 'SN1293310/SN1293310_combined_log.csv'), show_col_types = FALSE)
# wrangle data log
mustelus_310_combined_log <- 
  mustelus_310_combined_log_raw %>%
  dplyr::filter(`ADST DATA LOG` %in% c('PRESSURE', 'TEMPERATURE')) %>% 
  dplyr::select(`ADST DATA LOG`, ...4, ...6) %>%
  dplyr::filter(!is.na(...4)) %>%
  tidyr::pivot_wider(names_from = `ADST DATA LOG`, values_from = ...6) %>%
  dplyr::mutate(pressure = ifelse(is.na(pressure) & row_number() < n(), dplyr::lead(pressure), pressure)) %>%
  tidyr::drop_na() %>% #for now: drop NAs, improvement = take lead and lag temp val and set NA temp val to the middle
  dplyr::rename(time = ...4, temperature = TEMPERATURE, pressure = PRESSURE) %>%
  dplyr::select(time, temperature, pressure) %>%
  dplyr::mutate(time = time %>% lubridate::mdy_hms() %>% format("%Y-%m-%d %H:%M:%S") %>% as.POSIXct(),
                temperature = temperature %>% as.numeric(),
                pressure = pressure %>% as.numeric())
# export data log as csv
readr::write_csv(mustelus_310_combined_log, file = paste0(getwd(), '/00_data/data_mustelus_asterias_modelling/SN1293310/dst.csv'))

# quick plot #death date 310: 2019-07-23
p <- ggplot(data = mustelus_310_combined_log, mapping = aes(x = time, y = -pressure)) +
  geom_point() +
  theme_bw()
p %>% plotly::ggplotly()

# TAG 321 
mustelus_321_combined_log_raw <- readr::read_csv(paste0(data_mustelus_path, 'SN1293321/SN1293321_combined_log.csv'), show_col_types = FALSE)
# wrangle data log
mustelus_321_combined_log <- 
  mustelus_321_combined_log_raw %>%
  dplyr::filter(`RECORD TYPE` %in% c('PRESSURE', 'TEMPERATURE')) %>% 
  dplyr::select(`RECORD TYPE`, FIELD...4, FIELD...6) %>%
  tidyr::pivot_wider(names_from = `RECORD TYPE`, values_from = FIELD...6) %>%
  tidyr::drop_na() %>% #for now: drop NAs, improvement = take lead and lag temp val and set NA temp val to the middle
  dplyr::rename(time = FIELD...4, pressure = PRESSURE, temperature = TEMPERATURE) %>%
  dplyr::select(time, temperature, pressure) %>%
  dplyr::mutate(time = time %>% lubridate::mdy_hms() %>% format("%Y-%m-%d %H:%M:%S") %>% as.POSIXct(),
                temperature = temperature %>% as.numeric(),
                pressure = pressure %>% as.numeric())
# export data log as csv
readr::write_csv(mustelus_321_combined_log, file = paste0(getwd(), '/00_data/data_mustelus_asterias_modelling/SN1293321/dst.csv'))



# TAG 308 #
acoustic_tag_ids_308 <- 
  tags %>%
  dplyr::filter(tag_serial_number == "1293308") %>%
  dplyr::select(acoustic_tag_id)

detected_308 <- 
  detections %>%
  dplyr::filter(acoustic_tag_id %in% acoustic_tag_ids_308$acoustic_tag_id)

# export data as csv
readr::write_csv(detected_308, file = paste0(getwd(), '/00_data/data_mustelus_asterias_modelling/SN1293308/detected.csv'))

# TAG 321
acoustic_tag_ids_321 <- 
  tags %>%
  dplyr::filter(tag_serial_number == "1293321") %>%
  dplyr::select(acoustic_tag_id)

detected_321 <- 
  detections %>%
  dplyr::filter(acoustic_tag_id %in% acoustic_tag_ids_321$acoustic_tag_id)

# export data as csv
readr::write_csv(detected_321, file = paste0(getwd(), '/00_data/data_mustelus_asterias_modelling/SN1293321/detected.csv'))


# TAG 308 
info_308 <- 
  animals %>%
  dplyr::filter(acoustic_tag_id %in% acoustic_tag_ids_308$acoustic_tag_id) %>%
  dplyr::select(all_of(colnames)) %>%
  dplyr::rename(acoustic_id = acoustic_tag_id, release_time = release_date_time, recovery_time = capture_date_time, recovery_latitude = capture_latitude,
                recovery_longitude = capture_longitude)

# export data as csv
readr::write_csv(info_308, file = paste0(getwd(), '/00_data/data_mustelus_asterias_modelling/SN1293308/info.csv'))

# TAG 321 
info_321 <- 
  animals %>%
  dplyr::filter(acoustic_tag_id %in% acoustic_tag_ids_321$acoustic_tag_id) %>%
  dplyr::select(all_of(colnames)) %>%
  dplyr::rename(acoustic_id = acoustic_tag_id, release_time = release_date_time, recovery_time = capture_date_time, recovery_latitude = capture_latitude,
                recovery_longitude = capture_longitude)

# export data as csv
readr::write_csv(info_321, file = paste0(getwd(), '/00_data/data_mustelus_asterias_modelling/SN1293321/info.csv'))



detector_mustelus_asterias <- 
  deployments %>% 
  dplyr::select(all_of(colnames(detector))) %>%
  dplyr::filter(deployment_id %in% detected_308$deployment_id | 
                  deployment_id %in% detected_321$deployment_id)

# export data as csv
readr::write_csv(detector_mustelus_asterias, file = paste0(getwd(), '/00_data/data_mustelus_asterias_modelling/detector.csv'))
