# change dates for tag 308 and 321

library(readr)
library(lubridate)

setwd('C:/Users/lotte/HiDrive/Work/Research_stays/20240213_Ifremer_Woillez_geolocation_modelling/github/')
getwd()

data_path <- paste0(getwd(),"/00_data/data_mustelus_asterias_modelling/") 

### dst.csv ####

#### TAG 308 #####
dst_308 <- 
  readr::read_csv(file = paste0(data_path, "SN1293308/dst.csv"), show_col_types = F) %>%
  dplyr::mutate(time = time + lubridate::years(1))
readr::write_csv(dst_308, file = paste0(data_path, '/SN1293308/dst.csv'))

#### TAG 321 #####
dst_321 <- 
  readr::read_csv(file = paste0(data_path, "SN1293321/dst.csv"), show_col_types = F) %>%
  dplyr::mutate(time = time + lubridate::years(1))
readr::write_csv(dst_321, file = paste0(data_path, '/SN1293321/dst.csv'))


### acoustic.csv ####

#### TAG 308 #####
acoustic_308 <- 
  readr::read_csv(file = paste0(data_path, "SN1293308/acoustic.csv"), show_col_types = F) %>%
  dplyr::mutate(time = time + lubridate::years(1))
readr::write_csv(acoustic_308, file = paste0(data_path, '/SN1293308/acoustic.csv'))

#### TAG 321 ##### has no acoustic detections

### tagging_events.csv ####

#### TAG 308 #####
tagging_events_308 <- 
  readr::read_csv(file = paste0(data_path, "SN1293308/tagging_events.csv"), show_col_types = F) %>%
  dplyr::mutate(time = time + lubridate::years(1))
readr::write_csv(tagging_events_308, file = paste0(data_path, '/SN1293308/tagging_events.csv'))

#### TAG 321 #####
tagging_events_321 <- 
  readr::read_csv(file = paste0(data_path, "SN1293321/tagging_events.csv"), show_col_types = F) %>%
  dplyr::mutate(time = time + lubridate::years(1))
readr::write_csv(tagging_events_321, file = paste0(data_path, '/SN1293321/tagging_events.csv'))


### stations.csv ####

#### TAG 308 #####
stations_308 <- 
  readr::read_csv(file = paste0(data_path, "SN1293308/stations.csv"), show_col_types = F) %>%
  dplyr::mutate(deploy_time = deploy_time + lubridate::years(1))
readr::write_csv(stations_308, file = paste0(data_path, '/SN1293308/stations.csv'))
