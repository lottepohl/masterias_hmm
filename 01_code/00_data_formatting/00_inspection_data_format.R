#### LIBRARIES ####
library(readr)

#------ data format 13.2.24 --------------
#### SETUP WORKSPACE ####

setwd('C:/Users/lotte.pohl/HiDrive/Work/Research_stays/20240213_Ifremer_Woillez_geolocation_modelling/')
data_examples_path <- paste0(getwd(), '/00_data/data_example/DATA_FOR_PUBLICATION/')

##### OPEN FILES #####
detector <- readr::read_csv(paste0(data_examples_path, 'detector.csv'), show_col_types = FALSE)
detected <- readr::read_csv(paste0(data_examples_path, 'A19124/detected.csv'), show_col_types = FALSE)
info <- readr::read_csv(paste0(data_examples_path, 'A19124/info.csv'), show_col_types = FALSE)
dst <- readr::read_csv(paste0(data_examples_path, 'A19124/dst.csv'), show_col_types = FALSE)

##### OPEN ACOUSTIC DETECTION FILES ####
acoustic_files_path <- paste0(getwd(), '/00_data/data_mustelus_asterias_raw/')
animals <- readr::read_csv(paste0(acoustic_files_path, 'animals.csv'), show_col_types = FALSE)
deployments <- readr::read_csv(paste0(acoustic_files_path, 'deployments.csv'), show_col_types = FALSE)
detections <- readr::read_csv(paste0(acoustic_files_path, 'detections.csv'), show_col_types = FALSE)
receivers <- readr::read_csv(paste0(acoustic_files_path, 'receivers.csv'), show_col_types = FALSE)
tags <- readr::read_csv(paste0(acoustic_files_path, 'tags.csv'), show_col_types = FALSE)

#------ data format 14.2.24 --------------
#### SETUP WORKSPACE ####

setwd('C:/Users/lotte.pohl/HiDrive/Work/Research_stays/20240213_Ifremer_Woillez_geolocation_modelling/')
data_examples_path <- paste0(getwd(), '/00_data/data_example/updated_example/')

##### OPEN FILES #####
stations <- readr::read_csv(paste0(data_examples_path, 'stations.csv'), show_col_types = FALSE)
acoustic <- readr::read_csv(paste0(data_examples_path, 'acoustic.csv'), show_col_types = FALSE)
tagging_events <- readr::read_csv(paste0(data_examples_path, 'tagging_events.csv'), show_col_types = FALSE)
dst <- readr::read_csv(paste0(data_examples_path, 'dst.csv'), show_col_types = FALSE)
