
library(readr)
library(ggplot2)
library(dplyr)
library(plotly)
library(suncalc)

death_dates <- tibble( #visually inspect depthlogs
  tag_serial_number = c("1293295", "1293319", "1293322", "1293304", "1293310", "1293312", "1293308", "1293321"),
  death_date = c("2018-08-20 01:00:00", "2018-08-08 01:00:00", "2018-08-08 01:00:00", "2019-07-21 01:00:00", "2019-08-13 01:00:00", "2019-08-07 01:00:00", "2019-08-03 01:00:00", "2019-11-15 01:00:00"))

data_path <- 'C:/Users/lotte/HiDrive/Work/Research_stays/20240213_Ifremer_Woillez_geolocation_modelling/github/00_data/data_mustelus_asterias_modelling/'

# tag_serial_num <- '1293295'
make_depth_temp_plot <- function(tag_serial_num){
  dst <- readr::read_csv(paste0(data_path, '/SN', tag_serial_num, '/dst.csv')) 
  
  # depth plot
  depth <- 
    ggplot(data = dst, mapping = aes(x = time, y = -pressure)) +
      geom_line() +
      geom_vline(colour = 'red', xintercept = death_dates %>% dplyr::filter(tag_serial_number == tag_serial_num) %>% dplyr::select(death_date) %>% pull() %>% as.POSIXct()) +
      theme_bw()
  depth  
  
  ggplot2::ggsave(filename = paste0("C:/Users/lotte/HiDrive/Work/Research_stays/20240213_Ifremer_Woillez_geolocation_modelling/github/04_results_plots/SN", tag_serial_num, '/depth.png'), plot = depth, width = 18, height = 6, units = 'cm')
  ggplot2::ggsave(filename = paste0("C:/Users/lotte/HiDrive/Work/Research_stays/20240213_Ifremer_Woillez_geolocation_modelling/github/04_results_plots/SN", tag_serial_num, '/depth.pdf'), plot = depth, width = 18, height = 6, units = 'cm')
  
  # temperature plot
  temperature <- 
    ggplot(data = dst, mapping = aes(x = time, y = temperature)) +
    geom_line() +
    geom_vline(colour = 'red', xintercept = death_dates %>% dplyr::filter(tag_serial_number == tag_serial_num) %>% dplyr::select(death_date) %>% pull() %>% as.POSIXct()) +
    theme_bw()
  temperature  
  
  ggplot2::ggsave(filename = paste0("C:/Users/lotte/HiDrive/Work/Research_stays/20240213_Ifremer_Woillez_geolocation_modelling/github/04_results_plots/SN", tag_serial_num, '/temperature.png'), plot = temperature, width = 18, height = 6, units = 'cm')
  ggplot2::ggsave(filename = paste0("C:/Users/lotte/HiDrive/Work/Research_stays/20240213_Ifremer_Woillez_geolocation_modelling/github/04_results_plots/SN", tag_serial_num, '/temperature.pdf'), plot = temperature, width = 18, height = 6, units = 'cm')
  
}

# loop over all tag_serial_numbers, create a df and assign a unique name
for(tag in death_dates$tag_serial_number){
  dst <- make_depth_temp_plot(tag_serial_num = tag)
}


##### plot of tag 308 ####
# mustelus_all_tracks <- readr::read_csv('C:/Users/lotte/HiDrive/Work/Research_stays/20240213_Ifremer_Woillez_geolocation_modelling/github/00_data/data_mustelus_asterias_raw/masterias_depth_temp.csv', show_col_types = FALSE)

dst_1293308 <- readr::read_csv(paste0(data_path, '/SN1293308/dst.csv'))
sunlight_times <- suncalc::getSunlightTimes(lat = 50.0, lon = 0.5, date = dst_1293308$time %>% lubridate::date() %>% unique())

plot_308_temperature <- 
  ggplot(data = dst_1293308, mapping = aes(x = time, y = temperature)) +
  geom_line() +
  theme_bw()
plot_308_temperature %>% ggplotly()  


# Plot
plot_308_depth <- ggplot() +
  geom_rect(data = sunlight_times,
            aes(xmin = sunset, xmax = sunrise, ymin = -80, ymax = 0),
            fill = "white", alpha = 1) + # Adjust alpha for transparency
  # geom_rect(data = night_intervals,
  #           aes(xmin = start, xmax = end, ymin = -80, ymax = 0),
  #           fill = "grey", alpha = 0.4) + # Adjust alpha for transparency
  geom_line(data = dst_1293308, aes(x = time, y = -pressure)) +
  theme(panel.background = element_rect(fill = "gray80"))
  # theme_bw()

# Convert to plotly for an interactive plot
plot_308_depth %>% ggplotly()

