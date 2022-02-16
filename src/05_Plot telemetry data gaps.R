## ________________________________________________________________________

## Title:     05_Plot telemetry movement gaps.R
## Purpose:   Plot the time between successive GPS fixes of all birds to explore data gaps
## Author:    Dominic Henry
## Date:      16/02/2022

## Libraries
library(adehabitatLT)
library(sf)
library(tidyverse)
library(glue)
library(lubridate)
## ________________________________________________________________________


# Define projections ------------------------------------------------------
aeaproj <- CRS("+proj=aea +lat_1=20 +lat_2=-23 +lat_0=0 +lon_0=25 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m")
latlong <- CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs")

# Import track metadata ---------------------------------------------------
tracks_metadata <- readxl::read_xlsx("data input/EGtraxR.xlsx") %>%
  janitor::clean_names() %>%
  mutate(pttid = as.character(pttid)) %>%
  mutate(filename = str_c(str_sub(pttid, 1, nchar(pttid) - 2),"g"))

birds_id <- tracks_metadata %>%
  # filter(!site %in% c("JOZ","MAN")) %>% # Decide on which birds to remove
  pull(pttid)

# Add xy col to sf object -------------------------------------------------
sfc_as_cols <- function(x, names = c("x","y")) {
  stopifnot(inherits(x,"sf") && inherits(sf::st_geometry(x),"sfc_POINT"))
  ret <- sf::st_coordinates(x)
  ret <- tibble::as_tibble(ret)
  stopifnot(length(names) == ncol(ret))
  x <- x[ , !names(x) %in% names]
  ret <- setNames(ret,names)
  dplyr::bind_cols(x,ret)
}

# Import tracks -----------------------------------------------------------

calc_time_gap <- function(x){

  data_files <- list.files("data input/", pattern = ".txt", full.names = TRUE)
  file <- data_files[which(str_detect(data_files,str_sub(x, 1, nchar(x) - 2)))]
  raw_data <- read.delim(file, header = TRUE, sep = "\t")
  start_date <- tracks_metadata %>% filter(pttid == x) %>% pull(start_dat)
  end_date <- tracks_metadata %>% filter(pttid == x) %>% pull(end_date)
  site <- tracks_metadata %>% filter(pttid == x) %>% pull(site)


  track_data <- raw_data %>%
    mutate(ptt = x) %>%
    janitor::clean_names() %>%
    as_tibble() %>%
    filter(latitude_n != 0) %>%
    mutate(date = ymd_hm(date_time)) %>%
    filter(date >= start_date & date <= end_date)


  track <- st_as_sf(track_data, coords = c("longitude_e","latitude_n"), crs = latlong) %>%
    st_transform(crs = aeaproj) %>%
    sfc_as_cols

  track <- as.ltraj(xy = cbind(track$x,track$y),
                    date = as.POSIXct(track$date),
                    id = track$ptt,
                    # burst = track$ptt,
                    typeII = TRUE)
  # plot(track, main = x)

  time_data <- track[[1]][,c("date","dt")] %>%
    as_tibble() %>%
    mutate(days = dt/3600/24,
           ptt_id = x)

}

move_data <- map_df(.x = birds_id,
                    .f = calc_time_gap)

move_data %>%
  filter(!ptt_id  %in% c("7711701","7712201","7712301"))

p1 <- move_data %>%
  filter(!ptt_id  %in% c("7711701","7712201","7712301")) %>%
  ggplot(aes(x = date, y = days))+
  geom_line()+
  geom_point()+
  ylab("Days between fixes")+
  xlab("")+
  scale_x_datetime(date_labels = "%b-%y")+
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        strip.text = element_text(size = 12))+
  ggforce::facet_wrap_paginate(~ptt_id, scales = "free", ncol = 5, nrow = 4, page = 1)
ggsave("data output/data_gaps_p1 - V2.jpeg", p1, width = 16, height = 12)

p2 <- move_data %>%
  filter(!ptt_id  %in% c("7711701","7712201","7712301")) %>%
  ggplot(aes(x = date, y = days))+
  geom_line()+
  geom_point()+
  ylab("Days between fixes")+
  xlab("")+
  scale_x_datetime(date_labels = "%b-%y")+
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        strip.text = element_text(size = 12))+
  ggforce::facet_wrap_paginate(~ptt_id, scales = "free", ncol = 5, nrow = 4, page = 2)
ggsave("data output/data_gaps_p2 - V2.jpeg", p2, width = 16, height = 12)

