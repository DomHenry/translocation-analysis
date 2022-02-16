## ________________________________________________________________________

## Title:     03_MVLM - process move data.R
## Purpose:   Calculate the movement metrics which are subsequently used in the
##            multivariate linear model (MVLM) analysis
## Author:    Dominic Henry
## Date:      16/02/2022

## Libraries
library(adehabitatLT)
library(sf)
library(tidyverse)
library(glue)
library(lubridate)
library(migrateR)
## ________________________________________________________________________

# Create R project output directories -------------------------------------
dir.create("data output/MVLM")

# Set projections ---------------------------------------------------------
aeaproj <- CRS("+proj=aea +lat_1=20 +lat_2=-23 +lat_0=0 +lon_0=25 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m")
latlong <- CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs")

# Import track metadata ---------------------------------------------------
tracks_metadata <- readxl::read_xlsx("data input/EGtraxR.xlsx") %>%
  janitor::clean_names() %>%
  mutate(pttid = as.character(pttid)) %>%
  mutate(filename = str_c(str_sub(pttid, 1, nchar(pttid) - 2),"g"))

birds_id <- tracks_metadata %>%
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

calc_move_metrics <- function(x){

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

  # 1. Mean daily distance
  dist_df <- track[[1]] %>%
    as_tibble() %>%
    mutate(date_only = date(date)) %>%
    group_by(date_only) %>%
    summarise(mean_dist = mean(dist)) %>%
    ungroup

  (mean_dd <- mean(dist_df$mean_dist, na.rm = TRUE))

  # 2. SD of daily distance moved
  (sd_dd <- sd(dist_df$mean_dist, na.rm = TRUE))

  # 3. Mean of absolute turning angle
  (mean_absang <- mean(track[[1]]$abs.angle, na.rm = TRUE))

  # 4. SD of mean displacement (can't change this because we need to measure from the capture site)
  (sd_meandisp <- sd(track[[1]]$R2n, na.rm = TRUE))

  # 5. SD of relative moving angle
  (sd_relang <- sd(track[[1]]$rel.angle, na.rm = TRUE))

  # 6. Total number of fixes
  total_fix <- nrow(track_data)

  # 7. Mean fixes per day
  fix_df <- track[[1]] %>%
    as_tibble() %>%
    mutate(date_only = date(date)) %>%
    group_by(date_only) %>%
    summarise(nfix_day = length(dx)) %>%
    ungroup

  fix_per_day <- mean(fix_df$nfix_day)

  # 8. Number of tracking days
  tracking_days <- nrow(fix_df)

  return(tibble(x, site, mean_dd, sd_dd, mean_absang, sd_meandisp, sd_relang,
                nfixes = total_fix, nfix_day = fix_per_day, ndays = tracking_days) %>%
           rename(pttid = x))
}

move_data <- map_df(.x = birds_id,
                    .f = calc_move_metrics)

## All birds included
move_data %>%
  write_csv("data output/MVLM/movement_metrics_all_birds.csv")
