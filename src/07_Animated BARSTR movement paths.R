## ________________________________________________________________________

## Title:    07_Animated BARSTR movement paths.R
## Purpose:  Code to recreate the animated plots of translocated birds in Appendix 5 (.mp4 format)
## Author:   Dominic Henry
## Date:     16/02/2022

## Libraries
library(tidyverse)
library(sf)
library(moveVis)
library(move)
## ________________________________________________________________________


# Define projections ------------------------------------------------------
aeaproj <- "+proj=aea +lat_1=20 +lat_2=-23 +lat_0=0 +lon_0=25 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m"
latlongCRS <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

# Import BARSTR tracks ----------------------------------------------------
barstr_ptt <- c("7711603", "7711703", "7711803", "7711903", "7712103", "7713303")

import_tracks <- function(bird_id) {
  read_sf(glue::glue("data input/{bird_id}_pts.shp")) %>%
    mutate(birdID = bird_id) %>%
    mutate(datetime = as.POSIXct(date)) %>%
    st_transform(crs = latlongCRS)
}

barstr_tracks <- map(
  .x = barstr_ptt,
  .f = import_tracks
) %>%
  do.call(rbind, .)

# Filter tracks to only include data up until October 2015 ----------------
barstr_tracks <- barstr_tracks %>%
  filter(datetime < lubridate::ymd("2015-10-29"))

xydata <- as_tibble(st_coordinates(barstr_tracks))

tracks <- barstr_tracks %>%
  as_tibble() %>%
  bind_cols(., xydata) %>%
  mutate(birdID = as_factor(birdID))

tracks

# Create a move object ----------------------------------------------------
move_data <- df2move(tracks,
  proj = latlongCRS,
  x = "X", y = "Y", time = "datetime", track_id = "birdID"
)

move_data <- align_move(move_data, res = 2, digit = 0, unit = "hours")


# Set colors --------------------------------------------------------------
cbp2 <- c(
  "#000000", "#E69F00", "#56B4E9", "#009E73",
  "#0072B2", "#D55E00"
)


# Create spatial movement animated map ------------------------------------
frames <- moveVis::frames_spatial(
  move_data,
  path_legend = TRUE,
  map_service = "mapbox",
  map_type = "satellite", equidistant = FALSE,
  map_token = "pk.eyJ1IjoiZ2FyZXRoanRhdGUiLCJhIjoiY2s2Zjl3bTVyMGx3ZTNycXBoaDUyeGJnNyJ9.j4oQnc4ttqe-ufJVYUe_uQ",
  path_legend_title = "PTT",
  path_colours = cbp2,
  alpha = 0.8
) %>%
  add_progress() %>%
  add_scalebar(height = 0.015) %>%
  add_northarrow() %>%
  add_timestamps(move_data, type = "label", size = 4) %>%
  add_labels(
    x = "Longitude", y = "Latitude",
    title = "Tracks of translocated Egyptian Geese",
    subtitle = "PTT set to record 2 hourly fixes"
  ) %>%
  add_gg(
    expr(
      scale_colour_manual(
        name = "PTT",
        values = c("#009E73", "#000000", "#E69F00", "#56B4E9", "#0072B2", "#D55E00"),
        labels = c("7711903", "7711603", "7711703", "7711803", "7712103", "7713303")
      )
    )
  )


length(frames) # number of frames
frames[[1300]] # display one of the frames to check

# Write MP4 file ----------------------------------------------------------
animate_frames(frames, out_file = "data output/Animated_BARSTR_tracks.mp4", fps = 25)


