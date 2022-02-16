## ________________________________________________________________________

## Title:   06_Plot BARSTR tracks.R
## Purpose: Re-create Figure 4 in manuscript
## Author:  Dominic Henry
## Date:    16/02/2022

## Libraries
library(tidyverse)
library(sf)
library(ggspatial)
## ________________________________________________________________________


# Define projections ------------------------------------------------------
aeaproj <- "+proj=aea +lat_1=20 +lat_2=-23 +lat_0=0 +lon_0=25 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m"
latlongCRS <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"


# Import Africa shapefile -------------------------------------------------
africa <- read_sf("data input/Africa2.shp", crs = latlongCRS)

# Import BARSTR tracks ----------------------------------------------------
barstr_ptt <- c("7711603","7711703", "7711803", "7711903", "7712103", "7713303")

barstr_tracks <- glue::glue("data input/{barstr_ptt}.shp")  %>%
  map(read_sf) %>%
  do.call(rbind, .) %>%
  mutate(birdID = barstr_ptt) %>%
  st_transform(crs = latlongCRS)


# Plot species tracks -----------------------------------------------------
ggplot()+
  geom_sf(data = africa, fill = "palegreen", color = alpha("black",0.6))+
  geom_sf(data = barstr_tracks, col = "black", size = 0.8)+
  coord_sf(xlim = c(14, 25), ylim = c(-35, -29), expand = TRUE) +
  annotation_scale(location = "tl", width_hint=0.4) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         height = unit(0.9,"cm"), width = unit(0.6,"cm")) +
  theme(panel.background = element_rect(fill = "lightblue", colour = "black", size = 0.5),
        panel.grid.major = element_line(size = 0.05, colour = "blue"),
        strip.background = element_rect(fill = NA, color = "black"),
        strip.text = element_text(size = 18),
        axis.text = element_text(size = 14))+
  facet_wrap(~birdID)

ggsave("data output/BARSTR_track_plot.pdf", width = 12, height = 8)

