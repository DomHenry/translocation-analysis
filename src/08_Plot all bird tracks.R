## ________________________________________________________________________

## Title:   08_Plot all bird tracks.R
## Purpose: Re-create geese track maps in Figures 3a and 3b in manuscript
## Author:  Graeme S. Cumming
## Date:    16/02/2022

## Libraries
library(sf)
library(tidyverse)
library(glue)
library(lubridate)
library(raster)
library(ggspatial)
## ________________________________________________________________________

aeaproj <- "+proj=aea +lat_1=20 +lat_2=-23 +lat_0=0 +lon_0=25 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m"

latlong <- "+proj=longlat +datum=WGS84 +ellps=WGS84"

sf2 <- st_read("data input/Africa2.shp", crs = latlong) %>%
  st_transform(crs = aeaproj)

sf2 <- as(sf2, "Spatial")

# read in bird track data
ptt.summary.file <- ("data input/EGtraxR.xlsx")
ptt.summary <- readxl::read_xlsx(ptt.summary.file)
ptt.summary <- ptt.summary[-c(6, 19, 21), ]
# remove birds with less than a month of data

names <- ptt.summary$PTTID
names_lines <- paste("data input/", names, ".shp", sep = "")
names_points <- paste("data input/", names, "_pts.shp", sep = "")
nbirds <- dim(ptt.summary)[1]

# loop through each bird using metadata in EGtraxR
# variable names birds1 to birds35 now represent individual shapefiles
for (j in 1:nbirds) {
  varname <- paste("bird", j, sep = "")
  assign(varname, shapefile(names_lines[j]))
}

for (j in 1:nbirds) {
  varname2 <- paste("bird", j, ".pts", sep = "")
  assign(varname2, shapefile(names_points[j]))
}

# merge all bird files into a single massive shapefile
allbirds <- shapefile(names_lines[1])
for (j in 2:nbirds) {
  allbirds <- rbind(allbirds, shapefile(names_lines[j]))
}

allbirds$id <- names

sz <- 0.8
# function to generate colours spread across gradient
my.cols <- function(n) {
  if (n <= 9) {
    c(brewer.pal(n - 1, "Set2"))
  } else {
    c(hcl(h = seq(0, (n - 1) / (n),
      length = n
    ) * 360, c = 100, l = 65, fixup = TRUE))
  }
}

my.palette <- my.cols(35)
my.palette <- rev(my.palette)
my.palette[27] <- "#FFFF00"
# set to yellow
my.palette[33] <- "#9ACD32"
# yellowgreen
my.palette[22] <- "#FFFF99"
# light yellow
Legend <- as.character(allbirds$id)
allbirds.residents <- allbirds[-c(5, 7, 10, 13, 17, 33), ]
# plot of all bird data without translocated birds
my.palette <- my.cols(29)
my.palette <- rev(my.palette)
my.palette[17] <- "#FFFF99"
# light yellow
my.palette[22] <- "#FFFF00"
# set to yellow


Legend <- as.character(allbirds.residents$id)
ggplot() +
  theme(
    panel.background = element_rect(fill = "lightblue", colour = "black", size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.2, linetype = "solid", colour = "blue"),
    panel.grid.minor = element_line(size = 0.25, linetype = "solid", colour = "white")
  ) +
  geom_polygon(data = sf2, aes(x = long, y = lat, group = group), fill = "palegreen", color = "black") +
  layer_spatial(data = allbirds.residents, size = sz, aes(allbirds.residents$id, col = Legend, fill = "National boundaries")) +
  scale_color_manual(values = my.palette) +
  coord_sf(ylim = c(-3900000, -1800000), xlim = c(-1400000, 1100000)) +
  annotation_scale(location = "tl", width_hint = 0.4) +
  annotation_north_arrow(location = "bl", which_north = "true") +
  guides(fill = guide_legend(title = "Countries")) +
  theme(
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 15)
  )

ggsave("data output/all_resident_birds_tracks.pdf", width = 16, height = 9)

# to customise colours in my.palette:
bird.cols <- cbind(my.palette, allbirds.residents$id)
# see which bird has which colour
my.palette[29] <- "#FFFF00"
# set to yellow

tbirds <- c(names[5], names[7], names[10], names[13], names[17], names[33])

sz <- 0.9

ggplot() +
  theme(
    panel.background = element_rect(fill = "lightblue", colour = "black", size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.2, linetype = "solid", colour = "blue"),
    panel.grid.minor = element_line(size = 0.25, linetype = "solid", colour = "white")
  ) +
  geom_polygon(data = sf2, aes(x = long, y = lat, group = group), fill = "palegreen", color = "black") +
  # translocated birds are numbers 5, 8, 11, 14, 18, 36 but 6, 19, 21 were removed & new numbers are row#
  layer_spatial(data = bird17, color = "yellow", size = sz) +
  layer_spatial(data = bird5, color = "orange", size = sz) +
  layer_spatial(data = bird7, color = "red", size = sz) +
  layer_spatial(data = bird13, color = "brown", size = sz) +
  layer_spatial(data = bird33, color = "darkgreen", size = sz) +
  layer_spatial(data = bird10, color = "blue", size = sz) +
  layer_spatial(data = bird17.pts, color = "yellow") +
  layer_spatial(data = bird5.pts, color = "orange") +
  layer_spatial(data = bird7.pts, color = "red") +
  layer_spatial(data = bird13.pts, color = "brown") +
  layer_spatial(data = bird33.pts, color = "darkgreen") +
  layer_spatial(data = bird10.pts, color = "blue") +
  coord_sf(ylim = c(-3935000, -3352000), xlim = c(-1093000, -1000)) +
  annotation_scale(location = "tl", width_hint = 0.4) +
  annotation_north_arrow(location = "bl", which_north = "true")

ggsave("data output/translocated_birds_maps.pdf", width = 16, height = 9)
