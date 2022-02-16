## ________________________________________________________________________

## Title:    02_NSD analysis BARSTR.R
## Purpose:  Analyse movement tracks of BARSTR Egyptian Geese within a net-squared distance
##           framework to identify modes of movement
## Author:   Dominic Henry
## Date:     16/02/2022

## Libraries
library(adehabitatLT)
library(sf)
library(tidyverse)
library(glue)
library(lubridate)
library(migrateR)
## ________________________________________________________________________

barstrbirds <- c("7711603","7711703")

# Create R project output directories -------------------------------------
dir.create("data output/BARSTR bursts")

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

# Read in and process point data ------------------------------------------

# Define the number of years of data to extract for each PTT
nyears <- c(2,2)

import_tracks <- function(z, yearselect){

  bird_pts_all <- st_read(glue("data input/{z}_pts.shp")) %>%
    sfc_as_cols %>%
    select(date, x, y) %>%
    st_drop_geometry() %>%
    as_tibble() %>%
    mutate(ptt = z,
           date = ymd_hms(date),
           day = day(date),
           month = month(date),
           year = year(date))

  ## Thin to one point per day
  bird_pts <- bird_pts_all %>%
    group_by(day, month, year) %>%
    sample_n(1) %>%
    arrange(date) %>%
    ungroup()

  ## Filter years & add migration year' variable
  bird_pts <- bird_pts %>%
    filter(date < bird_pts$date[1] + dyears(yearselect)) %>%
    mutate(dayref = 1:nrow(.))

  start <- ymd("2015-06-09")
  year1 <- interval(start, start + years(1))
  year2 <- interval(start + years(1), start + years(2))
  year3 <- interval(start + years(2), start + years(3))

  bird_pts <- bird_pts %>%
    mutate(burst_id = case_when(
      date %within% year1 ~ str_c(z,"_birdyear1"),
      date %within% year2 ~ str_c(z,"_birdyear2"),
      date %within% year3 ~ str_c(z,"_birdyear3")
    ))

}

tracks <- map2_dfr(.x = barstrbirds, .y = nyears,
                   .f = import_tracks)

tracks

# Write bursts to file ----------------------------------------------------
albers_proj <- st_crs(st_read(glue("data input/7711802_pts.shp")))

bursts <- unique(tracks$burst_id)
bursts <- bursts[which(!is.na(bursts))]

walk(bursts,
     .f = function (z){ # Don't use the same name as a column (i.e. x)

       tracks %>%
         filter(burst_id %in% z) %>%
         st_as_sf(coords = c("x","y"),
                  crs = albers_proj
         ) %>%
         st_write(glue("data output/BARSTR bursts/{z}.shp"),
                  overwrite = TRUE, delete_dsn = TRUE)


     })


# Create ltraj object -----------------------------------------------------
eg_tracks <- as.ltraj(xy = tracks[,c(2,3)],
                      date = tracks$date,
                      id = tracks$ptt,
                      burst = tracks$burst_id,
                      typeII = TRUE)
plot(eg_tracks)
summary.ltraj(eg_tracks)
plotltr(eg_tracks, "R2n")

# NSD analysis ------------------------------------------------------------
stdt <- "06-19"
pest.n <- pEst()

## Run movement models
nsd <- mvmtClass(eg_tracks, fam = "nsd",
                 p.est = pest.n,
                 stdt = stdt,
                 warnOnly = FALSE # Use to interrogate all models
)
## Check results
summary(nsd)

## Missing models from each burst
sum(!fullmvmt(nsd)) # Total bursts with missing data
fullmvmt(nsd, out = "name")
fullmvmt(nsd, out = "logic")
fullmvmt(nsd, out = "numer")
all(fullmvmt(nsd))

## Plot checks
plot.mvmt(nsd[["7711603_birdyear1"]])
plot.mvmts(nsd)

## Write plots
pdf("data output/NSD plots/NSD_model_plots_BARSTR.pdf", width = 5, height = 4)
for(i in 1:length(nsd)){
  plot.mvmt(nsd[[i]])
}
dev.off()


# END ---------------------------------------------------------------------


