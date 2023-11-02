## ----knitr-setup, include = FALSE---------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----load-libraries, echo=FALSE, message=FALSE--------------------------------
library("climenv")
library("fs")
library("raster")
library("dplyr")
library("sf")
library("terra")

## ----training-data------------------------------------------------------------
# Let's make some training data

# Create temporary file to supply to the ce_extract
temp_path <- tempfile()

# Create the  sub-folders
dir.create(file.path(temp_path, "elev"), recursive = TRUE)
dir.create(file.path(temp_path, "prec"), recursive = TRUE)
dir.create(file.path(temp_path, "tmax"), recursive = TRUE)
dir.create(file.path(temp_path, "tavg"), recursive = TRUE)
dir.create(file.path(temp_path, "tmin"), recursive = TRUE)

# Create a empty raster serving as a base
r <- terra::rast(ncol = 10, nrow = 10)

# Modify the base Raster values and save them in correct configuration

#* Elevation 100m ####
terra::values(r) <- 1:100
terra::writeRaster(r, paste0(temp_path, "/elev/srtm.tif"))

# Prec
x <- c(5, 10, 15, 20, 25, 34.40666, 25, 20, 15, 10, 5, 0) * 8
temp2 <- paste0("prec_", sprintf("%02d", 1:12), ".tif")
for (i in seq_along(temp2)) {
  terra::values(r) <- x[i]
  terra::writeRaster(r, paste0(temp_path, "/prec/", temp2[i]))
}

# tmax
x <- c(43, 38, 33, 29, 25, 19.8, 17.01, 21, 25, 30, 37, 44)
temp2 <- paste0("tmax_", sprintf("%02d", 1:12), ".tif")
for (i in seq_along(temp2)) {
  values(r) <- x[i]
  writeRaster(r, paste0(temp_path, "/tmax/", temp2[i]))
}

# tmin
x <- c(43, 38, 33, 29, 25, 19.8, 17.01, 21, 25, 30, 37, 44) / 2
temp2 <- paste0("tmin_", sprintf("%02d", 1:12), ".tif")
for (i in seq_along(temp2)) {
  values(r) <- x[i]
  writeRaster(r, paste0(temp_path, "/tmin/", temp2[i]))
}

# tmean
x <- c(43, 38, 33, 29, 25, 19.8, 17.01, 21, 25, 30, 37, 44) -
  c(c(43, 38, 33, 29, 25, 19.8, 17.01, 21, 25, 30, 37, 44) / 2) / 2
temp2 <- paste0("tavg_", sprintf("%02d", 1:12), ".tif")
for (i in seq_along(temp2)) {
  values(r) <- x[i]
  writeRaster(r, paste0(temp_path, "/tavg/", temp2[i]))
}

# Create a polygon file from the raster
terra::values(r) <- 1:100
pol_py <- sf::st_as_sf(terra::as.polygons(r))
pol_py$grp <- c(rep("low", 25), rep("high", 75))

# Create a point file from the raster
pol_pt <- sf::st_as_sf(terra::as.points(r))
pol_pt$grp <- c(rep("low", 25), rep("high", 75))


## ----extract-data-------------------------------------------------------------
# Extract the climate data

data_py <- ce_extract(
  file.path(temp_path),
  location = pol_py,
  location_g = NULL
)

data_pt <- ce_extract(
  file.path(temp_path),
  location = pol_pt,
  location_g = NULL
)

# Remove temporary data
unlink(file.path(temp_path))


## ----holdridge-diagram, fig.height = 5, fig.width = 5, fig.align = "center", fig.cap="Fig 1. Position of the training data within Holdridge's (1967) life zone classification. The surface shading in the background is new addition to the original life zone classification and helps interpretation by converting a point in evapotranspiration-precipitation space to an appropriate cross-blended hypsometric colour – in this intuitive instance colours tending towards the red spectrum feature higher temperatures blended with lower precipitation compared while colours tending towards the blue colour spectrum have lower temperatures and higher precipitation."----
# Make Holdridge's (1967) life zone classification diagram
plot_h(data = data_py, "0")

## ----walter-leigh-diagram, fig.height = 5, fig.width = 5, fig.align = "center", fig.cap="Fig 2. Walter-Lieth’s climatic diagram (1960) of the training data. When precipitation is > 100 mm, the scale increases from 2mm C-1 to 20 mm  C-1 (as indicated by the black horizontal line) to avoid too high diagrams in very wet locations. This change is indicated by a black horizontal line, and the graph over is filled in solid blue. When the precipitation graph lies under the temperature graph (P < 2T) we have an arid period (filled in dotted red vertical lines). Otherwise the period is considered humid (filled in light blue). Daily maximum average temperature of the hottest month and daily minimum average temperature of the coldest month are labeled in black on the left margin of the diagram."----
# Make Walter-Leigh's (1960) climate diagram
plot_wl(data = data_py, "0")

## ----custom-climate-diagram, fig.height = 4, fig.width = 7, fig.align = "center", fig.cap="Fig 3. Custom diagram showing the climatic envelope of the training data. The abbreviations used are as follow: biotemperture (BioT), isothermality (ISO), mean annual temperature (MAT), temperature seasonality (TS), number of dry months with < 50 mm rainfall during the month (Dry mo), mean annual precipitation (MAP), potential evapotranspiration (PET), precipitation seasonality (PS), seasonal rainfall percentage in Summer (S), Autumn (A), Winter (W), Vernal (V), elevation (Elv) and latitude (Lat)."----
# Make the custom climate diagram
oldpar <- par(mar = c(1.5, 2.8, 2, 17))
plot_c(data = data_py, "0")
par(oldpar)

