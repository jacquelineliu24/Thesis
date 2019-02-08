# Hansen Global Forest Change Dataset (2000-2017)
# Set working directory
setwd("C:/Users/kras/ownCloud/research/research/papers/IND/land_titles")

# Load packages
library(sf)
library(rgdal)
library(sp)
# SK: You need to load the "raster" library too
library(raster)
source("software/polygonizer.R")
library(spex)
library(dplyr)

# Load village shape file

villages2013 <- st_read("download/input/villages/podes_bps2014/podes_bps2014.shp")
#plot(st_geometry(villages2013), lwd = 0.001)
load("download/input/podes/PODES_panel.RData") 

villages_podes <- inner_join(villages2013,PODES.panel,by = c("ID2013" = "Id2014"))

# Keep only transmigration villages (according to PODES 2014 variable); just selecting one village here for testing
transmigrasi_podes <- filter(villages_podes,village_status == 3 & PROVINSI == "SUMATERA SELATAN")

# Creating a 0.1 arc-degree (~ 50 km) buffer around relevant areas
transmigrasi_podes_buf <- st_buffer(transmigrasi_podes, dist = 0.1)

# Load raster layers 
treecover <- raster("download/input/gfc/Hansen_GFC-2017-v1.5_treecover2000_00N_100E.tif")
lossyear <- raster("download/input/gfc/Hansen_GFC-2017-v1.5_lossyear_00N_100E.tif")
last <- raster("download/input/gfc/Hansen_GFC-2017-v1.5_last_00N_100E.tif")
gain <- raster ("download/input/gfc/Hansen_GFC-2017-v1.5_gain_00N_100E.tif")
first <- raster("download/input/gfc/Hansen_GFC-2017-v1.5_first_00N_100E.tif")
datamask <- raster("download/input/gfc/Hansen_GFC-2017-v1.5_datamask_00N_100E.tif")

# Limiting raster to the extent of shapefile
transmigrasi_podes_buf_sp <- as(transmigrasi_podes_buf,"Spatial")

lossyear_crop <- crop(lossyear,transmigrasi_podes_buf_sp)
loss_year_small <- mask(lossyear_crop, mask = transmigrasi_podes_buf_sp)



plot(lossyear_crop)
plot(st_geometry(transmigrasi_podes), add = TRUE)
# The village looks quite small

# Check CRS 
crs(treecover, asText = TRUE)
crs(lossyear, asText = TRUE)
crs(last, asText = TRUE)
crs(gain, asText = TRUE)
crs(first, asText = TRUE)
crs(datamask, asText = TRUE)
# CRS is unprojected. All raster files contain same CRS string

# In order to get buffer zones in metres we should use a common projection (later we can also create buffers individually for each UTM zone)

# mercator = "+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
# transmigrasi_podes <- st_transform(transmigrasi_podes, crs = mercator)
# lossyear <- projectRaster(lossyear, crs = mercator, method = "ngb")

# Combine raster files 
sumatra <- merge(lossyear, gain)
# SK: We will probably only need lossyear, gain, treecover; lossyear is the most important. 
# We could use treecover to control for the level of treecover in 2000, but in the literature 
# this is rather done by using Margono's map for primary forest and restrict the study to that area

# Transform into sf object
# Check size of 'sumatra'
res(sumatra)
ncell(sumatra)
# Reduce size of 'sumatra' by factor of 10
# SK: This is not necessary. Since the cell is our unit of observation we want to keep all of them.
sumatra_red <- aggregate(sumatra, fact = 10, fun = mean)
res(sumatra_red)
ncell(sumatra_red)

# Convert 'sumatra_red' into data frame 
# This is a step we will do at the very end before exporting to Stata (we don't need the geo information then anymore)
sumatra_sf <- as.data.frame(sumatra_red, xy = TRUE)

# Turn pixels into polygons
lossyear_sf <- polygonize(lossyear_crop,na.rm = FALSE)