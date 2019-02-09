# Hansen Global Forest Change Dataset (2000-2017)
# Set working directory
setwd("/Users/jacquelineliu/Desktop/Thesis_Data/Input")

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
villages2013 <- st_read("villages/podes_bps2014/podes_bps2014.shp")
#plot(st_geometry(villages2013), lwd = 0.001)
load("podes/PODES_panel.RData") 

villages_podes <- inner_join(villages2013,PODES.panel,by = c("ID2013" = "Id2014"))

# Keep only transmigration villages (according to PODES 2014 variable); just selecting one village here for testing
transmigrasi_podes <- filter(villages_podes,village_status == 3 & PROVINSI == "SUMATERA SELATAN")

# Creating a 0.1 arc-degree (~ 50 km) buffer around relevant areas
transmigrasi_podes_buf <- st_buffer(transmigrasi_podes, dist = 0.1)

# Load raster layers 
treecover <- raster("gfc/treecover2000.tif")
lossyear <- raster("gfc/lossyear.tif")
last <- raster("gfc/last.tif")
gain <- raster ("gfc/gain.tif")
first <- raster("gfc/first.tif")
datamask <- raster("gfc/datamask.tif")

#Check CRS
crs(treecover, asText = TRUE)
crs(lossyear, asText = TRUE)
crs(last, asText = TRUE)
crs(gain, asText = TRUE)
crs(first, asText = TRUE)
crs(datamask, asText = TRUE)
# CRS is unprojected. All raster files contain same CRS string

#Set mercator projection
mercator = "+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
#Corresponding EPSG = 41001

# Limiting raster to the extent of shapefile
transmigrasi_podes_buf_sp <- as(transmigrasi_podes_buf,"Spatial")

lossyear_crop <- crop(lossyear,transmigrasi_podes_buf_sp)
loss_year_small <- mask(lossyear_crop, mask = transmigrasi_podes_buf_sp)
plot(lossyear_crop)
crs(lossyear_crop)

treecover_crop <- crop(treecover, transmigrasi_podes_buf_sp)
plot(treecover_crop)

gain_crop <- crop(gain, transmigrasi_podes_buf_sp)
plot(gain_crop)

plot(st_geometry(transmigrasi_podes), add = TRUE)
# The village looks quite small


#Create Raster stack 
crs(treecover_crop) <- mercator
crs(lossyear_crop) <- mercator
crs(gain_crop) <- mercator

forestcover = stack(treecover_crop, lossyear_crop, gain_crop)
head(forestcover)

forestcover_sf <- as.data.frame(forestcover, xy = TRUE)
head(forestcover_sf)

View(forestcover_sf)

summary(forestcover_sf)

colnames(forestcover_sf)[colnames(forestcover_sf)=="x"] <- "Longitude"
colnames(forestcover_sf)[colnames(forestcover_sf)=="y"] <- "Latitude"
head(forestcover_sf)


# In order to get buffer zones in metres we should use a common projection (later we can also create buffers individually for each UTM zone)
#transmigrasi_podes <- st_transform(transmigrasi_podes, crs = mercator)
#lossyear <- projectRaster(lossyear, crs = mercator, method = "ngb")
# SK: We will probably only need lossyear, gain, treecover; lossyear is the most important. 
# We could use treecover to control for the level of treecover in 2000, but in the literature 
# this is rather done by using Margono's map for primary forest and restrict the study to that area

# This is a step we will do at the very end before exporting to Stata (we don't need the geo information then anymore)
sumatra_sf <- as.data.frame(sumatra_red, xy = TRUE)
