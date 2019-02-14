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
# Converting the final dataset into a dataframe: sumatra_sf <- as.data.frame(sumatra_red, xy = TRUE)

# 10 Feb 2019: 
# Construct datasets by Province
# (1) Jambi 
jambi <- filter(villages_podes, province_name == "JAMBI")

# Creating a 0.1 arc-degree (~ 50 km) buffer around relevant areas
# transmigrasi_podes_buf <- st_buffer(transmigrasi_podes, dist = 0.1)
jambi_buf <- st_buffer(jambi, dist = 0.1)

# Limiting raster to the extent of shapefile
# transmigrasi_podes_buf_sp <- as(transmigrasi_podes_buf,"Spatial")
jambi_buf_sp <- as(jambi_buf, "Spatial")

# Experimenting with plotting outline of Jambi province
jambi_bigpoly <- st_union(jambi)
plot(jambi_bigpoly)

# Drop Z dimension of polygon
jambi_bigpoly_XY <- st_zm(jambi_bigpoly, drop = TRUE, what = "ZM")

# Convert polygon into spatial object
jambipoly <- as(jambi_bigpoly_XY, "Spatial")

# Limiting raster to extent of Jambi province
lossyear_jambi <- crop(lossyear, jambi_buf_sp)
lossyear_jambi_mask <- mask(lossyear_jambi, mask = jambipoly)
lossyear_jambi_crop <- crop(lossyear_jambi_mask, jambipoly)
plot(lossyear_jambi_mask)
plot(lossyear_jambi_crop)

treecover_jambi <- crop(treecover, jambi_buf_sp)
treecover_jambi_mask <- mask(treecover_jambi, mask = jambipoly)
treecover_jambi_crop <- crop(treecover_jambi_mask, jambipoly)
plot(treecover_jambi_mask)

gain_jambi <- crop(gain, jambi_buf_sp)
gain_jambi_mask <- mask(gain_jambi, mask = jambipoly)
gain_jambi_crop <- crop(gain_jambi_mask, jambipoly)
plot(gain_jambi_mask)

#Set mercator projection
mercator = "+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
#Corresponding EPSG = 41001

#Create Raster stack for Jambi
crs(treecover_jambi_crop) <- mercator
crs(lossyear_jambi_crop) <- mercator
crs(gain_jambi_crop) <- mercator

forestcover_jambi = stack(treecover_jambi_crop, lossyear_jambi_crop, gain_jambi_crop)
head(forestcover_jambi)

forestcover_jambi_sf <- as.data.frame(forestcover_jambi, xy = TRUE)
head(forestcover_jambi_sf)
# The dataframe contains many NA values due to whitespace

# Removing NA values from Jambi forestcover dataframe 
sum(is.na(forestcover_jambi_sf))
forestcover_jambi_clean <- na.omit(forestcover_jambi_sf)
sum(is.na(forestcover_jambi_clean))
# NA values removed

head(forestcover_jambi_clean)

# Rename column names for 'x' and 'y'
colnames(forestcover_jambi_clean)[colnames(forestcover_jambi_clean)=="x"] <- "Longitude"
colnames(forestcover_jambi_clean)[colnames(forestcover_jambi_clean)=="y"] <- "Latitude"
head(forestcover_jambi_clean)
summary(forestcover_jambi_clean)
str(forestcover_jambi_clean)

# Edits as of 14 Feb 2019: 
# (2) Sumatra Selantan
sumsel <- filter(villages_podes, province_name == "SUMATERA SELATAN")

# Creating a 0.1 arc-degree (~ 50 km) buffer around relevant areas
# transmigrasi_podes_buf <- st_buffer(transmigrasi_podes, dist = 0.1)
sumsel_buf <- st_buffer(sumsel, dist = 0.1)

# Limiting raster to the extent of shapefile
# transmigrasi_podes_buf_sp <- as(transmigrasi_podes_buf,"Spatial")
sumsel_buf_sp <- as(sumsel_buf, "Spatial")

# Experimenting with plotting outline of Sumatra Selatan province
sumsel_bigpoly <- st_union(sumsel)
plot(sumsel_bigpoly)

# Drop Z dimension of polygon
sumsel_bigpoly_XY <- st_zm(sumsel_bigpoly, drop = TRUE, what = "ZM")

# Convert polygon into spatial object
sumselpoly <- as(sumsel_bigpoly_XY, "Spatial")
plot(sumselpoly, axes = TRUE)

# Limiting raster to extent of Sumatra Selatan province
lossyear_sumsel <- crop(lossyear, sumsel_buf_sp)
lossyear_sumsel_mask <- mask(lossyear_sumsel, mask = sumselpoly)
lossyear_sumsel_crop <- crop(lossyear_sumsel_mask, sumselpoly)
plot(lossyear_sumsel_mask)
plot(lossyear_sumsel_crop)

treecover_sumsel <- crop(treecover, sumsel_buf_sp)
treecover_sumsel_mask <- mask(treecover_sumsel, mask = sumselpoly)
treecover_sumsel_crop <- crop(treecover_sumsel_mask, sumselpoly)
plot(treecover_sumsel_crop)

gain_sumsel <- crop(gain, sumsel_buf_sp)
gain_sumsel_mask <- mask(gain_sumsel, mask = sumselpoly)
gain_sumsel_crop <- crop(gain_sumsel_mask, sumselpoly)
plot(gain_sumsel_crop)

par(mfrow = c(2,2))
plot(lossyear_sumsel_crop)
plot(treecover_sumsel_crop)
plot(gain_sumsel_crop)

#Set mercator projection
mercator = "+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
#Corresponding EPSG = 41001

#Create Raster stack for Sumatra Selatan
crs(treecover_sumsel_crop) <- mercator
crs(lossyear_sumsel_crop) <- mercator
crs(gain_sumsel_crop) <- mercator

forestcover_sumsel = stack(treecover_sumsel_crop, lossyear_sumsel_crop, gain_sumsel_crop)
head(forestcover_sumsel)

forestcover_sumsel_sf <- as.data.frame(forestcover_sumsel, xy = TRUE)
head(forestcover_sumsel_sf)
# The dataframe contains many NA values due to whitespace

# Removing NA values from Sumatra Selatan forestcover dataframe 
sum(is.na(forestcover_sumsel_sf))
forestcover_sumsel_clean <- na.omit(forestcover_sumsel_sf)
sum(is.na(forestcover_sumsel_clean))
# NA values removed

head(forestcover_sumsel_clean)

# Rename column names for 'x' and 'y'
colnames(forestcover_sumsel_clean)[colnames(forestcover_sumsel_clean)=="x"] <- "Longitude"
colnames(forestcover_sumsel_clean)[colnames(forestcover_sumsel_clean)=="y"] <- "Latitude"
head(forestcover_sumsel_clean)
summary(forestcover_sumsel_clean)
str(forestcover_sumsel_clean)

# (3) Riau

