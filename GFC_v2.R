# Hansen Global Forest Change Dataset (2000-2017)
# Set working directory
setwd("/Users/jacquelineliu/Desktop/Thesis_Data/Input")

# Load packages
library(sf)
library(ggplot2)
library(rgdal)
library(sp)
library(raster)
source("software/polygonizer.R")
library(spex)
library(dplyr)
library(tidyverse)

# Load Indonesia shape file
IDN <- st_read("gadm36_IDN_shp/gadm36_IDN_1.shp")
plot(st_geometry(IDN), col = "grey")

# Load raster layers 
treecover <- raster("gfc/treecover2000.tif")
lossyear <- raster("gfc/lossyear.tif")
last <- raster("gfc/last.tif")
gain <- raster ("gfc/gain.tif")
first <- raster("gfc/first.tif")
datamask <- raster("gfc/datamask.tif")

#Set mercator projection
mercator = "+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
#Corresponding EPSG = 41001

# Construct datasets by Province
# (1) Jambi 
#jambi <- filter(villages_podes, province_name == "JAMBI")

jambi <- filter(IDN, NAME_1=="Jambi")

lossyear_jambi_mask <- mask(lossyear, mask = jambi)
lossyear_jambi_crop <- crop(lossyear_jambi_mask, jambi)
plot(lossyear_jambi_crop)

#treecover_jambi <- crop(treecover, jambi_buf_sp)
treecover_jambi_mask <- mask(treecover, mask = jambi)
treecover_jambi_crop <- crop(treecover_jambi_mask, jambi)
plot(treecover_jambi_crop)

#gain_jambi <- crop(gain, jambi_buf_sp)
gain_jambi_mask <- mask(gain, mask = jambi)
gain_jambi_crop <- crop(gain_jambi_mask, jambi)
plot(gain_jambi_crop)

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
saveRDS(forestcover_jambi_clean, file = "jambi.Rda")

# Limiting raster to study areas in Jambi
protection <- st_read("Test/Tamonarang_Prot.shp")
production <- st_read("Test/Tamonarang_Production.shp")
hd <- st_read("Test/Tamonarang_HD.shp")

# Protection area
lossyear_jprot <- mask(lossyear, mask = protection)
plot(lossyear_jprot)

lossyear_jprot_crop <- crop(lossyear_jprot, protection)
plot(lossyear_jprot_crop)


# Rename column names for 'x' and 'y'
#colnames(forestcover_jambi_clean)[colnames(forestcover_jambi_clean)=="x"] <- "Longitude"
#colnames(forestcover_jambi_clean)[colnames(forestcover_jambi_clean)=="y"] <- "Latitude"
#head(forestcover_jambi_clean)
#summary(forestcover_jambi_clean)
#str(forestcover_jambi_clean)

# (2) Riau
riau <- filter(villages_podes, province_name == "RIAU")

# Load raster layers (different tile from Jambi)

# Limiting raster to extent of Riau province
lossyear_riau <- crop(lossyear, riau_buf_sp)
lossyear_riau_mask <- mask(lossyear_riau, mask = riaupoly)
lossyear_riau_crop <- crop(lossyear_riau_mask, riaupoly)
plot(lossyear_riau_mask)
plot(lossyear_riau_crop)

treecover_riau <- crop(treecover, riau_buf_sp)
treecover_riau_mask <- mask(treecover_riau, mask = riaupoly)
treecover_riau_crop <- crop(treecover_riau_mask, riaupoly)
plot(treecover_riau_crop)

gain_riau <- crop(gain, riau_buf_sp)
gain_riau_mask <- mask(gain_riau, mask = riaupoly)
gain_riau_crop <- crop(gain_riau_mask, riaupoly)
plot(gain_riau_crop)

#Set mercator projection
mercator = "+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
#Corresponding EPSG = 41001

#Create Raster stack for Riau
crs(treecover_riau_crop) <- mercator
crs(lossyear_riau_crop) <- mercator
crs(gain_riau_crop) <- mercator

forestcover_riau = stack(treecover_riau_crop, lossyear_riau_crop, gain_riau_crop)
head(forestcover_riau)

forestcover_riau_sf <- as.data.frame(forestcover_riau, xy = TRUE)
head(forestcover_riau_sf)
# The dataframe contains many NA values due to whitespace

# Removing NA values from Riau forestcover dataframe 
sum(is.na(forestcover_riau_sf))
forestcover_riau_clean <- na.omit(forestcover_riau_sf)
sum(is.na(forestcover_riau_clean))
# NA values removed

head(forestcover_riau_clean)

# Rename column names for 'x' and 'y'
colnames(forestcover_riau_clean)[colnames(forestcover_riau_clean)=="x"] <- "Longitude"
colnames(forestcover_riau_clean)[colnames(forestcover_riau_clean)=="y"] <- "Latitude"
head(forestcover_riau_clean)
summary(forestcover_riau_clean)
str(forestcover_riau_clean)

# Limiting raster to study areas in Riau
