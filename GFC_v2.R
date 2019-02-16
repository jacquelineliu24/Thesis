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

# Add column for Province
forestcover_jambi_clean$Province <- c("Jambi")

# Save dataset
saveRDS(forestcover_jambi_clean, file = "jambi.Rda")

# Limiting raster to study areas in Jambi
protection <- st_read("Polygons/Jambi_1_GR_Tamonarang_Protection.shp")
production <- st_read("Polygons/Jambi_1_GR_Tamonarang_Production.shp")
hd <- st_read("Polygons/Jambi_1_GR_Tamonarang_HD.shp")

# Protection area
lossyear_jprot <- mask(lossyear_jambi_crop, mask = protection)
lossyear_jprot_crop <- crop(lossyear_jprot, protection)
plot(lossyear_jprot_crop)

treecover_jprot <- mask(treecover_jambi_crop, mask = protection)
treecover_jprot_crop <- crop(treecover_jprot, protection)
plot(treecover_jprot_crop)

gain_jprot <- mask(gain_jambi_crop, mask = protection)
gain_jprot_crop <- crop(gain_jprot, protection)
plot(gain_jprot_crop)

# Create Raster stack for protection area
crs(lossyear_jprot_crop) <- mercator
crs(treecover_jprot_crop) <- mercator
crs(gain_jprot_crop) <- mercator 

forestcover_jprot = stack(lossyear_jprot_crop, treecover_jprot_crop, gain_jprot_crop)
forestcover_jprot_sf <- as.data.frame(forestcover_jprot, xy = TRUE)

sum(is.na(forestcover_jprot_sf))
forestcover_jambi_clean <- na.omit(forestcover_jprot_sf)
sum(is.na(forestcover_jprot_clean))

# Add column(s) to code Forest Area type and Land title type 
forestcover_jprot_clean$Province <- c("Jambi")
forestcover_jprot_clean$Forest_Area <- c("Protection")
forestcover_jprot_clean$Concession <- c("NA")
forestcover_jprot_clean$Land_Title <- c("State")

# Save as separate dataset
head(forestcover_jprot_clean)
saveRDS(forestcover_jprot_clean, file = "jambi_protection.Rda")

# Production area 
lossyear_jprod <- mask(lossyear_jambi_crop, mask = production)
lossyear_jprod_crop <- mask(lossyear_jprod, mask = production)
plot(lossyear_jprod_crop)

treecover_jprod <- mask(treecover_jambi_crop, mask = production)
treecover_jprod_crop <- crop(treecover_jprot, production)
plot(treecover_jprod_crop)

gain_jprod <- mask(gain_jambi_crop, mask = production)
gain_jprod_crop <- crop(gain_jprod, production)
plot(gain_jprod_crop)

# Create Raster stack for production area
crs(lossyear_jprod_crop) <- mercator
crs(treecover_jprod_crop) <- mercator
crs(gain_jprod_crop) <- mercator 

forestcover_jprod = stack(lossyear_jprod_crop, treecover_jprod_crop, gain_jprod_crop)
forestcover_jprod_sf <- as.data.frame(forestcover_jprod, xy = TRUE)

sum(is.na(forestcover_jprod_sf))
forestcover_jambi_clean <- na.omit(forestcover_jprod_sf)
sum(is.na(forestcover_jprod_clean))

# Add column(s) to code Province, Forest Area type, Concession type and Land title type 
forestcover_jprod_clean$Province <- c("Jambi")
forestcover_jprod_clean$Forest_Area <- c("Production")
forestcover_jprod_clean$Concession <- c("NA")
forestcover_jprod_clean$Land_Title <- c("State")

# Save as separate dataset
head(forestcover_jprod_clean)
saveRDS(forestcover_jprod_clean, file = "jambi_production.Rda")

# HD area
lossyear_hd <- mask(lossyear_jambi_crop, mask = hd)
lossyear_hd_crop <- mask(lossyear_hd, mask = hd)
plot(lossyear_hd_crop)

treecover_hd <- mask(treecover_jambi_crop, mask = hd)
treecover_hd_crop <- mask(treecover_hd, mask = hd)
plot(treecover_hd_crop)

gain_hd <- mask(gain_jambi_crop, mask = hd)
gain_hd_crop <- mask(gain_hd, mask = hd)
plot(gain_hd_crop)

# Create Raster stack for production area
crs(lossyear_hd_crop) <- mercator
crs(treecover_hd_crop) <- mercator
crs(gain_hd_crop) <- mercator 

forestcover_hd = stack(lossyear_hd_crop, treecover_hd_crop, gain_hd_crop)
forestcover_hd_sf <- as.data.frame(forestcover_hd, xy = TRUE)

sum(is.na(forestcover_hd_sf))
forestcover_hd_clean <- na.omit(forestcover_hd_sf)
sum(is.na(forestcover_hd_clean))

# Add column(s) to code Forest Area type and Land title type 
forestcover_hd_clean$Province <- c("Jambi")
forestcover_hd_clean$Forest_Area <- c("Protection")
forestcover_hd_clean$Concession <- c("NA")
forestcover_hd_clean$Land_Title <- c("HD")

# Save as separate dataset
head(forestcover_hd_clean)
saveRDS(forestcover_hd_clean, file = "jambi_hd.Rda")

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
