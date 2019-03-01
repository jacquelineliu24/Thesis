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
library(rgeos)
library(geosphere)

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

# (1) Test study area: Tamonarang-1 
# Comments: Regression plots generally work but the optimal bandwidth calculated is very small
# Bandwidth was only 545 metres on each side (total approx. 1km) while in the literature, 
# 10 - 40 km seems quite common. The border length might be too short. To experiment with longer 
# borders and larger samples. 

# Load study area polygons
prot <- st_read("Polygons/Jambi_1_GR_Tamonarang_Protection.shp")
prod <- st_read("Polygons/Jambi_1_GR_Tamonarang_Production.shp")
hud <- st_read("Polygons/Jambi_1_GR_Tamonarang_HD.shp")

# Create buffer around study area polygons 
land_titles_merge <- merge(prot, hud)
land_titles_buf <- st_buffer(land_titles_merge, dist = )

# Crop raster layers to land_titles polygon
lossyear_prot <- mask(lossyear, mask = prot)
lossyear_prot_crop <- crop(lossyear_prot, prot)
plot(lossyear_prot_crop)

lossyear_hd <- mask(lossyear, mask = hud)
lossyear_hd_crop <- crop(lossyear_hd, hud)

lossyear_tam <- merge(lossyear_prot_crop, lossyear_hd_crop)
plot(lossyear_tam)

treecover_prot <- mask(treecover, mask = prot)
treecover_prot_crop <- crop(treecover_prot, prot)

treecover_hd <- mask(treecover, mask = hud)
treecover_hd_crop <- crop(treecover_hd, hud)

treecover_tam <- merge(treecover_prot_crop, treecover_hd_crop)
plot(treecover_tam)

gain_prot <- mask(gain, mask = prot)
gain_prot_crop <- crop(gain_prot, prot)

gain_hd <- mask(gain, mask = hud)
gain_hd_crop <- crop(gain_hd, hud)

gain_tam <- merge(gain_prot_crop, gain_hd_crop)
plot(gain_tam)

# Create raster stack
crs(lossyear_tam) <- mercator
crs(treecover_tam) <- mercator
crs(gain_tam) <- mercator

forestcover_tam = stack(treecover_tam, lossyear_tam, gain_tam)
head(forestcover_tam)

# Coerce raster to dataframe 
forestcover_tam_df <- as.data.frame(forestcover_tam, xy = TRUE)
sum(is.na(forestcover_tam_df))
forestcover_tam_clean <- na.omit(forestcover_tam_df)
sum(is.na(forestcover_tam_clean))
head(forestcover_tam_clean)
forestcover_tamonarang <- rename(forestcover_tam_clean, c("layer.1" = "treecover2000", "layer.2" = "lossyear", "layer.3" = "gain"))
head(forestcover_tamonarang)

# Create new column for Province 
forestcover_tamonarang$province <- c("Jambi")
head(forestcover_tamonarang)

# Create new column for Forest Area Type
forestcover_tamonarang$forest_area <- c("Hutan Lindung")
forestcover_tamonarang$forest_area_ENG <- c("Protection")
head(forestcover_tamonarang)

# Save dataframe to drive
saveRDS(forestcover_tamonarang, file = "tamonarang.Rda")
