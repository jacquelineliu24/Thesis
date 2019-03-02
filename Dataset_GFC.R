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

# (T) Test study area: Tamonarang-1 
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

############################################################################
# (1) Study Area 1: Jambi - Near HT-25

# Load study area polygons
prot <- st_read("Polygons/Study Area 1/Jambi_1_nearHT-25_HL.shp")
hud <- st_read("Polygons/Study Area 1/Jambi_1_nearHT-25_HD.shp")

# Crop raster layers to land_titles polygon
lossyear_prot <- mask(lossyear, mask = prot)
lossyear_prot_crop <- crop(lossyear_prot, prot)
plot(lossyear_prot_crop)

lossyear_hd <- mask(lossyear, mask = hud)
lossyear_hd_crop <- crop(lossyear_hd, hud)

lossyear_1 <- merge(lossyear_prot_crop, lossyear_hd_crop)
plot(lossyear_1)

treecover_prot <- mask(treecover, mask = prot)
treecover_prot_crop <- crop(treecover_prot, prot)

treecover_hd <- mask(treecover, mask = hud)
treecover_hd_crop <- crop(treecover_hd, hud)

treecover_1 <- merge(treecover_prot_crop, treecover_hd_crop)
plot(treecover_1)

# Create raster stack
crs(lossyear_1) <- mercator
crs(treecover_1) <- mercator

forestcover_stack = stack(treecover_1, lossyear_1)
head(forestcover_stack)

# Coerce raster to dataframe 
forestcover_1_df <- as.data.frame(forestcover_stack, xy = TRUE)
sum(is.na(forestcover_1_df))
forestcover_1_clean <- na.omit(forestcover_1_df)
sum(is.na(forestcover_1_clean))
head(forestcover_1_clean)
names(forestcover_1_clean)[3] <- "treecover2000"
names(forestcover_1_clean)[4] <- "lossyear"
forestcover_1 <- forestcover_1_clean
head(forestcover_1)

# Create new column for Province 
forestcover_1$province <- c("Jambi")
head(forestcover_1)

# Create new column for Forest Area Type
forestcover_1$forest_area <- c("Hutan Lindung")
forestcover_1$forest_area_ENG <- c("Protection")
head(forestcover_1)

# Save dataframe to drive
saveRDS(forestcover_1, file = "StudyArea1.Rda")

############################################################################
# (2) Study Area 2: Jambi - Lubuk Beringin

# Load study area polygons
prot <- st_read("Polygons/Study Area 2/Jambi_1_Lubuk_Beringin_HL.shp")
hud <- st_read("Polygons/Study Area 2/Jambi_1_Lubuk_Beringin_HD.shp")

# Crop raster layers to land_titles polygon
lossyear_prot <- mask(lossyear, mask = prot)
lossyear_prot_crop <- crop(lossyear_prot, prot)
plot(lossyear_prot_crop)

lossyear_hd <- mask(lossyear, mask = hud)
lossyear_hd_crop <- crop(lossyear_hd, hud)

lossyear_2 <- merge(lossyear_prot_crop, lossyear_hd_crop)
plot(lossyear_2)

treecover_prot <- mask(treecover, mask = prot)
treecover_prot_crop <- crop(treecover_prot, prot)

treecover_hd <- mask(treecover, mask = hud)
treecover_hd_crop <- crop(treecover_hd, hud)

treecover_2 <- merge(treecover_prot_crop, treecover_hd_crop)
plot(treecover_2)

# Create raster stack
crs(lossyear_2) <- mercator
crs(treecover_2) <- mercator

forestcover_stack2 = stack(treecover_2, lossyear_2)
head(forestcover_stack2)

# Coerce raster to dataframe 
forestcover_2_df <- as.data.frame(forestcover_stack2, xy = TRUE)
sum(is.na(forestcover_2_df))
forestcover_2_clean <- na.omit(forestcover_2_df)
sum(is.na(forestcover_2_clean))
head(forestcover_2_clean)
names(forestcover_2_clean)[3] <- "treecover2000"
names(forestcover_2_clean)[4] <- "lossyear"
forestcover_2 <- forestcover_2_clean
head(forestcover_2)

# Create new column for Province 
forestcover_2$province <- c("Jambi")
head(forestcover_2)

# Create new column for Forest Area Type
forestcover_2$forest_area <- c("Hutan Lindung")
forestcover_2$forest_area_ENG <- c("Protection")
head(forestcover_2)

# Save dataframe to drive
saveRDS(forestcover_2, file = "StudyArea2.Rda")

############################################################################
# (3) Study Area 3: Jambi - Tamonarang

# Load study area polygons
prot <- st_read("Polygons/Study Area 3/Jambi_1_Tamonarang_HL.shp")
hud <- st_read("Polygons/Study Area 3/Jambi_1_Tamonarang_HD.shp")

# Crop raster layers to land_titles polygon
lossyear_prot <- mask(lossyear, mask = prot)
lossyear_prot_crop <- crop(lossyear_prot, prot)
plot(lossyear_prot_crop)

lossyear_hd <- mask(lossyear, mask = hud)
lossyear_hd_crop <- crop(lossyear_hd, hud)

lossyear_3 <- merge(lossyear_prot_crop, lossyear_hd_crop)
plot(lossyear_3)

treecover_prot <- mask(treecover, mask = prot)
treecover_prot_crop <- crop(treecover_prot, prot)

treecover_hd <- mask(treecover, mask = hud)
treecover_hd_crop <- crop(treecover_hd, hud)

treecover_3 <- merge(treecover_prot_crop, treecover_hd_crop)
plot(treecover_3)

# Create raster stack
crs(lossyear_3) <- mercator
crs(treecover_3) <- mercator

forestcover_stack3 = stack(treecover_3, lossyear_3)
head(forestcover_stack3)

# Coerce raster to dataframe 
forestcover_3_df <- as.data.frame(forestcover_stack3, xy = TRUE)
sum(is.na(forestcover_3_df))
forestcover_3_clean <- na.omit(forestcover_3_df)
sum(is.na(forestcover_3_clean))
head(forestcover_3_clean)
names(forestcover_3_clean)[3] <- "treecover2000"
names(forestcover_3_clean)[4] <- "lossyear"
forestcover_3 <- forestcover_3_clean
head(forestcover_3)

# Create new column for Province 
forestcover_3$province <- c("Jambi")
head(forestcover_3)

# Create new column for Forest Area Type
forestcover_3$forest_area <- c("Hutan Lindung")
forestcover_3$forest_area_ENG <- c("Protection")
head(forestcover_3)

# Save dataframe to drive
saveRDS(forestcover_3, file = "StudyArea3.Rda")

