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

#IDN <- st_read("gadm36_IDN_shp/gadm36_IDN_1.shp")
#idn <- st_transform(IDN, crs = 41001)
#head(idn)
#jambi <- filter(idn, NAME_1 == "Jambi")
#head(jambi)

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

# Save dataframe to drive
saveRDS(forestcover_tamonarang, file = "tamonarang.Rda")
tamonarang <- readRDS("tamonarang.Rda")
head(tamonarang)

# Convert dataframe to sf object
# Changing projection to CRS = 4326 will allow us to plot the land title polygons on top of sf object
tamonarang_sf <- st_as_sf(tamonarang, coords = c("x", "y"), crs = 4326)
head(tamonarang_sf)

plot(tamonarang_sf["treecover2000"], axes = TRUE)
plot(tamonarang_sf["lossyear"], axes = TRUE)
plot(tamonarang_sf["gain"], axes = TRUE)

# Add column for Province
tamonarang_sf$province <- c("Jambi")
head(tamonarang_sf)

plot(st_geometry(tamonarang_sf), col = "forest green", axes = TRUE) 
plot(st_geometry(prot), border = "black", add = TRUE)
plot(st_geometry(hud), border = "black", add = TRUE)

tamonarang_sf$in_HD <- st_intersects(tamonarang_sf, hud, sparse = FALSE)
head(tamonarang_sf)
tamonarang_sf$forest_area_ENG <- c("Protection")
tamonarang_sf$forest_area <- c("Hutan Lindung")
head(tamonarang_sf)

# Identifying the border between polygons 
prot_sp <- as(prot, Class = "Spatial")
hud_sp <- as(hud, Class = "Spatial")

border = st_intersection(prot, hud)
border_sp <- as(border, Class = "Spatial")

# Convert border from polygon to spatial line
border_line <- as(border_sp, Class = "SpatialLines")

# Calculate length of border
SpatialLinesLengths(border_line)
# The border is 5km long

# Checking the plots
plot(st_geometry(tamonarang_sf), col = "forest green", axes = TRUE) 
plot(st_geometry(prot), border = "black", add = TRUE)
plot(st_geometry(hud), border = "black", add = TRUE)
plot(border_line, col = "red", add = TRUE)

# Calculating distance from border 
# Testing with 1 point 
test_matrix <- tamonarang[75000:76000, -(3:5)]
test <- dist2Line(test_matrix, border_line)
summary(test)
test

treecovertest <- tamonarang[,-(4:5)]
treecovertest1 <- dist2Line(treecovertest[,-3], border_line)
head(treecovertest1)
distance <- treecovertest1[,1]
plot(distance)

y <- treecovertest[, 3]
x <- distance
summary(x)
rdplot(y, x, c = 0.5)
rdrobust(y, x, c = 0.5)
# There could be an error with calculating distance - distances are all positive 
# Possible solutions: split the dataset into left and right, and calculate distances separately? 

