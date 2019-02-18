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

#protection <- st_transform(prot, crs = 41001)
#hd <- st_transform(hud, crs = 41001)

options(sf_max.plot=1)
plot(st_geometry(jambi))
plot(st_geometry(hd), add = TRUE)
plot(st_geometry(protection), add = TRUE)

land_titles <- st_union(protection, hd)
land_titles$id <- colnames("protection")
land_titles$id.1 <- colnames("hd")
head(land_titles)

options(sf_max.plot=1)
plot(st_geometry(jambi))
plot(st_geometry(land_titles), add = TRUE)

land_titles_box <- st_make_grid(land_titles, n = 1)

options(sf_max.plot=1)
plot(st_geometry(jambi))
plot(st_geometry(land_titles), add = TRUE)
plot(land_titles_box, add = TRUE)

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
tamonarang_sf_1 <- tamonarang_sf[,-(6), drop = FALSE]
head(tamonarang_sf_1)
tamonarang_sf_1$forest_area_ENG <- c("Protection")
tamonarang_sf_1$forest_area <- c("Hutan Lindung")
head(tamonarang_sf_1)

# Identifying the border between polygons 
prot_sp <- as(prot, Class = "Spatial")
hud_sp <- as(hud, Class = "Spatial")

land_titles <- st_union(prot, hud)
land_titles_sp <- as(land_titles, Class = "Spatial")

border = gDifference(as(land_titles_sp, "SpatialLines"), as(gUnaryUnion(prot_sp, id = NULL), "SpatialLines"), byid = TRUE)
border1 = gNearestPoints(as(prot_sp, "SpatialLines"), as(hud_sp, "SpatialLines"))
head(border1)
# Only returned 1 point - polygons may not actually be adjacent to each other? 

# Checking the plots
plot(st_geometry(tamonarang_sf), col = "forest green", axes = TRUE) 
plot(st_geometry(prot), border = "black", add = TRUE)
plot(st_geometry(hud), border = "black", add = TRUE)
plot(border1, col = "red", lwd = 2, add = TRUE)
