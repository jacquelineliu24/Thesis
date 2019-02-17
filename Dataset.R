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

protection <- st_transform(prot, crs = 41001)
hd <- st_transform(hud, crs = 41001)

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
crs(treecover) <- mercator
crs(lossyear) <- mercator
crs(gain) <- mercator

land_titles_sp <- as(land_titles, "Spatial")
head(land_titles_sp)

lossyear_tam <- mask(lossyear, mask = land_titles_sp)

lossyear_prot <- mask(lossyear, mask = protection)
lossyear_prot_crop <- crop(lossyear_prot, protection)

lossyear_hd <- mask(lossyear, mask = hd)
lossyear_hd_crop <- crop(lossyear_hd, hd)

lossyear_tam <- merge(lossyear_prot_crop, lossyear_hd_crop)

plot(lossyear_tam)
lossyear_tam_crop <- crop(lossyear_tam, land_titles)

