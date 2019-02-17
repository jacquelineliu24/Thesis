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
class(forestcover_jambi)

# Coerce raster to dataframe
forestcover_jambi_sf <- as.data.frame(forestcover_jambi, xy = TRUE)
class(forestcover_jambi_sf)
head(forestcover_jambi_sf)
# The dataframe contains many NA values due to whitespace

# Removing NA values from Jambi forestcover dataframe 
sum(is.na(forestcover_jambi_sf))
forestcover_jambi_clean <- na.omit(forestcover_jambi_sf)
sum(is.na(forestcover_jambi_clean))
# NA values removed

head(forestcover_jambi_clean)

# Save dataframe
saveRDS(forestcover_jambi_clean, file = "jambi.Rda")
jambi.data <- readRDS("jambi.Rda")

# Convert dataframe to sf object
jambi_sf <- st_as_sf(jambi.data, coords = c("x", "y"), crs = 41001)
head(jambi_sf)
plot(st_geometry(jambi_sf))

# Add column for Province
jambi_sf$Province <- c("Jambi")

# Save sf object
st_write(jambi_sf, "jambi.shp")

# Limiting raster to study areas in Jambi
protection <- st_read("Polygons/Jambi_1_GR_Tamonarang_Protection.shp")
production <- st_read("Polygons/Jambi_1_GR_Tamonarang_Production.shp")
hd <- st_read("Polygons/Jambi_1_GR_Tamonarang_HD.shp")

protection <- st_transform(protection, crs = mercator)
hd <- st_transform(hd, crs = mercator)

plot(st_geometry(protection), border = "black")
plot(st_geometry(hd), border = "black", add = TRUE)
plot(st_geometry(jambi_sf), pch = 16, col = "forest green", add = TRUE)

hd_box <- st_make_grid(hd, n = 1)
protection_box <- st_make_grid(protection, n = 1)
land_title_box <- st_union(hd_box, protection_box)


jambi_intersects <- st_intersects(land_title_box, jambi_sf)



# Create buffer around border
# Creating a 0.1 arc-degree (~ 50 km) buffer around relevant areas
#jambi_buf <- st_buffer(jambi, dist = 0.1)
#jambi_buf_sp <- as(jambi_buf, "Spatial")

protection_buf <- st_buffer(protection, dist = 0.1)
protection_buf_sp <- as(protection_buf, "Spatial")

hd_buf <- st_buffer(hd, dist = 0.1)
hd_buf_sp <- as(hd_buf, "Spatial")

buffer <- st_union(protection_buf_sp, hd_buf_sp)

lossyear_jambi <- mask(lossyear, mask = buffer)
lossyear_jambi_crop <- crop(lossyear_jambi, buffer) 

treecover_jambi <- mask(treecover, mask = buffer)
treecover_jambi_crop <- crop(treecover_jambi, buffer)

gain_jambi <- mask(gain, mask = buffer)
gain_jambi_crop <- crop(gain_jambi, buffer)

# Create Raster stack for buffer area
crs(lossyear_jambi_crop) <- mercator
crs(treecover_jambi_crop) <- mercator
crs(gain_jambi_crop) <- mercator 

forestcover_buf = stack(lossyear_jambi_crop, treecover_jambi_crop, gain_jambi_crop)
forestcover_buf_df <- as.data.frame(forestcover_buf, xy = TRUE)

sum(is.na(forestcover_buf_df))
forestcover_buf_df_clean <- na.omit(forestcover_buf_df)
sum(is.na(forestcover_buf_df_clean)) 

# Save dataframe
saveRDS(forestcover_buf_df_clean, file = "jambi_Tamonarang.Rda")
jambi_tam.data <- readRDS("jambi_Tamonarang.Rda")

# Convert dataframe to sf object
jambi_tam_sf <- st_as_sf(jambi_tam.data, coords = c("x", "y"), crs = 41001)
head(jambi_tam_sf)

# Limit sf object to polygons 
plot(st_geometry(protection), border = "black")
plot(st_geometry(hd), border = "black", add = TRUE)
plot(st_geometry(jambi_tam_sf), pch = 16, col = "forest green", add = TRUE)
st_bbox(jambi_tam_sf)

st_intersects()




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
