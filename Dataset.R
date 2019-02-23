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

#treecovertest <- tamonarang[,-(4:5)]
#treecovertest1 <- dist2Line(treecovertest[,-3], border_line)
#head(treecovertest1)
#distance <- treecovertest1[,1]
#plot(distance)

#y <- treecovertest[, 3]
#x <- distance
#summary(x)
#rdplot(y, x, c = 0.5)
#rdrobust(y, x, c = 0.5)
# There could be an error with calculating distance - distances are all positive 
# Possible solutions: split the dataset into left and right, and calculate distances separately? 

# Convert tamonarang_sf to dataframe 
tamonarang.df <- as.data.frame(tamonarang_sf, xy = TRUE)
tamonarang.df$distance <- treecovertest1[,1]
tamonarang.df$distrelative <- ifelse(tamonarang.df$in_HD==TRUE, tamonarang.df$distance, -tamonarang.df$distance)
summary(tamonarang.df$distrelative)

y <- tamonarang.df$treecover2000
x <- tamonarang.df$distrelative
rdplot(y, x, c = 0, title = "Baseline RD Plot", x.label = "Distance to the border", y.label = "Forest cover in 2000")
robust <- rdrobust(y, x, c = 0, all)

# Experimenting with different bin sizes 
# Evenly-spaced bins
plot_ebin <- rdplot(y, x, c = 0, binselect = "es")
summary(plot_ebin)

# Quantile-spaced bins
plot_qbin <- rdplot(y, x, c = 0, binselect = "qs")
summary(plot_qbin)

plot_bin <- rdplot(y, x, c = 0, binselect = "qsmv")
summary(plot_bin)

# Create deforestation variable 
# Deforestation defined as average rate of forest cover loss (2000-2017)
# As mentioned in Puyravaud (2003), we can use average rate of change formula
tamonarang_sf$treecover2017 <- ifelse(tamonarang_sf$lossyear > 0, 0, tamonarang_sf$treecover2000)
tamonarang_sf$rate <- -(tamonarang_sf$treecover2017-tamonarang_sf$treecover2000)/17

library(ggthemes)
library(gridExtra)
# Density plots for treecover in 2000 vs. 2017 (in %)
ggplot(tamonarang_sf, aes(treecover2000)) + geom_density(col = "forest green") + theme_tufte()
ggplot(tamonarang_sf, aes(treecover2017)) + geom_density(col = "red") + theme_tufte()
t2 <- ggplot(tamonarang_sf, aes(in_HD, treecover2017)) + geom_violin(scale = "area", color = "white", aes(fill = in_HD)) + theme_tufte()

t1 <- ggplot(tamonarang_sf, aes(in_HD, treecover2000)) + geom_violin(scale = "area", color = "white", aes(fill = in_HD)) + theme_tufte()

t2 <- t2 + coord_flip()
t1 <- t1 + coord_flip() 
grid.arrange(t1, t2, nrow = 2)

# Rate of forest cover loss 
t3 <- ggplot(tamonarang_sf, aes(rate)) + geom_dotplot(color = "white", alpha = 0.5) + theme_tufte()
t3 + facet_grid(.~in_HD)

t4 <- ggplot(tamonarang_sf, aes(rate)) + geom_histogram(alpha = 0.5) + theme_tufte()
t4 + facet_grid(.~in_HD)
