# Dataset - Creating the main variables
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

# Load the dataframe 
# (1) Test study area: Tamonarang-1 
tamonarang <- readRDS("tamonarang.Rda")
head(tamonarang)

# Load the study area polygons
prot <- st_read("Polygons/Jambi_1_GR_Tamonarang_Protection.shp")
prod <- st_read("Polygons/Jambi_1_GR_Tamonarang_Production.shp")
hud <- st_read("Polygons/Jambi_1_GR_Tamonarang_HD.shp")

# Convert dataframe to sf object to coerce x,y columns into geometry column 
# Changing projection to CRS = 4326 will allow us to plot the land title polygons on top of sf object
tamonarang_sf <- st_as_sf(tamonarang, coords = c("x", "y"), crs = 4326)
head(tamonarang_sf)

## CREATE DUMMY VARIABLE: SFP (SOCIAL)

# Identify if point lies in HD or not HD 
# Plot polygons together to visually inspect data
plot(st_geometry(tamonarang_sf), col = "forest green", axes = TRUE) 
plot(st_geometry(prot), border = "black", add = TRUE)
plot(st_geometry(hud), border = "black", add = TRUE)

# Check if GFC data intersects with study area polygons 
# Create new column `in_HD`
tamonarang_sf$in_HD <- st_intersects(tamonarang_sf, hud, sparse = FALSE)
head(tamonarang_sf)

## CREATE RUNNING VARIABLE: (DISTANCE)

# Create the border of interest
# Identifying the border between polygons 
prot_sp <- as(prot, Class = "Spatial")
hud_sp <- as(hud, Class = "Spatial")

border = st_intersection(prot, hud)
border_sp <- as(border, Class = "Spatial")

# Convert border from polygon to spatial line
border_line <- as(border_sp, Class = "SpatialLines")

# Calculate length of border
SpatialLinesLengths(border_line, longlat = TRUE)
# 5.081692 (Units are in Great Circle Distances in km) 
# The border is 5 km long

# Checking the plots
plot(st_geometry(tamonarang_sf), col = "forest green", axes = TRUE) 
plot(st_geometry(prot), border = "black", add = TRUE)
plot(st_geometry(hud), border = "black", add = TRUE)
plot(border_line, col = "red", add = TRUE)

# Calculating distance from border 
# Testing with 1 point 
#test_matrix <- tamonarang[75000:76000, -(3:5)]
#test <- dist2Line(test_matrix, border_line)
#summary(test)
#test

# Calculate distance from border 
# Convert tamonarang_sf to dataframe 
tamonarang.df <- as.data.frame(tamonarang_sf, xy = TRUE)

# Create matrix with latitude and longitude stored in x and y columns 
covermatrix <- tamonarang[,-(3:8)]

# Calculate distance and relative distance, create new columns to store the values
dist.tam <- dist2Line(covermatrix, border_line)
head(dist.tam)
tamonarang.df$distrelative <- ifelse(tamonarang.df$in_HD==TRUE, dist.tam[,1], -dist.tam[,1])
summary(tamonarang.df$distrelative)

# Renamed columns 
#names(tamonarang.df)[5] <- "forest_area_ENG"
#names(tamonarang.df)[6] <- "forest_area"
head(tamonarang.df)

## CREATE OUTCOME VARIABLE: RATE OF FOREST COVER LOSS

# Deforestation defined as average annual rate of forest cover loss
# We first assume that forest cover is recorded as forest cover in this dataset.
# Later, in robustness checks, we will overlay this with known plantation shapefiles to verify 
# if plantations have been recorded as forest cover. 
# As mentioned in Puyravaud (2003), we can use average rate of change formula
# For the difference-in-difference estimation, our point of comparison is 2008
# According to Afiff (2016), laws relating to the SFP scheme were: 
# (2007) Government Regulation 6/2007 on Forest Governance, Planning & Utilisation; and
# (2008) Minister of Forest Regulation P.49/Menhut-II/2008 on Village Forest, which laid out the mechanism for obtaining an SFP permit 


# Create variables for forest cover pre-2008 and post-2008
#tamonarang.df$pre <- ifelse(tamonarang.df$lossyear <= 8 & tamonarang.df$lossyear > 0, 0, tamonarang.df$treecover2000)
#tamonarang.df$post <- ifelse(tamonrang.df$lossyear > 8, 0, tamonrang.df$treecover2000)

tamonarang.df$cover_pre08 <- ifelse(tamonarang.df$lossyear <= 8 & tamonarang.df$lossyear > 0, 0, tamonarang.df$treecover2000)
tamonarang.df$cover_post08 <- ifelse(tamonarang.df$lossyear > 0, 0, tamonarang.df$treecover2000)

tamonarang.df$treecover2008 = lossdf$yr_8

# Rate of forest cover loss (f1, f2): -(f2 - f1)/t
# (f2-f1)/t only calculates rate of change. If rate of change is negative, then it denotes loss. 
# As this column is concerned with loss, we convert negative values into positive values for ease of interpretation.
tamonarang.df$prerate <- -(tamonarang.df$cover_pre08 - tamonarang.df$treecover2000)/8
tamonarang.df$postrate <- -(tamonarang.df$cover_post08 - tamonarang.df$treecover2008)/8

# Save dataframe to drive
saveRDS(tamonarang.df, file = "testDataset.Rda")

# Calculate forest cover in each year (based on lossyear only) for regression plots
# and for robustness checks
tam.test <- tamonarang.df

lossdf <- data.frame(id = 1:91331, cov_00 = tam.test$treecover2000, lossyear = tam.test$lossyear)

lossdf <- lossdf %>% 
  mutate(year=0) %>% 
  expand(nesting(id, cov_00, lossyear), year = 0:17) %>% 
  mutate(loss_event = if_else(year >= lossyear | year==0, 0, cov_00)) %>% 
  mutate(year = paste0("yr_", year)) %>% 
  spread(year, loss_event)

lossdf <- select(lossdf, -matches("yr_0"))
head(lossdf)

# Save lossdf as dataframe to drive
saveRDS(lossdf, file = "lossdf.Rda")




