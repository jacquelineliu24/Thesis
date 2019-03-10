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
library(lwgeom)

# (1) Test study area: Tamonarang-1 
# Load the dataframe
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

lossdf <- data.frame(id = 1:91331, yr_00 = tam.test$treecover2000, lossyear = tam.test$lossyear)

lossdf <- lossdf %>% 
  mutate(year=0) %>% 
  expand(nesting(id, lossyear, yr_00), year = 0:17) %>% 
  mutate(loss_event = if_else(year >= lossyear & lossyear !=0 , 0, yr_00)) %>% 
  mutate(year = paste0("yr_", year)) %>% 
  spread(year, loss_event)

head(lossdf)

lossdf = subset(lossdf, select = -c(yr_0))
head(lossdf)

# Save lossdf as dataframe to drive
saveRDS(lossdf, file = "lossdf.Rda")
lossdf <- readRDS("lossdf.Rda")

# Transform dataframes into wide and long (panel) formats for regression 
# Wide format: 
# Create tibble extracting geometry and in_HD columns from tamonarang.df dataframe
geo_id = tibble(geometry = tamonarang.df[,7], in_HD = tamonarang.df[,8])
head(geo_id)

# Join geo_id and lossdf tibble 
tam.wide = bind_cols(geo_id, lossdf)
dim(tam.wide)
head(tam.wide)

# Panel format: 
tam.panel = gather(tam.wide, key = year, value = treecover, -c(geometry, in_HD, id, lossyear))
head(tam.panel)
View(tam.panel)

# Checking for 1 point 
filter(tam.panel, id ==1)

# Save wide and panel formats: 
saveRDS(tam.wide, file = "tamwide.Rda")
saveRDS(tam.panel, file = "tampanel.Rda")

################################################################################
# (1) Study Area 1: Jambi - Near HT-25 
# Load the dataframe
SA1 <- readRDS("RFiles/StudyAreas/StudyArea1.Rda")
head(SA1)

# Load the study area polygons
prot <- st_read("Polygons/Study Area 1/J1_nearHT-25_HL.shp")
hud <- st_read("Polygons/Study Area 1/J1_nearHT-25_HD.shp")

# Convert dataframe to sf object to coerce x,y columns into geometry column 
# Changing projection to CRS = 4326 will allow us to plot the land title polygons on top of sf object
SA1_sf <- st_as_sf(SA1, coords = c("x", "y"), crs = 4326)
head(SA1_sf)

## CREATE DUMMY VARIABLE: SFP (SOCIAL)

# Identify if point lies in HD or not HD 
# Plot polygons together to visually inspect data
plot(st_geometry(SA1_sf), col = "forest green", axes = TRUE) 
plot(st_geometry(prot), border = "black", add = TRUE)
plot(st_geometry(hud), border = "black", add = TRUE)

# Check if GFC data intersects with study area polygons 
# Create new column `in_HD`
SA1_sf$in_HD <- st_intersects(SA1_sf, hud, sparse = FALSE)
head(SA1_sf)

## CREATE RUNNING VARIABLE: (DISTANCE)

# Create the border of interest
# Identifying the border between polygons 
prot_sp <- as(prot, Class = "Spatial")
hud_sp <- as(hud, Class = "Spatial")

# In case of self-intersection
#prot <- st_make_valid(prot)
#hud <- st_make_valid(hud)

border = st_intersection(prot, hud)
border_sp <- as(border, Class = "Spatial")

# Convert border from polygon to spatial line
border_line <- as(border_sp, Class = "SpatialLines")

# Calculate length of border
SpatialLinesLengths(border_line, longlat = TRUE)
# 16.06598
# Border is 16 km long.  

# Checking the plots
plot(st_geometry(SA1_sf), col = "forest green", axes = TRUE) 
plot(st_geometry(prot), border = "black", add = TRUE)
plot(st_geometry(hud), border = "black", add = TRUE)
plot(border_line, col = "red", add = TRUE)

# Calculate distance from border 
# Convert SA1_sf to dataframe 
SA1.df <- as.data.frame(SA1_sf, xy = TRUE)
head(SA1.df)

# Create matrix with latitude and longitude stored in x and y columns 
head(SA1)
covermatrix <- SA1[,(1:2)]

# Calculate distance and relative distance, create new columns to store the values
dist.SA1 <- dist2Line(covermatrix, border_line)
head(dist.SA1)
SA1.df$distrelative <- ifelse(SA1.df$in_HD==TRUE, dist.SA1[,1], -dist.SA1[,1])
summary(SA1.df$distrelative)
head(SA1.df)

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

# Calculate forest cover in each year (based on lossyear only) for regression plots
# and for robustness checks
SA1.test <- SA1.df

lossdf <- data.frame(id = 1:dim(SA1.df), yr_00 = SA1.test$treecover2000, lossyear = SA1.test$lossyear)

lossdf <- lossdf %>% 
  mutate(year=0) %>% 
  expand(nesting(id, lossyear, yr_00), year = 0:17) %>% 
  mutate(loss_event = if_else(year >= lossyear & lossyear !=0 , 0, yr_00)) %>% 
  mutate(year = paste0("yr_", year)) %>% 
  spread(year, loss_event)

head(lossdf)

lossdf = subset(lossdf, select = -c(yr_0))
head(lossdf)

# Save as dataframe to drive
saveRDS(SA1.df, file = "RFiles/StudyAreas/SA1-dataset.Rda")
saveRDS(lossdf, file = "RFiles/StudyAreas/lossdf-SA1.Rda")
lossdf1 <- readRDS("RFiles/StudyAreas/lossdf-SA1.Rda")

# Transform dataframes into wide and long (panel) formats for regression 
# Wide format: 
# Create tibble extracting geometry and in_HD columns from tamonarang.df dataframe
geo_id = tibble(geometry = SA1.df[,7], in_HD = SA1.df[,8])
head(geo_id)

# Join geo_id and lossdf tibble 
SA1.wide = bind_cols(geo_id, lossdf1)
dim(SA1.wide)
head(SA1.wide)

# Panel format: 
SA1.panel = gather(SA1.wide, key = year, value = treecover, -c(geometry, in_HD, id, lossyear))
head(SA1.panel)

# Checking for 1 point 
filter(SA1.panel, id ==1)

# Save wide and panel formats: 
saveRDS(SA1.wide, file = "RFiles/StudyAreas/SA1wide.Rda")
saveRDS(SA1.panel, file = "RFiles/StudyAreas/SA1panel.Rda")

# Combine dataframes and reorder columns
full1 = bind_cols(SA1.df, SA1.wide)
full1 <- full1 %>% select(-(10:12))
full1 <- full1 %>% select(-(1:2)) 
full1 <- full1 %>% select("geometry", "in_HD", "distrelative", "lossyear1", 
                          "yr_00", "yr_1", "yr_2", "yr_3", "yr_4", "yr_5", "yr_6", "yr_7", "yr_8",
                          "yr_9", "yr_10", "yr_11", "yr_12", "yr_13", "yr_14", "yr_15", "yr_16", "yr_17",
                          "province", "forest_area", "forest_area_ENG", "study_area")

# Panel format: 
full1.panel = gather(full1, key = year, value = treecover, -c(geometry, in_HD, distrelative, 
                                                              lossyear1, province, forest_area, 
                                                              forest_area_ENG, study_area))
head(full1.panel)

saveRDS(full1, file = "RFiles/StudyAreas/SA1full.Rda")
saveRDS(full1.panel, file = "RFiles/StudyAreas/SA1fullpanel.Rda")

# Status: Done as of 9 Mar 2019

################################################################################
# (2) Study Area 2: Jambi - Lubuk Beringin 
# Load the dataframe
SA2 <- readRDS("RFiles/StudyAreas/StudyArea2.Rda")
head(SA2)

# Load the study area polygons
prot <- st_read("Polygons/Study Area 2/J1_Lubuk_Beringin_HL.shp")
hud <- st_read("Polygons/Study Area 2/J1_Lubuk_Beringin_HD.shp")

# Convert dataframe to sf object to coerce x,y columns into geometry column 
# Changing projection to CRS = 4326 will allow us to plot the land title polygons on top of sf object
SA2_sf <- st_as_sf(SA2, coords = c("x", "y"), crs = 4326)
head(SA2_sf)

## CREATE DUMMY VARIABLE: SFP (SOCIAL)

# Identify if point lies in HD or not HD 
# Plot polygons together to visually inspect data
plot(st_geometry(SA2_sf), col = "forest green", axes = TRUE) 
plot(st_geometry(prot), border = "black", add = TRUE)
plot(st_geometry(hud), border = "black", add = TRUE)

# Check if GFC data intersects with study area polygons 
# Create new column `in_HD`
SA2_sf$in_HD <- st_intersects(SA2_sf, hud, sparse = FALSE)
head(SA2_sf)

## CREATE RUNNING VARIABLE: (DISTANCE)

# Create the border of interest
# Identifying the border between polygons 
prot_sp <- as(prot, Class = "Spatial")
hud_sp <- as(hud, Class = "Spatial")

# In case of issues with self-intersection: 
#prot <- st_make_valid(prot)
#hud <- st_make_valid(hud)

border = st_intersection(prot, hud)
border_sp <- as(border, Class = "Spatial")

# Convert border from polygon to spatial line
border_line <- as(border_sp, Class = "SpatialLines")

# Calculate length of border
SpatialLinesLengths(border_line, longlat = TRUE)
# 27.72401
# The border is 28 km long

# Checking the plots
plot(st_geometry(SA2_sf), col = "forest green", axes = TRUE) 
plot(st_geometry(prot), border = "black", add = TRUE)
plot(st_geometry(hud), border = "black", add = TRUE)
plot(border_line, col = "red", add = TRUE)

# Calculate distance from border 
# Convert tamonarang_sf to dataframe 
SA2.df <- as.data.frame(SA2_sf, xy = TRUE)
head(SA2.df)
head(SA2)

# Create matrix with latitude and longitude stored in x and y columns 
covermatrix <- SA2[,(1:2)]
head(covermatrix)

# Calculate distance and relative distance, create new columns to store the values
dist.SA2 <- dist2Line(covermatrix, border_line)
head(dist.SA2)
SA2.df$distrelative <- ifelse(SA2.df$in_HD==TRUE, dist.SA2[,1], -dist.SA2[,1])
summary(SA2.df$distrelative)
head(SA2.df)

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

# Calculate forest cover in each year (based on lossyear only) for regression plots
# and for robustness checks
SA2.test <- SA2.df

lossdf <- data.frame(id = 1:dim(SA2.df), yr_00 = SA2.test$treecover2000, lossyear = SA2.test$lossyear)
head(lossdf)
dim(lossdf)

lossdf <- lossdf %>% 
  mutate(year=0) %>% 
  expand(nesting(id, lossyear, yr_00), year = 0:17) %>% 
  mutate(loss_event = if_else(year >= lossyear & lossyear !=0 , 0, yr_00)) %>% 
  mutate(year = paste0("yr_", year)) %>% 
  spread(year, loss_event)

head(lossdf)

lossdf = subset(lossdf, select = -c(yr_0))
head(lossdf)

# Save lossdf as dataframe to drive
saveRDS(SA2.df, file = "RFiles/StudyAreas/SA2-dataset.Rda")
saveRDS(lossdf, file = "RFiles/StudyAreas/lossdf-SA2.Rda")
lossdf2 <- readRDS("RFiles/StudyAreas/lossdf-SA2.Rda")

# Transform dataframes into wide and long (panel) formats for regression 
# Wide format: 
# Create tibble extracting geometry and in_HD columns from tamonarang.df dataframe
geo_id = tibble(geometry = SA2.df[,7], in_HD = SA2.df[,8])
head(geo_id)

# Join geo_id and lossdf tibble 
SA2.wide = bind_cols(geo_id, lossdf2)
dim(SA2.wide)
head(SA2.wide)

# Panel format: 
SA2.panel = gather(SA2.wide, key = year, value = treecover, -c(geometry, in_HD, id, lossyear))
head(SA2.panel)

# Checking for 1 point 
filter(SA2.panel, id ==1)

# Save wide and panel formats: 
saveRDS(SA2.wide, file = "RFiles/StudyAreas/SA2wide.Rda")
saveRDS(SA2.panel, file = "RFiles/StudyAreas/SA2panel.Rda")

# Combine dataframes and reorder columns
full2 = bind_cols(SA2.df, SA2.wide)
full2 <- full2 %>% select(-(10:12))
full2 <- full2 %>% select(-(1:2)) 
full2 <- full2 %>% select("geometry", "in_HD", "distrelative", "lossyear1", 
                          "yr_00", "yr_1", "yr_2", "yr_3", "yr_4", "yr_5", "yr_6", "yr_7", "yr_8",
                          "yr_9", "yr_10", "yr_11", "yr_12", "yr_13", "yr_14", "yr_15", "yr_16", "yr_17",
                          "province", "forest_area", "forest_area_ENG", "study_area")
head(full2)

# Panel format: 
full2.panel = gather(full2, key = year, value = treecover, -c(geometry, in_HD, distrelative, 
                                                              lossyear1, province, forest_area, 
                                                              forest_area_ENG, study_area))
head(full2.panel)

saveRDS(full2, file = "RFiles/StudyAreas/SA2full.Rda")
saveRDS(full2.panel, file = "RFiles/StudyAreas/SA2fullpanel.Rda")

# Status: Done as of 9 Mar 2019

################################################################################
# (3) Study Area 3: Jambi - Tamonarang
# Load the dataframe
SA3 <- readRDS("RFiles/StudyAreas/StudyArea3.Rda")
head(SA3)

# Load the study area polygons
prot <- st_read("Polygons/Study Area 3/Jambi_1_Tamonarang_HL.shp")
hud <- st_read("Polygons/Study Area 3/Jambi_1_Tamonarang_HD.shp")

# Convert dataframe to sf object to coerce x,y columns into geometry column 
# Changing projection to CRS = 4326 will allow us to plot the land title polygons on top of sf object
SA3_sf <- st_as_sf(SA3, coords = c("x", "y"), crs = 4326)
head(SA3_sf)

## CREATE DUMMY VARIABLE: SFP (SOCIAL)

# Identify if point lies in HD or not HD 
# Plot polygons together to visually inspect data
plot(st_geometry(SA3_sf), col = "forest green", axes = TRUE) 
plot(st_geometry(prot), border = "black", add = TRUE)
plot(st_geometry(hud), border = "black", add = TRUE)

# Check if GFC data intersects with study area polygons 
# Create new column `in_HD`
SA3_sf$in_HD <- st_intersects(SA3_sf, hud, sparse = FALSE)
head(SA3_sf)

## CREATE RUNNING VARIABLE: (DISTANCE)

# Create the border of interest
# Identifying the border between polygons 
prot_sp <- as(prot, Class = "Spatial")
hud_sp <- as(hud, Class = "Spatial")

# In case of issues with self-intersection: 
prot <- st_make_valid(prot)
hud <- st_make_valid(hud)

border = st_intersection(prot, hud)
border_sp <- as(border, Class = "Spatial")

# Convert border from polygon to spatial line
border_line <- as(border_sp, Class = "SpatialLines")

# Calculate length of border
SpatialLinesLengths(border_line, longlat = TRUE)
# 4.95759
# The border is 5 km long. 

# Checking the plots
plot(st_geometry(SA3_sf), col = "forest green", axes = TRUE) 
plot(st_geometry(prot), border = "black", add = TRUE)
plot(st_geometry(hud), border = "black", add = TRUE)
plot(border_line, col = "red", add = TRUE)

# Calculate distance from border 
# Convert tamonarang_sf to dataframe 
SA3.df <- as.data.frame(SA3_sf, xy = TRUE)
head(SA3.df)
head(SA3)

# Create matrix with latitude and longitude stored in x and y columns 
covermatrix <- SA3[,(1:2)]
head(covermatrix)

# Calculate distance and relative distance, create new columns to store the values
dist.SA3 <- dist2Line(covermatrix, border_line)
head(dist.SA3)
SA3.df$distrelative <- ifelse(SA3.df$in_HD==TRUE, dist.SA3[,1], -dist.SA3[,1])
summary(SA3.df$distrelative)
head(SA3.df)

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

# Calculate forest cover in each year (based on lossyear only) for regression plots
# and for robustness checks
SA3.test <- SA3.df

lossdf <- data.frame(id = 1:dim(SA3.df), yr_00 = SA3.test$treecover2000, lossyear = SA3.test$lossyear)
head(lossdf)
dim(lossdf)

lossdf <- lossdf %>% 
  mutate(year=0) %>% 
  expand(nesting(id, lossyear, yr_00), year = 0:17) %>% 
  mutate(loss_event = if_else(year >= lossyear & lossyear !=0 , 0, yr_00)) %>% 
  mutate(year = paste0("yr_", year)) %>% 
  spread(year, loss_event)

head(lossdf)

lossdf = subset(lossdf, select = -c(yr_0))
head(lossdf)

# Save dataframes to drive
saveRDS(SA3.df, file = "RFiles/StudyAreas/SA3-dataset.Rda")
saveRDS(lossdf, file = "RFiles/StudyAreas/lossdf-SA3.Rda")
lossdf3 <- readRDS("RFiles/StudyAreas/lossdf-SA3.Rda")

# Transform dataframes into wide and long (panel) formats for regression 
# Wide format: 
# Create tibble extracting geometry and in_HD columns from tamonarang.df dataframe
geo_id = tibble(geometry = SA3.df[,7], in_HD = SA3.df[,8])
head(geo_id)

# Join geo_id and lossdf tibble 
SA3.wide = bind_cols(geo_id, lossdf3)
dim(SA3.wide)
head(SA3.wide)

# Panel format: 
SA3.panel = gather(SA3.wide, key = year, value = treecover, -c(geometry, in_HD, id, lossyear))
head(SA3.panel)

# Checking for 1 point 
filter(SA3.panel, id ==1)

# Save wide and panel formats: 
saveRDS(SA3.wide, file = "RFiles/StudyAreas/SA3wide.Rda")
saveRDS(SA3.panel, file = "RFiles/StudyAreas/SA3panel.Rda")

# Combine dataframes and reorder columns
full3 = bind_cols(SA3.df, SA3.wide)
full3 <- full3 %>% select(-(10:12))
full3 <- full3 %>% select(-(1:2)) 
full3 <- full3 %>% select("geometry", "in_HD", "distrelative", "lossyear1", 
                          "yr_00", "yr_1", "yr_2", "yr_3", "yr_4", "yr_5", "yr_6", "yr_7", "yr_8",
                          "yr_9", "yr_10", "yr_11", "yr_12", "yr_13", "yr_14", "yr_15", "yr_16", "yr_17",
                          "province", "forest_area", "forest_area_ENG", "study_area")
head(full3)

# Panel format: 
full3.panel = gather(full3, key = year, value = treecover, -c(geometry, in_HD, distrelative, 
                                                              lossyear1, province, forest_area, 
                                                              forest_area_ENG, study_area))
head(full3.panel)

saveRDS(full3, file = "RFiles/StudyAreas/SA3full.Rda")
saveRDS(full3.panel, file = "RFiles/StudyAreas/SA3fullpanel.Rda")

# Status: Done as of 9 Mar 2019

################################################################################
# (4) Study Area 4: Sumatra Selantan - Near Kota Pagalaram
# Load the dataframe
SA4 <- readRDS("RFiles/StudyAreas/StudyArea4.Rda")
head(SA4)

# Load the study area polygons
prot <- st_read("Polygons/Study Area 4/S3_Near_Kota_Pagaralam_HL.shp")
hud <- st_read("Polygons/Study Area 4/S3_Near_Kota_Pagaralam_HD.shp")

# Convert dataframe to sf object to coerce x,y columns into geometry column 
# Changing projection to CRS = 4326 will allow us to plot the land title polygons on top of sf object
SA4_sf <- st_as_sf(SA4, coords = c("x", "y"), crs = 4326)
head(SA4_sf)

## CREATE DUMMY VARIABLE: SFP (SOCIAL)

# Identify if point lies in HD or not HD 
# Plot polygons together to visually inspect data
plot(st_geometry(SA4_sf), col = "forest green", axes = TRUE) 
plot(st_geometry(prot), border = "black", add = TRUE)
plot(st_geometry(hud), border = "black", add = TRUE)

# Check if GFC data intersects with study area polygons 
# Create new column `in_HD`
SA4_sf$in_HD <- st_intersects(SA4_sf, hud, sparse = FALSE)
head(SA4_sf)

## CREATE RUNNING VARIABLE: (DISTANCE)

# Create the border of interest
# Identifying the border between polygons 
prot_sp <- as(prot, Class = "Spatial")
hud_sp <- as(hud, Class = "Spatial")

# In case of issues with self-intersection: 
prot <- st_make_valid(prot)
hud <- st_make_valid(hud)

border = st_intersection(prot, hud)
border_sp <- as(border, Class = "Spatial")

# Convert border from polygon to spatial line
border_line <- as(border_sp, Class = "SpatialLines")

# Calculate length of border
SpatialLinesLengths(border_line, longlat = TRUE)
# 4.95759
# The border is 5 km long. 

# Checking the plots
plot(st_geometry(SA4_sf), col = "forest green", axes = TRUE) 
plot(st_geometry(prot), border = "black", add = TRUE)
plot(st_geometry(hud), border = "black", add = TRUE)
plot(border_line, col = "red", add = TRUE)

# Calculate distance from border 
# Convert tamonarang_sf to dataframe 
SA4.df <- as.data.frame(SA4_sf, xy = TRUE)
head(SA4.df)
head(SA4)

# Create matrix with latitude and longitude stored in x and y columns 
covermatrix <- SA4[,-(3:8)]
head(covermatrix)

# Calculate distance and relative distance, create new columns to store the values
dist.SA4 <- dist2Line(covermatrix, border_line)
head(dist.SA4)
SA4.df$distrelative <- ifelse(SA4.df$in_HD==TRUE, dist.SA4[,1], -dist.SA4[,1])
summary(SA4.df$distrelative)
head(SA4.df)

## CREATE OUTCOME VARIABLE: FOREST COVER LOSS

# Deforestation defined as average annual rate of forest cover loss
# We first assume that forest cover is recorded as forest cover in this dataset.
# Later, in robustness checks, we will overlay this with known plantation shapefiles to verify 
# if plantations have been recorded as forest cover. 
# As mentioned in Puyravaud (2003), we can use average rate of change formula
# For the difference-in-difference estimation, our point of comparison is 2008
# According to Afiff (2016), laws relating to the SFP scheme were: 
# (2007) Government Regulation 6/2007 on Forest Governance, Planning & Utilisation; and
# (2008) Minister of Forest Regulation P.49/Menhut-II/2008 on Village Forest, which laid out the mechanism for obtaining an SFP permit 

# Create treecover2008 variable 
SA4.df$treecover2008 <- ifelse(SA4.df$lossyear <=8 & SA4.df$lossyear >0, 0, SA4.df$treecover2000)

# Create variables for forest cover pre-2008 and post-2008
SA4.df$cover_pre08 <- ifelse(SA4.df$lossyear <= 8 & SA4.df$lossyear > 0, 0, SA4.df$treecover2000)
SA4.df$cover_post08 <- ifelse(SA4.df$lossyear > 0, 0, SA4.df$treecover2000)

# Rate of forest cover loss (f1, f2): -(f2 - f1)/t
# (f2-f1)/t only calculates rate of change. If rate of change is negative, then it denotes loss. 
# As this column is concerned with loss, we convert negative values into positive values for ease of interpretation.
SA4.df$prerate <- -(SA4.df$cover_pre08 - SA4.df$treecover2000)/8
SA4.df$postrate <- -(SA4.df$cover_post08 - SA4.df$treecover2008)/8
head(SA4.df)
summary(SA4.df)

# Save dataframe to drive
saveRDS(SA4.df, file = "RFiles/StudyAreas/SA4-dataset.Rda")

# Calculate forest cover in each year (based on lossyear only) for regression plots
# and for robustness checks
SA4.test <- SA4.df

lossdf <- data.frame(id = 1:dim(SA4.df), yr_00 = SA4.test$treecover2000, lossyear = SA4.test$lossyear)
head(lossdf)
dim(lossdf)

lossdf <- lossdf %>% 
  mutate(year=0) %>% 
  expand(nesting(id, lossyear, yr_00), year = 0:17) %>% 
  mutate(loss_event = if_else(year >= lossyear & lossyear !=0 , 0, yr_00)) %>% 
  mutate(year = paste0("yr_", year)) %>% 
  spread(year, loss_event)

head(lossdf)

lossdf = subset(lossdf, select = -c(yr_0))
head(lossdf)

# Insert treecover values for 2017 
SA4.df$treecover2017 = lossdf$yr_17
head(SA4.df)

# Save lossdf as dataframe to drive
saveRDS(lossdf, file = "RFiles/StudyAreas/lossdf-SA4.Rda")
lossdf4 <- readRDS("RFiles/StudyAreas/lossdf-SA4.Rda")

# Transform dataframes into wide and long (panel) formats for regression 
# Wide format: 
# Create tibble extracting geometry and in_HD columns from tamonarang.df dataframe
geo_id = tibble(geometry = SA4.df[,7], in_HD = SA4.df[,8])
head(geo_id)

# Join geo_id and lossdf tibble 
SA4.wide = bind_cols(geo_id, lossdf4)
dim(SA4.wide)
head(SA4.wide)

# Panel format: 
SA4.panel = gather(SA4.wide, key = year, value = treecover, -c(geometry, in_HD, id, lossyear))
head(SA4.panel)
View(SA4.panel)

# Checking for 1 point 
filter(SA4.panel, id ==1)

# Save wide and panel formats: 
saveRDS(SA4.wide, file = "RFiles/StudyAreas/SA4wide.Rda")
saveRDS(SA4.panel, file = "RFiles/StudyAreas/SA4panel.Rda")

# Combine dataframes and reorder columns
SA4filter <- SA4.df %>% select(-(10:15))

full4 = bind_cols(SA4filter, SA4.wide)
full4 <- full4 %>% select(-(10:12))
full4 <- full4 %>% select(-(1:2)) 
full4 <- full4 %>% select("geometry", "in_HD", "distrelative", "lossyear1", 
                          "yr_00", "yr_1", "yr_2", "yr_3", "yr_4", "yr_5", "yr_6", "yr_7", "yr_8",
                          "yr_9", "yr_10", "yr_11", "yr_12", "yr_13", "yr_14", "yr_15", "yr_16", "yr_17",
                          "province", "forest_area", "forest_area_ENG", "study_area")

# Panel format: 
full4.panel = gather(full4, key = year, value = treecover, -c(geometry, in_HD, distrelative, 
                                                              lossyear1, province, forest_area, 
                                                              forest_area_ENG, study_area))
head(full4.panel)

saveRDS(full4, file = "RFiles/StudyAreas/SA4full.Rda")
saveRDS(full4.panel, file = "RFiles/StudyAreas/SA4fullpanel.Rda")

# Status: Done as of 9 Mar 2019

##############################################################################################
# Pooling of datasets
# Testing with only HD on HL
# Study Areas 1 to 4
SA1.df <- readRDS("RFiles/StudyAreas/SA1full.Rda")
SA2.df <- readRDS("RFiles/StudyAreas/SA2full.Rda")
SA3.df <- readRDS("RFiles/StudyAreas/SA3full.Rda")
SA4.df <- readRDS("RFiles/StudyAreas/SA4full.Rda")

# Convert Study Areas into numerics
SA1.df <- SA1.df %>% mutate(study_area = recode(study_area, "Study Area 1" = 1))
head(SA1.df)

SA2.df <- SA2.df %>% mutate(study_area = recode(study_area, "Study Area 2" = 2))
head(SA2.df)

SA3.df <- SA3.df %>% mutate(study_area = recode(study_area, "Study Area 3" = 3))
head(SA3.df)

SA4.df <- SA4.df %>% mutate(study_area = recode(study_area, "Study Area 4" = 4))
head(SA4.df)

pooled <- bind_rows(SA1.df, SA2.df, SA3.df, SA4.df)
pooledJambi <- bind_rows(SA1.df, SA2.df, SA3.df)

# Convert study areas into factor variables
pooled$study_area <- factor(pooled$study_area)
str(pooled)

# Save dataframe to drive 
saveRDS(pooled, file = "RFiles/StudyAreas/pooled.Rda")
saveRDS(pooledJambi, file = "RFiles/StudyAreas/pooledJambi.Rda")