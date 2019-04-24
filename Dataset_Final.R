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

# 1. Creation of study areas
############################################################################
# (1) Study Area 1: Jambi - Near HT-25

# Load study area polygons
prot <- st_read("Polygons/Study Area 1/J1_nearHT-25_HL.shp")
hud <- st_read("Polygons/Study Area 1/J1_nearHT-25_HD.shp")

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

# Create new column for study area 
forestcover_1$study_area <- c("Study Area 1")

# Save dataframe to drive
saveRDS(forestcover_1, file = "RFiles/StudyAreas/StudyArea1.Rda")

# Status: Done as of 9 Mar 2019

############################################################################
# (2) Study Area 2: Jambi - Lubuk Beringin

# Load study area polygons
prot <- st_read("Polygons/Study Area 2/J1_Lubuk_Beringin_HL.shp")
hud <- st_read("Polygons/Study Area 2/J1_Lubuk_Beringin_HD.shp")

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

# Create new column for study area 
forestcover_2$study_area <- c("Study Area 2")

# Save dataframe to drive
saveRDS(forestcover_2, file = "RFiles/StudyAreas/StudyArea2.Rda")

# Status: Done as of 9 Mar 2019

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

# Create new column for study area 
forestcover_3$study_area <- c("Study Area 3")

# Save dataframe to drive
saveRDS(forestcover_3, file = "RFiles/StudyAreas/StudyArea3.Rda")

# Status: Done as of 9 Mar 2019

##########################################################################################################
# (4) Study Area 4: Sumatra Selantan - Near Kota Pagalaram

# Load study area polygons
prot <- st_read("Polygons/Study Area 4/S3_Near_Kota_Pagaralam_HL.shp")
hud <- st_read("Polygons/Study Area 4/S3_Near_Kota_Pagaralam_HD.shp")

# Crop raster layers to land_titles polygon
lossyear_prot <- mask(lossyear, mask = prot)
lossyear_prot_crop <- crop(lossyear_prot, prot)
plot(lossyear_prot_crop)

lossyear_hd <- mask(lossyear, mask = hud)
lossyear_hd_crop <- crop(lossyear_hd, hud)

lossyear_4 <- merge(lossyear_prot_crop, lossyear_hd_crop)
plot(lossyear_4)

treecover_prot <- mask(treecover, mask = prot)
treecover_prot_crop <- crop(treecover_prot, prot)

treecover_hd <- mask(treecover, mask = hud)
treecover_hd_crop <- crop(treecover_hd, hud)

treecover_4 <- merge(treecover_prot_crop, treecover_hd_crop)
plot(treecover_4)

# Create raster stack
crs(lossyear_4) <- mercator
crs(treecover_4) <- mercator

forestcover_stack4 = stack(treecover_4, lossyear_4)
head(forestcover_stack4)

# Coerce raster to dataframe 
forestcover_4_df <- as.data.frame(forestcover_stack4, xy = TRUE)
sum(is.na(forestcover_4_df))
forestcover_4_clean <- na.omit(forestcover_4_df)
sum(is.na(forestcover_4_clean))
head(forestcover_4_clean)
names(forestcover_4_clean)[3] <- "treecover2000"
names(forestcover_4_clean)[4] <- "lossyear"
forestcover_4 <- forestcover_4_clean
head(forestcover_4)

# Create new column for Province 
forestcover_4$province <- c("Sumatra Selantan")
head(forestcover_4)

# Create new column for Forest Area Type
forestcover_4$forest_area <- c("Hutan Lindung")
forestcover_4$forest_area_ENG <- c("Protection")
head(forestcover_4)

# Create new column for study area 
forestcover_4$study_area <- c("Study Area 4")

# Save dataframe to drive
saveRDS(forestcover_4, file = "RFiles/StudyAreas/StudyArea4.Rda")

# Status: Done as of 9 Mar 2019

##########################################################################################################
# (5) Study Area 5: W. Kalimantan - K2 Nangaputih 

# Load study area polygons
prot <- st_read("Polygons/Study Area 5/K2_Nangaputih_HL.shp")
hud <- st_read("Polygons/Study Area 5/K2_Nangaputih_HD.shp")

# Load masked raster layers from QGIS
lossyear_prot <- raster("gfc/Kalimantan/lossyear_K2_HL.tif")
lossyear_hd <- raster("gfc/Kalimantan/lossyear_K2_HD.tif")
treecover_prot <- raster("gfc/Kalimantan/treecover_K2_HL.tif")
treecover_hd <- raster("gfc/Kalimantan/treecover_K2_HD.tif")

# Mask and Crop raster layers to polygon layer
# Lossyear
lossyear_prot1 <- mask(lossyear_prot, prot)
lossyear_hd1 <- mask(lossyear_hd, hud)

lossyear_prot_crop <- crop(lossyear_prot1, prot)
lossyear_hd_crop <- crop(lossyear_hd1, hud)

lossyear_5 <- merge(lossyear_prot_crop, lossyear_hd_crop)
plot(lossyear_5)

# Treecover
treecover_prot1 <- mask(treecover_prot, prot)
treecover_hd1 <- mask(treecover_hd, hud)

treecover_prot_crop <- crop(treecover_prot1, prot)
treecover_hd_crop <- crop(treecover_hd1, hud)

treecover_5 <- merge(treecover_prot_crop, treecover_hd_crop)
plot(treecover_5)

# Create raster stack
crs(lossyear_5) <- mercator
crs(treecover_5) <- mercator

forestcover_stack5 = stack(treecover_5, lossyear_5)
head(forestcover_stack5)

# Coerce raster to dataframe 
forestcover_5_df <- as.data.frame(forestcover_stack5, xy = TRUE)
sum(is.na(forestcover_5_df))
forestcover_5_clean <- na.omit(forestcover_5_df)
sum(is.na(forestcover_5_clean))
head(forestcover_5_clean)
names(forestcover_5_clean)[3] <- "treecover2000"
names(forestcover_5_clean)[4] <- "lossyear"
forestcover_5 <- forestcover_5_clean
head(forestcover_5)

# Create new column for Province 
forestcover_5$province <- c("Kalimantan Barat")
head(forestcover_5)

# Create new column for Forest Area Type
forestcover_5$forest_area <- c("Hutan Lindung")
forestcover_5$forest_area_ENG <- c("Protection")
head(forestcover_5)

# Create new column for study area 
forestcover_5$study_area <- c("Study Area 5")

# Save dataframe to drive
saveRDS(forestcover_5, file = "RFiles/StudyAreas/StudyArea5.Rda")

# Status: Done as of 15 Mar 2019

##########################################################################################################
# (6) Study Area 6: W. Kalimantan - K2 Near HT-0

# Load study area polygons
prot <- st_read("Polygons/Study Area 6/K2_Near_HT-0_HL.shp")
hud <- st_read("Polygons/Study Area 6/K2_Near_HT-0_HD.shp")

# Load masked raster layers from QGIS
lossyear_prot <- raster("gfc/Kalimantan/lossyear_6_HL.tif")
lossyear_hd <- raster("gfc/Kalimantan/lossyear_6_HD.tif")
treecover_prot <- raster("gfc/Kalimantan/treecover_6_HL.tif")
treecover_hd <- raster("gfc/Kalimantan/treecover_6_HD.tif")

# Mask and Crop raster layers to polygon layer
# Lossyear
lossyear_prot1 <- mask(lossyear_prot, prot)
lossyear_hd1 <- mask(lossyear_hd, hud)

lossyear_prot_crop <- crop(lossyear_prot1, prot)
lossyear_hd_crop <- crop(lossyear_hd1, hud)

lossyear_6 <- merge(lossyear_prot_crop, lossyear_hd_crop)
plot(lossyear_6)

# Treecover
treecover_prot1 <- mask(treecover_prot, prot)
treecover_hd1 <- mask(treecover_hd, hud)

treecover_prot_crop <- crop(treecover_prot1, prot)
treecover_hd_crop <- crop(treecover_hd1, hud)

treecover_6 <- merge(treecover_prot_crop, treecover_hd_crop)
plot(treecover_6)

# Create raster stack
crs(lossyear_6) <- mercator
crs(treecover_6) <- mercator

forestcover_stack6 = stack(treecover_6, lossyear_6)
head(forestcover_stack6)

# Coerce raster to dataframe 
forestcover_6_df <- as.data.frame(forestcover_stack6, xy = TRUE)
sum(is.na(forestcover_6_df))
forestcover_6_clean <- na.omit(forestcover_6_df)
sum(is.na(forestcover_6_clean))
head(forestcover_6_clean)
names(forestcover_6_clean)[3] <- "treecover2000"
names(forestcover_6_clean)[4] <- "lossyear"
forestcover_6 <- forestcover_6_clean
head(forestcover_6)

# Create new column for Province 
forestcover_6$province <- c("Kalimantan Barat")
head(forestcover_6)

# Create new column for Forest Area Type
forestcover_6$forest_area <- c("Hutan Lindung")
forestcover_6$forest_area_ENG <- c("Protection")
head(forestcover_6)

# Create new column for study area 
forestcover_6$study_area <- c("Study Area 6")

# Save dataframe to drive
saveRDS(forestcover_6, file = "RFiles/StudyAreas/StudyArea6.Rda")

# Status: Done as of 15 Mar 2019

##########################################################################################################
# (7) Study Area 7: N. Kalimantan - EK5 HT-21 

# Load study area polygons
prot <- st_read("Polygons/Study Area 7/EK_HT-21_HL.shp")
hkm <- st_read("Polygons/Study Area 7/EK_HT-21_HKm.shp")

# Load masked raster layers from QGIS
lossyear_prot <- raster("gfc/Kalimantan/lossyear_7_HL.tif")
lossyear_hkm <- raster("gfc/Kalimantan/lossyear_7_HKm.tif")
treecover_prot <- raster("gfc/Kalimantan/treecover_7_HL.tif")
treecover_hkm <- raster("gfc/Kalimantan/treecover_7_HKm.tif")

# Mask and Crop raster layers to polygon layer
# Lossyear
lossyear_prot1 <- mask(lossyear_prot, prot)
lossyear_hkm1 <- mask(lossyear_hkm, hkm)

lossyear_prot_crop <- crop(lossyear_prot1, prot)
lossyear_hkm_crop <- crop(lossyear_hkm1, hkm)

lossyear_7 <- merge(lossyear_prot_crop, lossyear_hkm_crop)
plot(lossyear_7)

# Treecover
treecover_prot1 <- mask(treecover_prot, prot)
treecover_hkm1 <- mask(treecover_hkm, hkm)

treecover_prot_crop <- crop(treecover_prot1, prot)
treecover_hkm_crop <- crop(treecover_hkm, hkm)

treecover_7 <- merge(treecover_prot_crop, treecover_hkm_crop)
plot(treecover_7)

# Create raster stack
crs(lossyear_7) <- mercator
crs(treecover_7) <- mercator

forestcover_stack7 = stack(treecover_7, lossyear_7)
head(forestcover_stack7)

# Coerce raster to dataframe 
forestcover_7_df <- as.data.frame(forestcover_stack7, xy = TRUE)
sum(is.na(forestcover_7_df))
forestcover_7_clean <- na.omit(forestcover_7_df)
sum(is.na(forestcover_7_clean))
head(forestcover_7_clean)
names(forestcover_7_clean)[3] <- "treecover2000"
names(forestcover_7_clean)[4] <- "lossyear"
forestcover_7 <- forestcover_7_clean
head(forestcover_7)

# Create new column for Province 
forestcover_7$province <- c("Kalimantan Utara")
head(forestcover_7)

# Create new column for Forest Area Type
forestcover_7$forest_area <- c("Hutan Lindung")
forestcover_7$forest_area_ENG <- c("Protection")
head(forestcover_7)

# Create new column for study area 
forestcover_7$study_area <- c("Study Area 7")

# Save dataframe to drive
saveRDS(forestcover_7, file = "RFiles/StudyAreas/StudyArea7.Rda")

# Status: Done as of 15 Mar 2019

##########################################################################################################
# (8) Study Area 8: Lampung - L Unit IX-KPHL

# Load study area polygons
prot <- st_read("Polygons/Study Area 8/L_UNIT_IX-KPHL_HL.shp")
hkm <- st_read("Polygons/Study Area 8/L_UNIT_IX-KPHL_HKm.shp")

# Load masked raster layers from QGIS
lossyear_prot <- raster("gfc/Lampung/lossyear_8_HL.tif")
lossyear_hkm <- raster("gfc/Lampung/lossyear_8_HKm.tif")
treecover_prot <- raster("gfc/Lampung/treecover_8_HL.tif")
treecover_hkm <- raster("gfc/Lampung/treecover_8_HKm.tif")

# Mask and Crop raster layers to polygon layer
# Lossyear
lossyear_prot1 <- mask(lossyear_prot, prot)
lossyear_hkm1 <- mask(lossyear_hkm, hkm)

lossyear_prot_crop <- crop(lossyear_prot1, prot)
lossyear_hkm_crop <- crop(lossyear_hkm1, hkm)

lossyear_8 <- merge(lossyear_prot_crop, lossyear_hkm_crop)
plot(lossyear_8)

# Treecover
treecover_prot1 <- mask(treecover_prot, prot)
treecover_hkm1 <- mask(treecover_hkm, hkm)

treecover_prot_crop <- crop(treecover_prot1, prot)
treecover_hkm_crop <- crop(treecover_hkm, hkm)

treecover_8 <- merge(treecover_prot_crop, treecover_hkm_crop)
plot(treecover_8)

# Create raster stack
crs(lossyear_8) <- mercator
crs(treecover_8) <- mercator

forestcover_stack8 = stack(treecover_8, lossyear_8)
head(forestcover_stack8)

# Coerce raster to dataframe 
forestcover_8_df <- as.data.frame(forestcover_stack8, xy = TRUE)
sum(is.na(forestcover_8_df))
forestcover_8_clean <- na.omit(forestcover_8_df)
sum(is.na(forestcover_8_clean))
head(forestcover_8_clean)
names(forestcover_8_clean)[3] <- "treecover2000"
names(forestcover_8_clean)[4] <- "lossyear"
forestcover_8 <- forestcover_8_clean
head(forestcover_8)

# Create new column for Province 
forestcover_8$province <- c("Lampung")
head(forestcover_8)

# Create new column for Forest Area Type
forestcover_8$forest_area <- c("Hutan Lindung")
forestcover_8$forest_area_ENG <- c("Protection")
head(forestcover_8)

# Create new column for study area 
forestcover_8$study_area <- c("Study Area 8")

# Save dataframe to drive
saveRDS(forestcover_8, file = "RFiles/StudyAreas/StudyArea8.Rda")

# Status: Done as of 15 Mar 2019

##########################################################################################################
# (9) Study Area 9: Aceh - N KAB. PIDIE

# Load study area polygons
prot <- st_read("Polygons/Study Area 9/N_KAB._PIDIE_HL.shp")
hkm <- st_read("Polygons/Study Area 9/N_KAB._PIDIE_HKm.shp")

# Load masked raster layers from QGIS
lossyear_prot <- raster("gfc/Aceh/lossyear_9_HL.tif")
lossyear_hkm <- raster("gfc/Aceh/lossyear_9_HKm.tif")
treecover_prot <- raster("gfc/Aceh/treecover_9_HL.tif")
treecover_hkm <- raster("gfc/Aceh/treecover_9_HKm.tif")

# Mask and Crop raster layers to polygon layer
# Lossyear
lossyear_prot1 <- mask(lossyear_prot, prot)
lossyear_hkm1 <- mask(lossyear_hkm, hkm)

lossyear_prot_crop <- crop(lossyear_prot1, prot)
lossyear_hkm_crop <- crop(lossyear_hkm1, hkm)

lossyear_9 <- merge(lossyear_prot_crop, lossyear_hkm_crop)
plot(lossyear_9)

# Treecover
treecover_prot1 <- mask(treecover_prot, prot)
treecover_hkm1 <- mask(treecover_hkm, hkm)

treecover_prot_crop <- crop(treecover_prot1, prot)
treecover_hkm_crop <- crop(treecover_hkm, hkm)

treecover_9 <- merge(treecover_prot_crop, treecover_hkm_crop)
plot(treecover_9)

# Create raster stack
crs(lossyear_9) <- mercator
crs(treecover_9) <- mercator

forestcover_stack9 = stack(treecover_9, lossyear_9)
head(forestcover_stack9)

# Coerce raster to dataframe 
forestcover_9_df <- as.data.frame(forestcover_stack9, xy = TRUE)
sum(is.na(forestcover_9_df))
forestcover_9_clean <- na.omit(forestcover_9_df)
sum(is.na(forestcover_9_clean))
head(forestcover_9_clean)
names(forestcover_9_clean)[3] <- "treecover2000"
names(forestcover_9_clean)[4] <- "lossyear"
forestcover_9 <- forestcover_9_clean
head(forestcover_9)

# Create new column for Province 
forestcover_9$province <- c("Aceh")
head(forestcover_9)

# Create new column for Forest Area Type
forestcover_9$forest_area <- c("Hutan Lindung")
forestcover_9$forest_area_ENG <- c("Protection")
head(forestcover_9)

# Create new column for study area 
forestcover_9$study_area <- c("Study Area 9")

# Save dataframe to drive
saveRDS(forestcover_9, file = "RFiles/StudyAreas/StudyArea9.Rda")

# Status: Done as of 15 Mar 2019

##########################################################################################################
# (10) Study Area 10: W. Sulawesi - WS UNIT VIII - KPHL

# Load study area polygons
prot <- st_read("Polygons/Study Area 10/WS_UNIT_VIII-KPHL_HL.shp")
hkm <- st_read("Polygons/Study Area 10/WS_Fixed_HKm.shp")

# Load masked raster layers from QGIS
lossyear_prot <- raster("gfc/Sulawesi/lossyear_10_HL.tif")
lossyear_hkm <- raster("gfc/Sulawesi/lossyear_10_HKm.tif")
treecover_prot <- raster("gfc/Sulawesi/treecover_10_HL.tif")
treecover_hkm <- raster("gfc/Sulawesi/treecover_10_HKm.tif")

# Mask and Crop raster layers to polygon layer
# Lossyear
lossyear_prot1 <- mask(lossyear_prot, prot)
lossyear_hkm1 <- mask(lossyear_hkm, hkm)

lossyear_prot_crop <- crop(lossyear_prot1, prot)
lossyear_hkm_crop <- crop(lossyear_hkm1, hkm)

lossyear_10 <- merge(lossyear_prot_crop, lossyear_hkm_crop)
plot(lossyear_10)

# Treecover
treecover_prot1 <- mask(treecover_prot, prot)
treecover_hkm1 <- mask(treecover_hkm, hkm)

treecover_prot_crop <- crop(treecover_prot1, prot)
treecover_hkm_crop <- crop(treecover_hkm, hkm)

treecover_10 <- merge(treecover_prot_crop, treecover_hkm_crop)
plot(treecover_10)

# Create raster stack
crs(lossyear_10) <- mercator
crs(treecover_10) <- mercator

forestcover_stack10 = stack(treecover_10, lossyear_10)
head(forestcover_stack10)

# Coerce raster to dataframe 
forestcover_10_df <- as.data.frame(forestcover_stack10, xy = TRUE)
sum(is.na(forestcover_10_df))
forestcover_10_clean <- na.omit(forestcover_10_df)
sum(is.na(forestcover_10_clean))
head(forestcover_10_clean)
names(forestcover_10_clean)[3] <- "treecover2000"
names(forestcover_10_clean)[4] <- "lossyear"
forestcover_10 <- forestcover_10_clean
head(forestcover_10)

# Create new column for Province 
forestcover_10$province <- c("Sulawesi Barat")
head(forestcover_10)

# Create new column for Forest Area Type
forestcover_10$forest_area <- c("Hutan Lindung")
forestcover_10$forest_area_ENG <- c("Protection")
head(forestcover_10)

# Create new column for study area 
forestcover_10$study_area <- c("Study Area 10")

# Save dataframe to drive
saveRDS(forestcover_10, file = "RFiles/StudyAreas/StudyArea10.Rda")

# Status: Done as of 16 Mar 2019

##########################################################################################################
# (11) Study Area 11: Riau: R3 Top-left corner

# Load study area polygons
prot <- st_read("Polygons/Study Area 11/R3_Top_Left_Corner_HL.shp")
hud <- st_read("Polygons/Study Area 11/R3_Top_Left_Corner_HD.shp")

# Load masked raster layers from QGIS
lossyear_prot <- raster("gfc/Riau/lossyear_11_HL.tif")
lossyear_hd <- raster("gfc/Riau/lossyear_11_HD.tif")
treecover_prot <- raster("gfc/Riau/treecover_11_HL.tif")
treecover_hd <- raster("gfc/Riau/treecover_11_HD.tif")

# Mask and Crop raster layers to polygon layer
# Lossyear
lossyear_prot1 <- mask(lossyear_prot, prot)
lossyear_hd1 <- mask(lossyear_hd, hud)

lossyear_prot_crop <- crop(lossyear_prot1, prot)
lossyear_hd_crop <- crop(lossyear_hd1, hud)

lossyear_11 <- merge(lossyear_prot_crop, lossyear_hd_crop)
plot(lossyear_11)

# Treecover
treecover_prot1 <- mask(treecover_prot, prot)
treecover_hd1 <- mask(treecover_hd, hud)

treecover_prot_crop <- crop(treecover_prot1, prot)
treecover_hd_crop <- crop(treecover_hd, hud)

treecover_11 <- merge(treecover_prot_crop, treecover_hd_crop)
plot(treecover_11)

# Create raster stack
crs(lossyear_11) <- mercator
crs(treecover_11) <- mercator

forestcover_stack11 = stack(treecover_11, lossyear_11)
head(forestcover_stack11)

# Coerce raster to dataframe 
forestcover_11_df <- as.data.frame(forestcover_stack11, xy = TRUE)
sum(is.na(forestcover_11_df))
forestcover_11_clean <- na.omit(forestcover_11_df)
sum(is.na(forestcover_11_clean))
head(forestcover_11_clean)
names(forestcover_11_clean)[3] <- "treecover2000"
names(forestcover_11_clean)[4] <- "lossyear"
forestcover_11 <- forestcover_11_clean
head(forestcover_11)

# Create new column for Province 
forestcover_11$province <- c("Riau")
head(forestcover_11)

# Create new column for Forest Area Type
forestcover_11$forest_area <- c("Hutan Lindung")
forestcover_11$forest_area_ENG <- c("Protection")
head(forestcover_11)

# Create new column for study area 
forestcover_11$study_area <- c("Study Area 11")

# Save dataframe to drive
saveRDS(forestcover_11, file = "RFiles/StudyAreas/StudyArea11.Rda")

# Status: Done as of 16 Mar 2019

# 2. Creation of variables 
################################################################################
# (1) Study Area 1: Jambi - Near HT-25 
# Load the dataframe
SA1 <- readRDS("RFiles/StudyAreas/StudyArea1.Rda")
head(SA1)

# Load the study area polygons
prot <- st_read("Polygons/Study Area 1/HL_Diff_1.shp")
hud <- st_read("Polygons/Study Area 1/HD_Diff_1.shp")
line <- st_read("Polygons/Study Area 1/Intersection_1.shp")

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
plot(st_geometry(line), col = "red", add = TRUE)

# Check if GFC data intersects with study area polygons 
# Create new column `in_HD`
SA1_sf$in_HD <- st_intersects(SA1_sf, line, sparse = FALSE)
SA1_sf$in_HD <- na_if(SA1_sf$in_HD, "TRUE")
sum(is.na(SA1_sf))
SA1_sf <- na.omit(SA1_sf)
sum(is.na((SA1_sf)))

SA1_sf$in_HD <- st_intersects(SA1_sf, hud, sparse = FALSE)
head(SA1_sf)

## CREATE RUNNING VARIABLE: (DISTANCE)

# Convert border to spatial line
line_sp <- as(line, Class = "Spatial")

# Convert border from polygon to spatial line
border_line <- as(line_sp, Class = "SpatialLines")

# Calculate length of border
SpatialLinesLengths(border_line, longlat = TRUE)
# 16.06598
# The border is 16 km long. 

# Checking the plots
plot(st_geometry(SA1_sf), col = "forest green", axes = TRUE) 
plot(st_geometry(prot), border = "black", add = TRUE)
plot(st_geometry(hud), border = "black", add = TRUE)
plot(border_line, col = "red", add = TRUE)

# Calculate distance from border 
# Convert sf to dataframe to insert distrelative column 
SA1.df <- as.data.frame(SA1_sf, xy = TRUE)

# Extract geometry column from sf and convert into matrix 
# with latitude and longitude stored in x and y columns  
geometry <- SA1_sf$geometry
geom <- st_coordinates(geometry)

# Calculate distance and relative distance, create new columns to store the values
dist.SA1 <- dist2Line(geom, border_line)
head(dist.SA1)
SA1.df$distrelative <- ifelse(SA1.df$in_HD==TRUE, dist.SA1[,1], -dist.SA1[,1])
summary(SA1.df$distrelative)
head(SA1.df)

# Save dataframe to drive
saveRDS(SA1.df, file = "RFiles/StudyAreas/SA1-dataset.Rda")


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

# Status: Edited as of 21 Mar 2019
# Intersections were throwing density tests off

################################################################################
# (2) Study Area 2: Jambi - Lubuk Beringin 
# Load the dataframe
SA2 <- readRDS("RFiles/StudyAreas/StudyArea2.Rda")
head(SA2)

# Load the study area polygons
prot <- st_read("Polygons/Study Area 2/HL_Diff_2.shp")
hud <- st_read("Polygons/Study Area 2/HD_Diff_2.shp")
line <- st_read("Polygons/Study Area 2/Intersection_2.shp")

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
plot(st_geometry(line), col = "red", add = TRUE)

# Check if GFC data intersects with study area polygons 
# Create new column `in_HD`
SA2_sf$in_HD <- st_intersects(SA2_sf, line, sparse = FALSE)
SA2_sf$in_HD <- na_if(SA2_sf$in_HD, "TRUE")
sum(is.na(SA2_sf))
SA2_sf <- na.omit(SA2_sf)
sum(is.na((SA2_sf)))

SA2_sf$in_HD <- st_intersects(SA2_sf, hud, sparse = FALSE)
head(SA2_sf)

## CREATE RUNNING VARIABLE: (DISTANCE)

# Convert border to spatial line
line_sp <- as(line, Class = "Spatial")

# Convert border from polygon to spatial line
border_line <- as(line_sp, Class = "SpatialLines")

# Calculate length of border
SpatialLinesLengths(border_line, longlat = TRUE)
# 27.72401
# The border is 28 km long. 

# Checking the plots
plot(st_geometry(SA2_sf), col = "forest green", axes = TRUE) 
plot(st_geometry(prot), border = "black", add = TRUE)
plot(st_geometry(hud), border = "black", add = TRUE)
plot(border_line, col = "red", add = TRUE)

# Calculate distance from border 
# Convert sf to dataframe to insert distrelative column 
SA2.df <- as.data.frame(SA2_sf, xy = TRUE)

# Extract geometry column from sf and convert into matrix 
# with latitude and longitude stored in x and y columns  
geometry <- SA2_sf$geometry
geom <- st_coordinates(geometry)

# Calculate distance and relative distance, create new columns to store the values
dist.SA2 <- dist2Line(geom, border_line)
head(dist.SA2)
SA2.df$distrelative <- ifelse(SA2.df$in_HD==TRUE, dist.SA2[,1], -dist.SA2[,1])
summary(SA2.df$distrelative)
head(SA2.df)

# Save dataframe to drive
saveRDS(SA2.df, file = "RFiles/StudyAreas/SA2-dataset.Rda")


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

# Panel format: 
full2.panel = gather(full2, key = year, value = treecover, -c(geometry, in_HD, distrelative, 
                                                              lossyear1, province, forest_area, 
                                                              forest_area_ENG, study_area))
head(full2.panel)

saveRDS(full2, file = "RFiles/StudyAreas/SA2full.Rda")
saveRDS(full2.panel, file = "RFiles/StudyAreas/SA2fullpanel.Rda")

# Status: Edited as of 21 Mar 2019
# Intersections were throwing density tests off
################################################################################
# (3) Study Area 3: Jambi - Tamonarang
# Load the dataframe
SA3 <- readRDS("RFiles/StudyAreas/StudyArea3.Rda")
head(SA3)

# Load the study area polygons
prot <- st_read("Polygons/Study Area 3/HL_Diff_3.shp")
hud <- st_read("Polygons/Study Area 3/HD_Diff_3.shp")
line <- st_read("Polygons/Study Area 3/Intersection_3.shp")

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
plot(st_geometry(line), col = "red", add = TRUE)

# Check if GFC data intersects with study area polygons 
# Create new column `in_HD`
SA3_sf$in_HD <- st_intersects(SA3_sf, line, sparse = FALSE)
SA3_sf$in_HD <- na_if(SA3_sf$in_HD, "TRUE")
sum(is.na(SA3_sf))
SA3_sf <- na.omit(SA3_sf)
sum(is.na((SA3_sf)))

SA3_sf$in_HD <- st_intersects(SA3_sf, hud, sparse = FALSE)
head(SA3_sf)

## CREATE RUNNING VARIABLE: (DISTANCE)

# Convert border to spatial line
line_sp <- as(line, Class = "Spatial")

# Convert border from polygon to spatial line
border_line <- as(line_sp, Class = "SpatialLines")

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
# Convert sf to dataframe to insert distrelative column 
SA3.df <- as.data.frame(SA3_sf, xy = TRUE)

# Extract geometry column from sf and convert into matrix 
# with latitude and longitude stored in x and y columns  
geometry <- SA3_sf$geometry
geom <- st_coordinates(geometry)

# Calculate distance and relative distance, create new columns to store the values
dist.SA3 <- dist2Line(geom, border_line)
head(dist.SA3)
SA3.df$distrelative <- ifelse(SA3.df$in_HD==TRUE, dist.SA3[,1], -dist.SA3[,1])
summary(SA3.df$distrelative)
head(SA3.df)

# Save dataframe to drive
saveRDS(SA3.df, file = "RFiles/StudyAreas/SA3-dataset.Rda")


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

# Status: Edited as of 21 Mar 2019

################################################################################
# (4) Study Area 4: Sumatra Selantan - Near Kota Pagalaram
# Load the dataframe
SA4 <- readRDS("RFiles/StudyAreas/StudyArea4.Rda")
head(SA4)

# Load the study area polygons
prot <- st_read("Polygons/Study Area 4/HL_Diff_4.shp")
hud <- st_read("Polygons/Study Area 4/HD_Diff_4.shp")
line <- st_read("Polygons/Study Area 4/Intersection_4.shp")

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
plot(st_geometry(line), col = "red", add = TRUE)

# Check if GFC data intersects with study area polygons 
# Create new column `in_HD`
SA4_sf$in_HD <- st_intersects(SA4_sf, line, sparse = FALSE)
SA4_sf$in_HD <- na_if(SA4_sf$in_HD, "TRUE")
sum(is.na(SA4_sf))
SA4_sf <- na.omit(SA4_sf)
sum(is.na((SA4_sf)))

SA4_sf$in_HD <- st_intersects(SA4_sf, hud, sparse = FALSE)
head(SA4_sf)

## CREATE RUNNING VARIABLE: (DISTANCE)

# Convert border to spatial line
line_sp <- as(line, Class = "Spatial")

# Convert border from polygon to spatial line
border_line <- as(line_sp, Class = "SpatialLines")

# Calculate length of border
SpatialLinesLengths(border_line, longlat = TRUE)
# 61.84666
# The border is 62 km long. 

# Checking the plots
plot(st_geometry(SA4_sf), col = "forest green", axes = TRUE) 
plot(st_geometry(prot), border = "black", add = TRUE)
plot(st_geometry(hud), border = "black", add = TRUE)
plot(border_line, col = "red", add = TRUE)

# Calculate distance from border 
# Convert sf to dataframe to insert distrelative column 
SA4.df <- as.data.frame(SA4_sf, xy = TRUE)

# Extract geometry column from sf and convert into matrix 
# with latitude and longitude stored in x and y columns  
geometry <- SA4_sf$geometry
geom <- st_coordinates(geometry)

# Calculate distance and relative distance, create new columns to store the values
dist.SA4 <- dist2Line(geom, border_line)
head(dist.SA4)
SA4.df$distrelative <- ifelse(SA4.df$in_HD==TRUE, dist.SA4[,1], -dist.SA4[,1])
summary(SA4.df$distrelative)
head(SA4.df)

# Save dataframe to drive
saveRDS(SA4.df, file = "RFiles/StudyAreas/SA4-dataset.Rda")


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

# Checking for 1 point 
filter(SA4.panel, id ==1)

# Save wide and panel formats: 
saveRDS(SA4.wide, file = "RFiles/StudyAreas/SA4wide.Rda")
saveRDS(SA4.panel, file = "RFiles/StudyAreas/SA4panel.Rda")

# Combine dataframes and reorder columns
full4 = bind_cols(SA4.df, SA4.wide)
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

# Status: Edited as of 21 Mar 2019

################################################################################
# (5) Study Area 5: W. Kalimantan - K2 Nangaputih 
# Load the dataframe
SA5 <- readRDS("RFiles/StudyAreas/StudyArea5.Rda")
head(SA5)

# Load the study area polygons
prot <- st_read("Polygons/Study Area 5/HL_5_Diff.shp")
hud <- st_read("Polygons/Study Area 5/HD_5_Diff.shp")
line <- st_read("Polygons/Study Area 5/Intersection_5.shp")

# Convert dataframe to sf object to coerce x,y columns into geometry column 
# Changing projection to CRS = 4326 will allow us to plot the land title polygons on top of sf object
SA5_sf <- st_as_sf(SA5, coords = c("x", "y"), crs = 4326)
head(SA5_sf)

## CREATE DUMMY VARIABLE: SFP (SOCIAL)

# Identify if point lies in HD or not HD 
# Plot polygons together to visually inspect data
plot(st_geometry(SA5_sf), col = "forest green", axes = TRUE) 
plot(st_geometry(prot), border = "black", add = TRUE)
plot(st_geometry(hud), border = "black", add = TRUE)
plot(st_geometry(line), col = "red", add = TRUE)

# Check if GFC data intersects with study area polygons 
SA5_sf$in_HD<- st_intersects(SA5_sf, line, sparse = FALSE)
SA5_sf$in_HD <- na_if(SA5_sf$in_HD, "TRUE")
sum(is.na(SA5_sf))
SA5_sf <- na.omit(SA5_sf)
sum(is.na(SA5_sf))

# Create new column `in_HD`
SA5_sf$in_HD <- st_intersects(SA5_sf, hud, sparse = FALSE)
head(SA5_sf)

## CREATE RUNNING VARIABLE: (DISTANCE)

# Convert border to spatial line
line_sp <- as(line, Class = "Spatial")

# Convert border from polygon to spatial line
border_line <- as(line_sp, Class = "SpatialLines")

# Calculate length of border
SpatialLinesLengths(border_line, longlat = TRUE)
# 29.63541
# The border is  30 km long. 

# Checking the plots
plot(st_geometry(SA5_sf), col = "forest green", axes = TRUE) 
plot(st_geometry(prot), border = "black", add = TRUE)
plot(st_geometry(hud), border = "black", add = TRUE)
plot(border_line, col = "red", add = TRUE)

# Calculate distance from border 
# Convert sf to dataframe to insert distrelative column 
SA5.df <- as.data.frame(SA5_sf, xy = TRUE)

# Extract geometry column from sf and convert into matrix 
# with latitude and longitude stored in x and y columns  
geometry <- SA5_sf$geometry
geom <- st_coordinates(geometry)

# Calculate distance and relative distance, create new columns to store the values
dist.SA5 <- dist2Line(geom, border_line)
head(dist.SA5)
SA5.df$distrelative <- ifelse(SA5.df$in_HD==TRUE, dist.SA5[,1], -dist.SA5[,1])
summary(SA5.df$distrelative)
head(SA5.df)

# Save dataframe to drive
saveRDS(SA5.df, file = "RFiles/StudyAreas/SA5-dataset.Rda")

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

# Calculate forest cover in each year (based on lossyear only) for regression plots
# and for robustness checks
SA5.test <- SA5.df

lossdf <- data.frame(id = 1:dim(SA5.df), yr_00 = SA5.test$treecover2000, lossyear = SA5.test$lossyear)
head(lossdf)
dim(lossdf)

lossdf <- lossdf %>% 
  mutate(year=0) %>% 
  expand(nesting(id, lossyear, yr_00), year = 0:17) %>% 
  mutate(loss_event = ifelse(year >= lossyear & lossyear !=0 , 0, yr_00)) %>% 
  mutate(year = paste0("yr_", year)) %>% 
  spread(year, loss_event)

head(lossdf)

lossdf = subset(lossdf, select = -c(yr_0))
head(lossdf)

# Save lossdf as dataframe to drive
saveRDS(lossdf, file = "RFiles/StudyAreas/lossdf-SA5.Rda")
lossdf5 <- readRDS("RFiles/StudyAreas/lossdf-SA5.Rda")

# Transform dataframes into wide and long (panel) formats for regression 
# Wide format: 
# Create tibble extracting geometry and in_HD columns from tamonarang.df dataframe
geo_id = tibble(geometry = SA5.df[,7], in_HD = SA5.df[,8])
head(geo_id)

# Join geo_id and lossdf tibble 
SA5.wide = bind_cols(geo_id, lossdf5)
dim(SA5.wide)
head(SA5.wide)

# Panel format: 
SA5.panel = gather(SA5.wide, key = year, value = treecover, -c(geometry, in_HD, id, lossyear))
head(SA5.panel)
View(SA5.panel)

# Checking for 1 point 
filter(SA5.panel, id ==1)

# Save wide and panel formats: 
saveRDS(SA5.wide, file = "RFiles/StudyAreas/SA5wide.Rda")
saveRDS(SA5.panel, file = "RFiles/StudyAreas/SA5panel.Rda")

# Combine dataframes and reorder columns
full5 = bind_cols(SA5.df, SA5.wide)
full5 <- full5 %>% select(-(10:12))
full5 <- full5 %>% select(-(1:2)) 
full5 <- full5 %>% select("geometry", "in_HD", "distrelative", "lossyear1", 
                          "yr_00", "yr_1", "yr_2", "yr_3", "yr_4", "yr_5", "yr_6", "yr_7", "yr_8",
                          "yr_9", "yr_10", "yr_11", "yr_12", "yr_13", "yr_14", "yr_15", "yr_16", "yr_17",
                          "province", "forest_area", "forest_area_ENG", "study_area")

# Panel format: 
full5.panel = gather(full5, key = year, value = treecover, -c(geometry, in_HD, distrelative, 
                                                              lossyear1, province, forest_area, 
                                                              forest_area_ENG, study_area))
head(full5.panel)

saveRDS(full5, file = "RFiles/StudyAreas/SA5full.Rda")
saveRDS(full5.panel, file = "RFiles/StudyAreas/SA5fullpanel.Rda")

# Status: Done as of 16 Mar 2019

################################################################################
# (6) Study Area Study Area 6: W. Kalimantan - K2 Near HT-0

# Load the dataframe
SA6 <- readRDS("RFiles/StudyAreas/StudyArea6.Rda")
head(SA6)

# Load the study area polygons (difference processed in QGIS)
prot <- st_read("Polygons/Study Area 6/HL_Diff_6.shp")
hud <- st_read("Polygons/Study Area 6/HD_Diff_6.shp")
line <- st_read("Polygons/Study Area 6/Intersection_6.shp")

# Convert dataframe to sf object to coerce x,y columns into geometry column 
# Changing projection to CRS = 4326 will allow us to plot the land title polygons on top of sf object
SA6_sf <- st_as_sf(SA6, coords = c("x", "y"), crs = 4326)
head(SA6_sf)

## CREATE DUMMY VARIABLE: SFP (SOCIAL)

# Identify if point lies in HD or not HD 
# Plot polygons together to visually inspect data
plot(st_geometry(SA6_sf), col = "forest green", axes = TRUE) 
plot(st_geometry(prot), border = "black", add = TRUE)
plot(st_geometry(hud), border = "black", add = TRUE)
plot(st_geometry(line), col = "red", add = TRUE)

# Check if GFC data intersects with study area polygons 
SA6_sf$in_HD<- st_intersects(SA6_sf, line, sparse = FALSE)
SA6_sf$in_HD <- na_if(SA6_sf$in_HD, "TRUE")
sum(is.na(SA6_sf))
SA6_sf <- na.omit(SA6_sf)
sum(is.na(SA6_sf))

# Create new column `in_HD`
SA6_sf$in_HD <- st_intersects(SA6_sf, hud, sparse = FALSE)
head(SA6_sf)

## CREATE RUNNING VARIABLE: (DISTANCE)

# Convert border to spatial line
line_sp <- as(line, Class = "Spatial")

# Convert border from polygon to spatial line
border_line <- as(line_sp, Class = "SpatialLines")

# Calculate length of border
SpatialLinesLengths(border_line, longlat = TRUE)
# 24.93478
# The border is 25 km long. 

# Checking the plots
plot(st_geometry(SA6_sf), col = "forest green", axes = TRUE) 
plot(st_geometry(prot), border = "black", add = TRUE)
plot(st_geometry(hud), border = "black", add = TRUE)
plot(border_line, col = "red", add = TRUE)

# Calculate distance from border 
# Convert sf to dataframe to insert distrelative column 
SA6.df <- as.data.frame(SA6_sf, xy = TRUE)

# Extract geometry column from sf and convert into matrix 
# with latitude and longitude stored in x and y columns  
geometry <- SA6_sf$geometry
geom <- st_coordinates(geometry)

# Calculate distance and relative distance, create new columns to store the values
dist.SA6 <- dist2Line(geom, border_line)
head(dist.SA6)
SA6.df$distrelative <- ifelse(SA6.df$in_HD==TRUE, dist.SA6[,1], -dist.SA6[,1])
summary(SA6.df$distrelative)
head(SA6.df)

# Save dataframe to drive
saveRDS(SA6.df, file = "RFiles/StudyAreas/SA6-dataset.Rda")

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

# Calculate forest cover in each year (based on lossyear only) for regression plots
# and for robustness checks
SA6.test <- SA6.df

lossdf <- data.frame(id = 1:dim(SA6.df), yr_00 = SA6.test$treecover2000, lossyear = SA6.test$lossyear)
head(lossdf)
dim(lossdf)

lossdf <- lossdf %>% 
  mutate(year=0) %>% 
  expand(nesting(id, lossyear, yr_00), year = 0:17) %>% 
  mutate(loss_event = ifelse(year >= lossyear & lossyear !=0 , 0, yr_00)) %>% 
  mutate(year = paste0("yr_", year)) %>% 
  spread(year, loss_event)

head(lossdf)

lossdf = subset(lossdf, select = -c(yr_0))
head(lossdf)

# Save lossdf as dataframe to drive
saveRDS(lossdf, file = "RFiles/StudyAreas/lossdf-SA6.Rda")
lossdf6 <- readRDS("RFiles/StudyAreas/lossdf-SA6.Rda")

# Transform dataframes into wide and long (panel) formats for regression 
# Wide format: 
# Create tibble extracting geometry and in_HD columns from tamonarang.df dataframe
geo_id = tibble(geometry = SA6.df[,7], in_HD = SA6.df[,8])
head(geo_id)

# Join geo_id and lossdf tibble 
SA6.wide = bind_cols(geo_id, lossdf6)
dim(SA6.wide)
head(SA6.wide)

# Panel format: 
SA6.panel = gather(SA6.wide, key = year, value = treecover, -c(geometry, in_HD, id, lossyear))
head(SA6.panel)
View(SA6.panel)

# Checking for 1 point 
filter(SA6.panel, id ==1)

# Save wide and panel formats: 
saveRDS(SA6.wide, file = "RFiles/StudyAreas/SA6wide.Rda")
saveRDS(SA6.panel, file = "RFiles/StudyAreas/SA6panel.Rda")

# Combine dataframes and reorder columns
full6 = bind_cols(SA6.df, SA6.wide)
full6 <- full6 %>% select(-(10:12))
full6 <- full6 %>% select(-(1:2)) 
full6 <- full6 %>% select("geometry", "in_HD", "distrelative", "lossyear1", 
                          "yr_00", "yr_1", "yr_2", "yr_3", "yr_4", "yr_5", "yr_6", "yr_7", "yr_8",
                          "yr_9", "yr_10", "yr_11", "yr_12", "yr_13", "yr_14", "yr_15", "yr_16", "yr_17",
                          "province", "forest_area", "forest_area_ENG", "study_area")

head(full6)

# Panel format: 
full6.panel = gather(full6, key = year, value = treecover, -c(geometry, in_HD, distrelative, 
                                                              lossyear1, province, forest_area, 
                                                              forest_area_ENG, study_area))
head(full6.panel)

saveRDS(full6, file = "RFiles/StudyAreas/SA6full.Rda")
saveRDS(full6.panel, file = "RFiles/StudyAreas/SA6fullpanel.Rda")

# Status: Done as of 16 Mar 2019

################################################################################
# (7) Study Area 7: N. Kalimantan - EK5 HT-21 

# Load the dataframe
SA7 <- readRDS("RFiles/StudyAreas/StudyArea7.Rda")
head(SA7)

# Load the study area polygons (difference processed in QGIS)
prot <- st_read("Polygons/Study Area 7/HL_Diff_7.shp")
hkm <- st_read("Polygons/Study Area 7/HKm_Diff_7.shp")
line <- st_read("Polygons/Study Area 7/Intersection_7.shp")

# Convert dataframe to sf object to coerce x,y columns into geometry column 
# Changing projection to CRS = 4326 will allow us to plot the land title polygons on top of sf object
SA7_sf <- st_as_sf(SA7, coords = c("x", "y"), crs = 4326)
head(SA7_sf)

## CREATE DUMMY VARIABLE: SFP (SOCIAL)

# Identify if point lies in HD or not HD 
# Plot polygons together to visually inspect data
plot(st_geometry(SA7_sf), col = "forest green", axes = TRUE) 
plot(st_geometry(prot), border = "black", add = TRUE)
plot(st_geometry(hkm), border = "black", add = TRUE)
plot(st_geometry(line), col = "red", add = TRUE)

# Check if GFC data intersects with study area polygons 
# Excluding area within border 
SA7_sf$in_HD<- st_intersects(SA7_sf, line, sparse = FALSE)
SA7_sf$in_HD <- na_if(SA7_sf$in_HD, "TRUE")
sum(is.na(SA7_sf))
SA7_sf <- na.omit(SA7_sf)
sum(is.na(SA7_sf))

# Create new column `in_HD`
SA7_sf$in_HD <- st_intersects(SA7_sf, hkm, sparse = FALSE) # Intersection with HKm - to change column name later
head(SA7_sf)

## CREATE RUNNING VARIABLE: (DISTANCE)

# Convert border to spatial line
line_sp <- as(line, Class = "Spatial")

# Convert border from polygon to spatial line
border_line <- as(line_sp, Class = "SpatialLines")

# Calculate length of border
SpatialLinesLengths(border_line, longlat = TRUE)
# 20.51986
# The border is 21 km long. 

# Checking the plots
plot(st_geometry(SA7_sf), col = "forest green", axes = TRUE) 
plot(st_geometry(prot), border = "black", add = TRUE)
plot(st_geometry(hkm), border = "black", add = TRUE)
plot(border_line, col = "red", add = TRUE)

# Calculate distance from border 
# Convert sf to dataframe to insert distrelative column 
SA7.df <- as.data.frame(SA7_sf, xy = TRUE)

# Extract geometry column from sf and convert into matrix 
# with latitude and longitude stored in x and y columns  
geometry <- SA7_sf$geometry
geom <- st_coordinates(geometry)

# Calculate distance and relative distance, create new columns to store the values
dist.SA7 <- dist2Line(geom, border_line)
head(dist.SA7)
SA7.df$distrelative <- ifelse(SA7.df$in_HD==TRUE, dist.SA7[,1], -dist.SA7[,1])
summary(SA7.df$distrelative)
head(SA7.df)

# Save dataframe to drive
saveRDS(SA7.df, file = "RFiles/StudyAreas/SA7-dataset.Rda")

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

# Calculate forest cover in each year (based on lossyear only) for regression plots
# and for robustness checks
SA7.test <- SA7.df

lossdf <- data.frame(id = 1:dim(SA7.df), yr_00 = SA7.test$treecover2000, lossyear = SA7.test$lossyear)
head(lossdf)
dim(lossdf)

lossdf <- lossdf %>% 
  mutate(year=0) %>% 
  expand(nesting(id, lossyear, yr_00), year = 0:17) %>% 
  mutate(loss_event = ifelse(year >= lossyear & lossyear !=0 , 0, yr_00)) %>% 
  mutate(year = paste0("yr_", year)) %>% 
  spread(year, loss_event)

head(lossdf)

lossdf = subset(lossdf, select = -c(yr_0))
head(lossdf)

# Save lossdf as dataframe to drive
saveRDS(lossdf, file = "RFiles/StudyAreas/lossdf-SA7.Rda")
lossdf7 <- readRDS("RFiles/StudyAreas/lossdf-SA7.Rda")

# Transform dataframes into wide and long (panel) formats for regression 
# Wide format: 
# Create tibble extracting geometry and in_HD columns from dataframe
geo_id = tibble(geometry = SA7.df[,7], in_HD = SA7.df[,8])
head(geo_id)

# Join geo_id and lossdf tibble 
SA7.wide = bind_cols(geo_id, lossdf7)
dim(SA7.wide)
head(SA7.wide)

# Panel format: 
SA7.panel = gather(SA7.wide, key = year, value = treecover, -c(geometry, in_HD, id, lossyear))
head(SA7.panel)
View(SA7.panel)

# Checking for 1 point 
filter(SA7.panel, id ==1)

# Save wide and panel formats: 
saveRDS(SA7.wide, file = "RFiles/StudyAreas/SA7wide.Rda")
saveRDS(SA7.panel, file = "RFiles/StudyAreas/SA7panel.Rda")

# Combine dataframes and reorder columns
full7 = bind_cols(SA7.df, SA7.wide)
full7 <- full7 %>% select(-(10:12))
full7 <- full7 %>% select(-(1:2)) 
full7 <- full7 %>% select("geometry", "in_HD", "distrelative", "lossyear1", 
                          "yr_00", "yr_1", "yr_2", "yr_3", "yr_4", "yr_5", "yr_6", "yr_7", "yr_8",
                          "yr_9", "yr_10", "yr_11", "yr_12", "yr_13", "yr_14", "yr_15", "yr_16", "yr_17",
                          "province", "forest_area", "forest_area_ENG", "study_area")

head(full7)

# Panel format: 
full7.panel = gather(full7, key = year, value = treecover, -c(geometry, in_HD, distrelative, 
                                                              lossyear1, province, forest_area, 
                                                              forest_area_ENG, study_area))
head(full7.panel)

saveRDS(full7, file = "RFiles/StudyAreas/SA7full.Rda")
saveRDS(full7.panel, file = "RFiles/StudyAreas/SA7fullpanel.Rda")

# Status: Done as of 16 Mar 2019

################################################################################
# (8) Study Area 8: Lampung - L Unit IX-KPHL

# Load the dataframe
SA8 <- readRDS("RFiles/StudyAreas/StudyArea8.Rda")
head(SA8)

# Load the study area polygons (difference processed in QGIS)
prot <- st_read("Polygons/Study Area 8/HL_Diff_8.shp")
hkm <- st_read("Polygons/Study Area 8/HKm_Diff_8.shp")
line <- st_read("Polygons/Study Area 8/Intersection_8.shp")

# Convert dataframe to sf object to coerce x,y columns into geometry column 
# Changing projection to CRS = 4326 will allow us to plot the land title polygons on top of sf object
SA8_sf <- st_as_sf(SA8, coords = c("x", "y"), crs = 4326)
head(SA8_sf)

## CREATE DUMMY VARIABLE: SFP (SOCIAL)

# Identify if point lies in HD or not HD 
# Plot polygons together to visually inspect data
plot(st_geometry(SA8_sf), col = "forest green", axes = TRUE) 
plot(st_geometry(prot), border = "black", add = TRUE)
plot(st_geometry(hkm), border = "black", add = TRUE)
plot(st_geometry(line), col = "red", add = TRUE)

# Check if GFC data intersects with study area polygons 
# Excluding area within border 
SA8_sf$in_HD<- st_intersects(SA8_sf, line, sparse = FALSE)
SA8_sf$in_HD <- na_if(SA8_sf$in_HD, "TRUE")
sum(is.na(SA8_sf))
SA8_sf <- na.omit(SA8_sf)
sum(is.na(SA8_sf))

# Create new column `in_HD`
SA8_sf$in_HD <- st_intersects(SA8_sf, hkm, sparse = FALSE) # Intersection with HKm - to change column name later
head(SA8_sf)

## CREATE RUNNING VARIABLE: (DISTANCE)

# Convert border to spatial line
line_sp <- as(line, Class = "Spatial")

# Convert border from polygon to spatial line
border_line <- as(line_sp, Class = "SpatialLines")

# Calculate length of border
SpatialLinesLengths(border_line, longlat = TRUE)
# 41.54838
# The border is 42 km long. 

# Checking the plots
plot(st_geometry(SA8_sf), col = "forest green", axes = TRUE) 
plot(st_geometry(prot), border = "black", add = TRUE)
plot(st_geometry(hkm), border = "black", add = TRUE)
plot(border_line, col = "red", add = TRUE)

# Calculate distance from border 
# Convert sf to dataframe to insert distrelative column 
SA8.df <- as.data.frame(SA8_sf, xy = TRUE)

# Extract geometry column from sf and convert into matrix 
# with latitude and longitude stored in x and y columns  
geometry <- SA8_sf$geometry
geom <- st_coordinates(geometry)

# Calculate distance and relative distance, create new columns to store the values
dist.SA8 <- dist2Line(geom, border_line)
head(dist.SA8)
SA8.df$distrelative <- ifelse(SA8.df$in_HD==TRUE, dist.SA8[,1], -dist.SA8[,1])
summary(SA8.df$distrelative)
head(SA8.df)

# Save dataframe to drive
saveRDS(SA8.df, file = "RFiles/StudyAreas/SA8-dataset.Rda")

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

# Calculate forest cover in each year (based on lossyear only) for regression plots
# and for robustness checks
SA8.test <- SA8.df

lossdf <- data.frame(id = 1:dim(SA8.df), yr_00 = SA8.test$treecover2000, lossyear = SA8.test$lossyear)
head(lossdf)
dim(lossdf)

lossdf <- lossdf %>% 
  mutate(year=0) %>% 
  expand(nesting(id, lossyear, yr_00), year = 0:17) %>% 
  mutate(loss_event = ifelse(year >= lossyear & lossyear !=0 , 0, yr_00)) %>% 
  mutate(year = paste0("yr_", year)) %>% 
  spread(year, loss_event)

head(lossdf)

lossdf = subset(lossdf, select = -c(yr_0))
head(lossdf)

# Save lossdf as dataframe to drive
saveRDS(lossdf, file = "RFiles/StudyAreas/lossdf-SA8.Rda")
lossdf8 <- readRDS("RFiles/StudyAreas/lossdf-SA8.Rda")

# Transform dataframes into wide and long (panel) formats for regression 
# Wide format: 
# Create tibble extracting geometry and in_HD columns from dataframe
geo_id = tibble(geometry = SA8.df[,7], in_HD = SA8.df[,8])
head(geo_id)

# Join geo_id and lossdf tibble 
SA8.wide = bind_cols(geo_id, lossdf8)
dim(SA8.wide)
head(SA8.wide)

# Panel format: 
SA8.panel = gather(SA8.wide, key = year, value = treecover, -c(geometry, in_HD, id, lossyear))
head(SA8.panel)
View(SA8.panel)

# Checking for 1 point 
filter(SA8.panel, id ==1)

# Save wide and panel formats: 
saveRDS(SA8.wide, file = "RFiles/StudyAreas/SA8wide.Rda")
saveRDS(SA8.panel, file = "RFiles/StudyAreas/SA8panel.Rda")

# Combine dataframes and reorder columns
full8 = bind_cols(SA8.df, SA8.wide)
full8 <- full8 %>% select(-(10:12))
full8 <- full8 %>% select(-(1:2)) 
full8 <- full8 %>% select("geometry", "in_HD", "distrelative", "lossyear1", 
                          "yr_00", "yr_1", "yr_2", "yr_3", "yr_4", "yr_5", "yr_6", "yr_7", "yr_8",
                          "yr_9", "yr_10", "yr_11", "yr_12", "yr_13", "yr_14", "yr_15", "yr_16", "yr_17",
                          "province", "forest_area", "forest_area_ENG", "study_area")

head(full8)

# Panel format: 
full8.panel = gather(full8, key = year, value = treecover, -c(geometry, in_HD, distrelative, 
                                                              lossyear1, province, forest_area, 
                                                              forest_area_ENG, study_area))
head(full8.panel)

saveRDS(full8, file = "RFiles/StudyAreas/SA8full.Rda")
saveRDS(full8.panel, file = "RFiles/StudyAreas/SA8fullpanel.Rda")

# Status: Done as of 16 Mar 2019

################################################################################
# (9) Study Area 9: Aceh - N KAB. PIDIE

# Load the dataframe
SA9 <- readRDS("RFiles/StudyAreas/StudyArea9.Rda")
head(SA9)

# Load the study area polygons (difference processed in QGIS)
prot <- st_read("Polygons/Study Area 9/HL_Diff_9.shp")
hkm <- st_read("Polygons/Study Area 9/HKm_Diff_9.shp")
line <- st_read("Polygons/Study Area 9/Intersection_9.shp")

# Convert dataframe to sf object to coerce x,y columns into geometry column 
# Changing projection to CRS = 4326 will allow us to plot the land title polygons on top of sf object
SA9_sf <- st_as_sf(SA9, coords = c("x", "y"), crs = 4326)
head(SA9_sf)

## CREATE DUMMY VARIABLE: SFP (SOCIAL)

# Identify if point lies in HD or not HD 
# Plot polygons together to visually inspect data
plot(st_geometry(SA9_sf), col = "forest green", axes = TRUE) 
plot(st_geometry(prot), border = "black", add = TRUE)
plot(st_geometry(hkm), border = "black", add = TRUE)
plot(st_geometry(line), col = "red", add = TRUE)

# Check if GFC data intersects with study area polygons 
# Excluding area within border 
SA9_sf$in_HD<- st_intersects(SA9_sf, line, sparse = FALSE)
SA9_sf$in_HD <- na_if(SA9_sf$in_HD, "TRUE")
sum(is.na(SA9_sf))
SA9_sf <- na.omit(SA9_sf)
sum(is.na(SA9_sf))

# Create new column `in_HD`
SA9_sf$in_HD <- st_intersects(SA9_sf, hkm, sparse = FALSE) # Intersection with HKm - to change column name later
head(SA9_sf)

## CREATE RUNNING VARIABLE: (DISTANCE)

# Convert border to spatial line
line_sp <- as(line, Class = "Spatial")

# Convert border from polygon to spatial line
border_line <- as(line_sp, Class = "SpatialLines")

# Calculate length of border
SpatialLinesLengths(border_line, longlat = TRUE)
# 23.3601
# The border is 23 km long. 

# Checking the plots
plot(st_geometry(SA9_sf), col = "forest green", axes = TRUE) 
plot(st_geometry(prot), border = "black", add = TRUE)
plot(st_geometry(hkm), border = "black", add = TRUE)
plot(border_line, col = "red", add = TRUE)

# Calculate distance from border 
# Convert sf to dataframe to insert distrelative column 
SA9.df <- as.data.frame(SA9_sf, xy = TRUE)

# Extract geometry column from sf and convert into matrix 
# with latitude and longitude stored in x and y columns  
geometry <- SA9_sf$geometry
geom <- st_coordinates(geometry)

# Calculate distance and relative distance, create new columns to store the values
dist.SA9 <- dist2Line(geom, border_line)
head(dist.SA9)
SA9.df$distrelative <- ifelse(SA9.df$in_HD==TRUE, dist.SA9[,1], -dist.SA9[,1])
summary(SA9.df$distrelative)
head(SA9.df)

# Save dataframe to drive
saveRDS(SA9.df, file = "RFiles/StudyAreas/SA9-dataset.Rda")

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

# Calculate forest cover in each year (based on lossyear only) for regression plots
# and for robustness checks
SA9.test <- SA9.df

lossdf <- data.frame(id = 1:dim(SA9.df), yr_00 = SA9.test$treecover2000, lossyear = SA9.test$lossyear)
head(lossdf)
dim(lossdf)

lossdf <- lossdf %>% 
  mutate(year=0) %>% 
  expand(nesting(id, lossyear, yr_00), year = 0:17) %>% 
  mutate(loss_event = ifelse(year >= lossyear & lossyear !=0 , 0, yr_00)) %>% 
  mutate(year = paste0("yr_", year)) %>% 
  spread(year, loss_event)

head(lossdf)

lossdf = subset(lossdf, select = -c(yr_0))
head(lossdf)

# Save lossdf as dataframe to drive
saveRDS(lossdf, file = "RFiles/StudyAreas/lossdf-SA9.Rda")
lossdf9 <- readRDS("RFiles/StudyAreas/lossdf-SA9.Rda")

# Transform dataframes into wide and long (panel) formats for regression 
# Wide format: 
# Create tibble extracting geometry and in_HD columns from dataframe
geo_id = tibble(geometry = SA9.df[,7], in_HD = SA9.df[,8])
head(geo_id)

# Join geo_id and lossdf tibble 
SA9.wide = bind_cols(geo_id, lossdf9)
dim(SA9.wide)
head(SA9.wide)

# Panel format: 
SA9.panel = gather(SA9.wide, key = year, value = treecover, -c(geometry, in_HD, id, lossyear))
head(SA9.panel)
View(SA9.panel)

# Checking for 1 point 
filter(SA9.panel, id ==1)

# Save wide and panel formats: 
saveRDS(SA9.wide, file = "RFiles/StudyAreas/SA9wide.Rda")
saveRDS(SA9.panel, file = "RFiles/StudyAreas/SA9panel.Rda")

# Combine dataframes and reorder columns
full9 = bind_cols(SA9.df, SA9.wide)
full9 <- full9 %>% select(-(10:12))
full9 <- full9 %>% select(-(1:2)) 
full9 <- full9 %>% select("geometry", "in_HD", "distrelative", "lossyear1", 
                          "yr_00", "yr_1", "yr_2", "yr_3", "yr_4", "yr_5", "yr_6", "yr_7", "yr_8",
                          "yr_9", "yr_10", "yr_11", "yr_12", "yr_13", "yr_14", "yr_15", "yr_16", "yr_17",
                          "province", "forest_area", "forest_area_ENG", "study_area")

head(full9)

# Panel format: 
full9.panel = gather(full9, key = year, value = treecover, -c(geometry, in_HD, distrelative, 
                                                              lossyear1, province, forest_area, 
                                                              forest_area_ENG, study_area))
head(full9.panel)

saveRDS(full9, file = "RFiles/StudyAreas/SA9full.Rda")
saveRDS(full9.panel, file = "RFiles/StudyAreas/SA9fullpanel.Rda")

# Status: Done as of 16 Mar 2019

################################################################################
# (10) Study Area 10: W. Sulawesi - WS UNIT VIII - KPHL

# Load the dataframe
SA10 <- readRDS("RFiles/StudyAreas/StudyArea10.Rda")
head(SA10)

# Load the study area polygons (difference processed in QGIS)
prot <- st_read("Polygons/Study Area 10/HL_Diff_10.shp")
hkm <- st_read("Polygons/Study Area 10/HKm_Diff_10.shp")
line <- st_read("Polygons/Study Area 10/Intersection_10.shp")

# Convert dataframe to sf object to coerce x,y columns into geometry column 
# Changing projection to CRS = 4326 will allow us to plot the land title polygons on top of sf object
SA10_sf <- st_as_sf(SA10, coords = c("x", "y"), crs = 4326)
head(SA10_sf)

## CREATE DUMMY VARIABLE: SFP (SOCIAL)

# Identify if point lies in HD or not HD 
# Plot polygons together to visually inspect data
plot(st_geometry(SA10_sf), col = "forest green", axes = TRUE) 
plot(st_geometry(prot), border = "black", add = TRUE)
plot(st_geometry(hkm), border = "black", add = TRUE)
plot(st_geometry(line), col = "red", add = TRUE)

# Check if GFC data intersects with study area polygons 
# Excluding area within border 
SA10_sf$in_HD<- st_intersects(SA10_sf, line, sparse = FALSE)
SA10_sf$in_HD <- na_if(SA10_sf$in_HD, "TRUE")
sum(is.na(SA10_sf))
SA10_sf <- na.omit(SA10_sf)
sum(is.na(SA10_sf))

# Create new column `in_HD`
SA10_sf$in_HD <- st_intersects(SA10_sf, hkm, sparse = FALSE) # Intersection with HKm - to change column name later
head(SA10_sf)

## CREATE RUNNING VARIABLE: (DISTANCE)

# Convert border to spatial line
line_sp <- as(line, Class = "Spatial")

# Convert border from polygon to spatial line
border_line <- as(line_sp, Class = "SpatialLines")

# Calculate length of border
SpatialLinesLengths(border_line, longlat = TRUE)
# 12.80509
# The border is 13 km long. 

# Checking the plots
plot(st_geometry(SA10_sf), col = "forest green", axes = TRUE) 
plot(st_geometry(prot), border = "black", add = TRUE)
plot(st_geometry(hkm), border = "black", add = TRUE)
plot(border_line, col = "red", add = TRUE)

# Calculate distance from border 
# Convert sf to dataframe to insert distrelative column 
SA10.df <- as.data.frame(SA10_sf, xy = TRUE)

# Extract geometry column from sf and convert into matrix 
# with latitude and longitude stored in x and y columns  
geometry <- SA10_sf$geometry
geom <- st_coordinates(geometry)

# Calculate distance and relative distance, create new columns to store the values
dist.SA10 <- dist2Line(geom, border_line)
head(dist.SA10)
SA10.df$distrelative <- ifelse(SA10.df$in_HD==TRUE, dist.SA10[,1], -dist.SA10[,1])
summary(SA10.df$distrelative)
head(SA10.df)

# Save dataframe to drive
saveRDS(SA10.df, file = "RFiles/StudyAreas/SA10-dataset.Rda")

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

# Calculate forest cover in each year (based on lossyear only) for regression plots
# and for robustness checks
SA10.test <- SA10.df

lossdf <- data.frame(id = 1:dim(SA10.df), yr_00 = SA10.test$treecover2000, lossyear = SA10.test$lossyear)
head(lossdf)
dim(lossdf)

lossdf <- lossdf %>% 
  mutate(year=0) %>% 
  expand(nesting(id, lossyear, yr_00), year = 0:17) %>% 
  mutate(loss_event = ifelse(year >= lossyear & lossyear !=0 , 0, yr_00)) %>% 
  mutate(year = paste0("yr_", year)) %>% 
  spread(year, loss_event)

head(lossdf)

lossdf = subset(lossdf, select = -c(yr_0))
head(lossdf)

# Save lossdf as dataframe to drive
saveRDS(lossdf, file = "RFiles/StudyAreas/lossdf-SA10.Rda")
lossdf10 <- readRDS("RFiles/StudyAreas/lossdf-SA10.Rda")

# Transform dataframes into wide and long (panel) formats for regression 
# Wide format: 
# Create tibble extracting geometry and in_HD columns from dataframe
geo_id = tibble(geometry = SA10.df[,7], in_HD = SA10.df[,8])
head(geo_id)

# Join geo_id and lossdf tibble 
SA10.wide = bind_cols(geo_id, lossdf10)
dim(SA10.wide)
head(SA10.wide)

# Panel format: 
SA10.panel = gather(SA10.wide, key = year, value = treecover, -c(geometry, in_HD, id, lossyear))
head(SA10.panel)
View(SA10.panel)

# Checking for 1 point 
filter(SA10.panel, id ==1)

# Save wide and panel formats: 
saveRDS(SA10.wide, file = "RFiles/StudyAreas/SA10wide.Rda")
saveRDS(SA10.panel, file = "RFiles/StudyAreas/SA10panel.Rda")

# Combine dataframes and reorder columns
full10 = bind_cols(SA10.df, SA10.wide)
full10 <- full10 %>% select(-(10:12))
full10 <- full10 %>% select(-(1:2)) 
full10 <- full10 %>% select("geometry", "in_HD", "distrelative", "lossyear1", 
                            "yr_00", "yr_1", "yr_2", "yr_3", "yr_4", "yr_5", "yr_6", "yr_7", "yr_8",
                            "yr_9", "yr_10", "yr_11", "yr_12", "yr_13", "yr_14", "yr_15", "yr_16", "yr_17",
                            "province", "forest_area", "forest_area_ENG", "study_area")

head(full10)

# Panel format: 
full10.panel = gather(full10, key = year, value = treecover, -c(geometry, in_HD, distrelative, 
                                                                lossyear1, province, forest_area, 
                                                                forest_area_ENG, study_area))
head(full10.panel)

saveRDS(full10, file = "RFiles/StudyAreas/SA10full.Rda")
saveRDS(full10.panel, file = "RFiles/StudyAreas/SA10fullpanel.Rda")

# Status: Done as of 16 Mar 2019

################################################################################
# (11) Study Area 11: Riau: R3 Top-left corner

# Load the dataframe
SA11 <- readRDS("RFiles/StudyAreas/StudyArea11.Rda")
head(SA11)

# Load the study area polygons (difference processed in QGIS)
prot <- st_read("Polygons/Study Area 11/HL_Diff_11.shp")
hud <- st_read("Polygons/Study Area 11/HD_Diff_11.shp")
line <- st_read("Polygons/Study Area 11/Intersection_11.shp")

# Convert dataframe to sf object to coerce x,y columns into geometry column 
# Changing projection to CRS = 4326 will allow us to plot the land title polygons on top of sf object
SA11_sf <- st_as_sf(SA11, coords = c("x", "y"), crs = 4326)
head(SA11_sf)

## CREATE DUMMY VARIABLE: SFP (SOCIAL)

# Identify if point lies in HD or not HD 
# Plot polygons together to visually inspect data
plot(st_geometry(SA11_sf), col = "forest green", axes = TRUE) 
plot(st_geometry(prot), border = "black", add = TRUE)
plot(st_geometry(hud), border = "black", add = TRUE)
plot(st_geometry(line), col = "red", add = TRUE)

# Check if GFC data intersects with study area polygons 
# Excluding area within border 
SA11_sf$in_HD<- st_intersects(SA11_sf, line, sparse = FALSE)
SA11_sf$in_HD <- na_if(SA11_sf$in_HD, "TRUE")
sum(is.na(SA11_sf))
SA11_sf <- na.omit(SA11_sf)
sum(is.na(SA11_sf))

# Create new column `in_HD`
SA11_sf$in_HD <- st_intersects(SA11_sf, hud, sparse = FALSE) 
head(SA11_sf)

## CREATE RUNNING VARIABLE: (DISTANCE)

# Convert border to spatial line
line_sp <- as(line, Class = "Spatial")

# Convert border from polygon to spatial line
border_line <- as(line_sp, Class = "SpatialLines")

# Calculate length of border
SpatialLinesLengths(border_line, longlat = TRUE)
# 6.331865
# The border is 6 km long. 

# Checking the plots
plot(st_geometry(SA11_sf), col = "forest green", axes = TRUE) 
plot(st_geometry(prot), border = "black", add = TRUE)
plot(st_geometry(hud), border = "black", add = TRUE)
plot(border_line, col = "red", add = TRUE)

# Calculate distance from border 
# Convert sf to dataframe to insert distrelative column 
SA11.df <- as.data.frame(SA11_sf, xy = TRUE)

# Extract geometry column from sf and convert into matrix 
# with latitude and longitude stored in x and y columns  
geometry <- SA11_sf$geometry
geom <- st_coordinates(geometry)

# Calculate distance and relative distance, create new columns to store the values
dist.SA11 <- dist2Line(geom, border_line)
head(dist.SA11)
SA11.df$distrelative <- ifelse(SA11.df$in_HD==TRUE, dist.SA11[,1], -dist.SA11[,1])
summary(SA11.df$distrelative)
head(SA11.df)

# Save dataframe to drive
saveRDS(SA11.df, file = "RFiles/StudyAreas/SA11-dataset.Rda")

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

# Calculate forest cover in each year (based on lossyear only) for regression plots
# and for robustness checks
SA11.test <- SA11.df

lossdf <- data.frame(id = 1:dim(SA11.df), yr_00 = SA11.test$treecover2000, lossyear = SA11.test$lossyear)
head(lossdf)
dim(lossdf)

lossdf <- lossdf %>% 
  mutate(year=0) %>% 
  expand(nesting(id, lossyear, yr_00), year = 0:17) %>% 
  mutate(loss_event = ifelse(year >= lossyear & lossyear !=0 , 0, yr_00)) %>% 
  mutate(year = paste0("yr_", year)) %>% 
  spread(year, loss_event)

head(lossdf)

lossdf = subset(lossdf, select = -c(yr_0))
head(lossdf)

# Save lossdf as dataframe to drive
saveRDS(lossdf, file = "RFiles/StudyAreas/lossdf-SA11.Rda")
lossdf11 <- readRDS("RFiles/StudyAreas/lossdf-SA11.Rda")

# Transform dataframes into wide and long (panel) formats for regression 
# Wide format: 
# Create tibble extracting geometry and in_HD columns from dataframe
geo_id = tibble(geometry = SA11.df[,7], in_HD = SA11.df[,8])
head(geo_id)

# Join geo_id and lossdf tibble 
SA11.wide = bind_cols(geo_id, lossdf11)
dim(SA11.wide)
head(SA11.wide)

# Panel format: 
SA11.panel = gather(SA11.wide, key = year, value = treecover, -c(geometry, in_HD, id, lossyear))
head(SA11.panel)
View(SA11.panel)

# Checking for 1 point 
filter(SA11.panel, id ==1)

# Save wide and panel formats: 
saveRDS(SA11.wide, file = "RFiles/StudyAreas/SA11wide.Rda")
saveRDS(SA11.panel, file = "RFiles/StudyAreas/SA11panel.Rda")

# Combine dataframes and reorder columns
full11 = bind_cols(SA11.df, SA11.wide)
full11 <- full11 %>% select(-(10:12))
full11 <- full11 %>% select(-(1:2)) 
full11 <- full11 %>% select("geometry", "in_HD", "distrelative", "lossyear1", 
                            "yr_00", "yr_1", "yr_2", "yr_3", "yr_4", "yr_5", "yr_6", "yr_7", "yr_8",
                            "yr_9", "yr_10", "yr_11", "yr_12", "yr_13", "yr_14", "yr_15", "yr_16", "yr_17",
                            "province", "forest_area", "forest_area_ENG", "study_area")

head(full11)

# Panel format: 
full11.panel = gather(full11, key = year, value = treecover, -c(geometry, in_HD, distrelative, 
                                                                lossyear1, province, forest_area, 
                                                                forest_area_ENG, study_area))
head(full11.panel)

saveRDS(full11, file = "RFiles/StudyAreas/SA11full.Rda")
saveRDS(full11.panel, file = "RFiles/StudyAreas/SA11fullpanel.Rda")

# Status: Done as of 16 Mar 2019

##############################################################################################
# Pooling of datasets - HD/HKm on HL
# Study Areas 1 to 11
SA1.df <- readRDS("RFiles/StudyAreas/SA1full.Rda")
SA2.df <- readRDS("RFiles/StudyAreas/SA2full.Rda")
SA3.df <- readRDS("RFiles/StudyAreas/SA3full.Rda")
SA4.df <- readRDS("RFiles/StudyAreas/SA4full.Rda")
SA5.df <- readRDS("RFiles/StudyAreas/SA5full.Rda")
SA6.df <- readRDS("RFiles/StudyAreas/SA6full.Rda")
SA7.df <- readRDS("RFiles/StudyAreas/SA7full.Rda")
SA8.df <- readRDS("RFiles/StudyAreas/SA8full.Rda")
SA9.df <- readRDS("RFiles/StudyAreas/SA9full.Rda")
SA10.df <- readRDS("RFiles/StudyAreas/SA10full.Rda")
SA11.df <- readRDS("RFiles/StudyAreas/SA11full.Rda")

# Convert Study Areas into numerics
SA1.df <- SA1.df %>% dplyr::mutate(study_area = dplyr::recode(study_area, "Study Area 1" = 1))
SA1.df <- SA1.df %>% dplyr::mutate(SFPID = ifelse(in_HD==TRUE, "1_Village", "1_State"))
head(SA1.df)

SA2.df <- SA2.df %>% dplyr::mutate(study_area = dplyr::recode(study_area, "Study Area 2" = 2))
SA2.df <- SA2.df %>% dplyr::mutate(SFPID = ifelse(in_HD==TRUE, "2_Village", "2_State"))
head(SA2.df)

SA3.df <- SA3.df %>% dplyr::mutate(study_area = dplyr::recode(study_area, "Study Area 3" = 3))
SA3.df <- SA3.df %>% dplyr::mutate(SFPID = ifelse(in_HD==TRUE, "3_Village", "3_State"))
head(SA3.df)

SA4.df <- SA4.df %>% dplyr::mutate(study_area = dplyr::recode(study_area, "Study Area 4" = 4))
SA4.df <- SA4.df %>% dplyr::mutate(SFPID = ifelse(in_HD==TRUE, "4_Village", "4_State"))
head(SA4.df)

SA5.df <- SA5.df %>% dplyr::mutate(study_area = dplyr::recode(study_area, "Study Area 5" = 5))
SA5.df <- SA5.df %>% dplyr::mutate(SFPID = ifelse(in_HD==TRUE, "5_Village", "5_State"))
head(SA5.df)

SA6.df <- SA6.df %>% dplyr::mutate(study_area = dplyr::recode(study_area, "Study Area 6" = 6))
SA6.df <- SA6.df %>% dplyr::mutate(SFPID = ifelse(in_HD==TRUE, "6_Village", "6_State"))
head(SA6.df)

SA7.df <- SA7.df %>% dplyr::mutate(study_area = dplyr::recode(study_area, "Study Area 7" = 7))
SA7.df <- SA7.df %>% dplyr::mutate(SFPID = ifelse(in_HD==TRUE, "7_Village", "7_State"))
head(SA7.df)

SA8.df <- SA8.df %>% dplyr::mutate(study_area = dplyr::recode(study_area, "Study Area 8" = 8))
SA8.df <- SA8.df %>% dplyr::mutate(SFPID = ifelse(in_HD==TRUE, "8_Village", "8_State"))
head(SA8.df)

SA9.df <- SA9.df %>% dplyr::mutate(study_area = dplyr::recode(study_area, "Study Area 9" = 9))
SA9.df <- SA9.df %>% dplyr::mutate(SFPID = ifelse(in_HD==TRUE, "9_Village", "9_State"))
head(SA9.df)

SA10.df <- SA10.df %>% dplyr::mutate(study_area = dplyr::recode(study_area, "Study Area 10" = 10))
SA10.df <- SA10.df %>% dplyr::mutate(SFPID = ifelse(in_HD==TRUE, "10_Village", "10_State"))
head(SA10.df)

SA11.df <- SA11.df %>% dplyr::mutate(study_area = dplyr::recode(study_area, "Study Area 11" = 11))
SA11.df <- SA11.df %>% dplyr::mutate(SFPID = ifelse(in_HD==TRUE, "11_Village", "11_State"))
head(SA11.df)


pooled <- bind_rows(SA1.df, SA2.df, SA3.df, SA4.df, SA5.df, SA6.df, SA7.df, SA8.df,
                    SA9.df, SA10.df, SA11.df)

pooled_sf <- st_sf(pooled, crs = 4326)

# Convert study areas into factor variables
pooled$study_area <- factor(pooled$study_area)
str(pooled[,-1])

pooled_sf$study_area <- factor(pooled_sf$study_area)
str(pooled_sf)

# Rename in_HD column 
library(plyr)
pooled <- plyr::rename(pooled, c("in_HD" = "SFP"))
pooled_sf <- plyr::rename(pooled_sf, c("in_HD" = "SFP"))

# Save dataframe to drive 
saveRDS(pooled, file = "RFiles/StudyAreas/pooled.Rda")
saveRDS(pooled_sf, file = "RFiles/StudyAreas/pooled_sf.Rda")

# Status: Edited as of 21 Mar 2019

# [DO NOT RUN] Pooled dataset for HD only 
SA1.df <- readRDS("RFiles/StudyAreas/SA1full.Rda")
SA2.df <- readRDS("RFiles/StudyAreas/SA2full.Rda")
SA3.df <- readRDS("RFiles/StudyAreas/SA3full.Rda")
SA4.df <- readRDS("RFiles/StudyAreas/SA4full.Rda")
SA5.df <- readRDS("RFiles/StudyAreas/SA5full.Rda")
SA6.df <- readRDS("RFiles/StudyAreas/SA6full.Rda")
SA11.df <- readRDS("RFiles/StudyAreas/SA11full.Rda")

# Convert Study Areas into numerics
SA1.df <- SA1.df %>% dplyr::mutate(study_area = dplyr::recode(study_area, "Study Area 1" = 1))
head(SA1.df)

SA2.df <- SA2.df %>% dplyr::mutate(study_area = dplyr::recode(study_area, "Study Area 2" = 2))
head(SA2.df)

SA3.df <- SA3.df %>% dplyr::mutate(study_area = dplyr::recode(study_area, "Study Area 3" = 3))
head(SA3.df)

SA4.df <- SA4.df %>% dplyr::mutate(study_area = dplyr::recode(study_area, "Study Area 4" = 4))
head(SA4.df)

SA5.df <- SA5.df %>% dplyr::mutate(study_area = dplyr::recode(study_area, "Study Area 5" = 5))
head(SA5.df)

SA6.df <- SA6.df %>% dplyr::mutate(study_area = dplyr::recode(study_area, "Study Area 6" = 6))
head(SA6.df)

SA11.df <- SA11.df %>% dplyr::mutate(study_area = dplyr::recode(study_area, "Study Area 11" = 11))
head(SA11.df)

pooledHD <- bind_rows(SA1.df, SA2.df, SA3.df, SA4.df, SA5.df, SA6.df, SA11.df)

pooledHD_sf <- st_sf(pooledHD, crs = 4326)

# Convert study areas into factor variables
pooledHD$study_area <- factor(pooledHD$study_area)
str(pooledHD)

pooledHD_sf$study_area <- factor(pooledHD_sf$study_area)
str(pooledHD_sf)

# Rename in_HD column 
library(plyr)
pooledHD <- plyr::rename(pooledHD, c("in_HD" = "SFP"))
pooledHD_sf <- plyr::rename(pooledHD_sf, c("in_HD" = "SFP"))

# Save dataframe to drive 
saveRDS(pooledHD, file = "RFiles/StudyAreas/pooledHD.Rda")
saveRDS(pooledHD_sf, file = "RFiles/StudyAreas/pooledHD_sf.Rda")

# 3. Creation of covariates
###########################################################################################
# Soil quality 
soilHD <- st_read("Polygons/Study Area 1/Soil_HD_1.shp")
soilHL <- st_read("Polygons/Study Area 1/Soil_HL_1.shp")
hud <- st_read("Polygons/Study Area 1/HD_Diff_1.shp")
SA1.df <- SA1.df

plot(st_geometry(soilHD))
plot(st_geometry(soilHL))

soilHD <- as.data.frame(soilHD)
soilHD <- soilHD[,3]
head(soilHD)
soilHD <- as.character(soilHD)

soilHL <- as.data.frame(soilHL)
soilHL <- soilHL[,3]
head(soilHL)
soilHL <- as.character(soilHL)

SA1.df$soil <- ifelse(SA1.df$in_HD==TRUE, soilHD, soilHL)
head(SA1.df)
covs_1 <- SA1.df %>% select(study_area,SFPID, soil, province, distrelative)

# Study area (2) 
soilHD <- st_read("Polygons/Study Area 2/Soil_HD_2.shp")
soilHL <- st_read("Polygons/Study Area 2/Soil_HL_2.shp")
hud <- st_read("Polygons/Study Area 2/HD_Diff_2.shp")
SA2.df <- SA2.df

plot(st_geometry(soilHD))
plot(st_geometry(soilHL))

soilHD <- as.data.frame(soilHD)
soilHD <- soilHD[,3]
head(soilHD)
soilHD <- as.character(soilHD)

soilHL <- as.data.frame(soilHL)
soilHL <- soilHL[,3]
head(soilHL)
soilHL <- as.character(soilHL)

SA2.df$soil <- ifelse(SA2.df$in_HD==TRUE, soilHD, soilHL)
head(SA2.df)
covs_2 <- SA2.df %>% select(study_area,SFPID, soil, province, distrelative)
head(covs_2)

# Study area (3)
soilHD <- st_read("Polygons/Study Area 3/Soil_HD_3.shp")
soilHL <- st_read("Polygons/Study Area 3/Soil_HL_3.shp")
hud <- st_read("Polygons/Study Area 3/HD_Diff_3.shp")
SA3.df <- SA3.df

plot(st_geometry(soilHD))
plot(st_geometry(soilHL))

soilHD <- as.data.frame(soilHD)
# 2 levels of soil classification present
# QGIS shows that the dominant soil classification is Od across the border
# Selecting first row only (Od)
soilHD <- soilHD[1,3]
head(soilHD)
soilHD <- as.character(soilHD)

soilHL <- as.data.frame(soilHL)
# 2 levels of soil classification present
# QGIS shows that the dominant soil classification is Od across the border
# Selecting first row only (Od)
soilHL <- soilHL[1,3]
head(soilHL)
soilHL <- as.character(soilHL)

SA3.df$soil <- ifelse(SA3.df$in_HD==TRUE, soilHD, soilHL)
head(SA3.df)
covs_3 <- SA3.df %>% select(study_area,SFPID, soil, province, distrelative)
head(covs_3)

# Study area (4)
soilHD <- st_read("Polygons/Study Area 4/Soil_HD_4.shp")
soilHL <- st_read("Polygons/Study Area 4/Soil_HL_4.shp")
hud <- st_read("Polygons/Study Area 4/HD_Diff_4.shp")
SA4.df <- SA4.df
SA4_sf <- SA4_sf
plot(st_geometry(SA4_sf))

# Assigning different soil classes to individual points 
soilHD_4 <- st_join(SA4_sf, soilHD)
soilHD_4 <- soilHD_4[,9]

soilHL_4 <- st_join(SA4_sf, soilHL)
soilHL_4 <- soilHL_4[,9]

# Remove NA values 
soilHD_4 <- na.omit(soilHD_4)
soilHL_4 <- na.omit(soilHL_4)

# Convert into vectors
soilHD <- as.data.frame(soilHD_4)
soilHD <- soilHD[,1]
soilHD <- as.character(soilHD)

soilHL <- as.data.frame(soilHL_4)
soilHL <- soilHL[,1]
soilHL <- as.character(soilHL)

# Add to dataframe
SA4.df$soil <- ifelse(SA4.df$in_HD==TRUE, soilHD, soilHL)
head(SA4.df)
covs_4 <- SA4.df %>% select(study_area,SFPID, soil, province, distrelative)
head(covs_4)

# Study Area (5)
soilHD <- st_read("Polygons/Study Area 5/Soil_HD_5.shp")
soilHL <- st_read("Polygons/Study Area 5/Soil_HL_5.shp")
SA5.df <- SA5.df

plot(st_geometry(soilHD))
plot(st_geometry(soilHL))

soilHD <- as.data.frame(soilHD)
soilHD <- soilHD[,3]
head(soilHD)
soilHD <- as.character(soilHD)

soilHL <- as.data.frame(soilHL)
soilHL <- soilHL[,3]
head(soilHL)
soilHL <- as.character(soilHL)

SA5.df$soil <- ifelse(SA5.df$in_HD==TRUE, soilHD, soilHL)
head(SA5.df)
covs_5 <- SA5.df %>% select(study_area,SFPID, soil, province, distrelative)
head(covs_5)

# Study Area (6)
soilHD <- st_read("Polygons/Study Area 6/Soil_HD_6.shp")
soilHL <- st_read("Polygons/Study Area 6/Soil_HL_6.shp")
SA6.df <- SA6.df

plot(st_geometry(soilHD))
plot(st_geometry(soilHL))

soilHD <- as.data.frame(soilHD)
soilHD <- soilHD[,3]
head(soilHD)
soilHD <- as.character(soilHD)

soilHL <- as.data.frame(soilHL)
soilHL <- soilHL[,3]
head(soilHL)
soilHL <- as.character(soilHL)

SA6.df$soil <- ifelse(SA6.df$in_HD==TRUE, soilHD, soilHL)
head(SA6.df)
covs_6 <- SA6.df %>% select(study_area,SFPID, soil, province, distrelative)
head(covs_6)

# Study area (7)
soilHD <- st_read("Polygons/Study Area 7/Soil_HKm_7.shp")
soilHL <- st_read("Polygons/Study Area 7/Soil_HL_7.shp")
SA7.df <- SA7.df

plot(st_geometry(soilHD))
plot(st_geometry(soilHL))
# SoilHL has two classes of soil type but dominant soil type is still Qc 

soilHD <- as.data.frame(soilHD)
soilHD <- soilHD[,3]
head(soilHD)
soilHD <- as.character(soilHD)

soilHL <- as.data.frame(soilHL)
soilHL <- soilHL[1,3]
head(soilHL)
soilHL <- as.character(soilHL)

SA7.df$soil <- ifelse(SA7.df$in_HD==TRUE, soilHD, soilHL)
head(SA7.df)
covs_7 <- SA7.df %>% select(study_area,SFPID, soil, province, distrelative)
head(covs_7)

# Study area (8)
soilHD <- st_read("Polygons/Study Area 8/Soil_HKm_8.shp")
soilHL <- st_read("Polygons/Study Area 8/Soil_HL_8.shp")
SA8.df <- SA8.df
SA8_sf <- SA8_sf

# Assigning different soil classes to individual points 
soilHD_8 <- st_join(SA8_sf, soilHD)
soilHD_8 <- soilHD_8[,9]

soilHL_8 <- st_join(SA8_sf, soilHL)
soilHL_8 <- soilHL_8[,9]

# Remove NA values 
soilHD_8 <- na.omit(soilHD_8)
soilHL_8 <- na.omit(soilHL_8)

# Convert into vectors
soilHD <- as.data.frame(soilHD_8)
soilHD <- soilHD[,1]
soilHD <- as.character(soilHD)

soilHL <- as.data.frame(soilHL_8)
soilHL <- soilHL[,1]
soilHL <- as.character(soilHL)

# Add to dataframe
SA8.df$soil <- ifelse(SA8.df$in_HD==TRUE, soilHD, soilHL)
head(SA8.df)
covs_8 <- SA8.df %>% select(study_area,SFPID, soil, province, distrelative)
head(covs_8)

# Study area (9)
soilHD <- st_read("Polygons/Study Area 9/Soil_HKm_9.shp")
soilHL <- st_read("Polygons/Study Area 9/Soil_HL_9.shp")
SA9.df <- SA9.df
SA9_sf <- SA9_sf

# Assigning different soil classes to individual points 
soilHD_9 <- st_join(SA9_sf, soilHD)
soilHD_9 <- soilHD_9[,9]

soilHL_9 <- st_join(SA9_sf, soilHL)
soilHL_9 <- soilHL_9[,9]

# Remove NA values 
soilHD_9 <- na.omit(soilHD_9)
soilHL_9 <- na.omit(soilHL_9)

# Convert into vectors
soilHD <- as.data.frame(soilHD_9)
soilHD <- soilHD[,1]
soilHD <- as.character(soilHD)

soilHL <- as.data.frame(soilHL_9)
soilHL <- soilHL[,1]
soilHL <- as.character(soilHL)

# Add to dataframe
SA9.df$soil <- ifelse(SA9.df$in_HD==TRUE, soilHD, soilHL)
head(SA9.df)
covs_9 <- SA9.df %>% select(study_area,SFPID, soil, province, distrelative)
head(covs_9)

# Study area (10)
soilHD <- st_read("Polygons/Study Area 10/Soil_HKm_10.shp")
soilHL <- st_read("Polygons/Study Area 10/Soil_HL_10.shp")
SA10.df <- SA10.df

soilHD <- as.data.frame(soilHD)
soilHD <- soilHD[,3]
head(soilHD)
soilHD <- as.character(soilHD)

soilHL <- as.data.frame(soilHL)
soilHL <- soilHL[,3]
head(soilHL)
soilHL <- as.character(soilHL)

SA10.df$soil <- ifelse(SA10.df$in_HD==TRUE, soilHD, soilHL)
head(SA10.df)
covs_10 <- SA10.df %>% select(study_area,SFPID, soil, province, distrelative)
head(covs_10)

# Study area (11)
soilHD <- st_read("Polygons/Study Area 11/Soil_HD_11.shp")
soilHL <- st_read("Polygons/Study Area 11/Soil_HL_11.shp")
SA11.df <- SA11.df

plot(st_geometry(soilHD))
plot(st_geometry(soilHL))

soilHD <- as.data.frame(soilHD)
soilHD <- soilHD[,3]
head(soilHD)
soilHD <- as.character(soilHD)

soilHL <- as.data.frame(soilHL)
soilHL <- soilHL[,3]
head(soilHL)
soilHL <- as.character(soilHL)

SA11.df$soil <- ifelse(SA11.df$in_HD==TRUE, soilHD, soilHL)
head(SA11.df)
covs_11 <- SA11.df %>% select(study_area,SFPID, soil, province, distrelative)
head(covs_11)

# Combine covariate dataframe
covs_df <- bind_rows(covs_1, covs_2, covs_3, covs_4, covs_5, covs_6, covs_7, covs_8,
                     covs_9, covs_10, covs_11)

covs_df <- covs_df %>% arrange(study_area)

saveRDS(covs_df, file = "RFiles/StudyAreas/covs.Rda")

ggplot(covs_df, aes(x=soil)) + geom_bar()
ggplot(covs_df, aes(x=soil, y=distrelative)) + geom_boxplot() + coord_flip()



