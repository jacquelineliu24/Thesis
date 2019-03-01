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

# Descriptive statistics - plots 
library(ggthemes)
library(gridExtra)

# Load dataset(s)
tamonarang.df <- readRDS("testDataset.Rda")
lossdf <- readRDS("lossdf.Rda")

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

ggplot(tamonarang.df, aes(distrelative)) + geom_density()
