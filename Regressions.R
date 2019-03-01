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

# Load datasets for analysis
tamonarang.df <- readRDS("testDataset.Rda")
lossdf <- readRDS("lossdf.Rda")

# RD plot for treecover in 2000
y <- tamonarang.df$treecover2000
x <- tamonarang.df$distrelative
z <- tamonarang.df$treecover2017

rdplot(y, x, c = 0, title = "Baseline RD Plot", x.label = "Distance to the border", 
       y.label = "Forest cover in 2000", h = 425, x.lim = c(-800, 800))
robust <- rdrobust(y, x, c = 0, all = TRUE)
summary(robust)
bandwidth <- rdbwselect(z, x, c = 0, all = TRUE)
summary(bandwidth)

# RD plot for treecover in 2017
rdplot(z, x, c = 0, binselect = "es")

# Testing with different bandwidth sizes 
# Based on results in line 179, summary(robust), optimal bandwidth selected was 545.092. 
# Testing using bandwidth of 550 (rounded up) - DISTANCE IS IN METRES
#rdsample <- subset(tamonarang.df, distrelative <= 550 & distrelative >= -550)
#summary(rdsample)

#y1 <- rdsample$treecover2000 
#x1 <- rdsample$distrelative
#z1 <- rdsample$treecover2017

# Using evenly-sized bins to smoothen the plots 
rdplot(y1, x1, c=0, y.lim=c(0,100), binselect = "es", x.label = "Distance to the border", y.label = "Forest cover in 2000")
rdplot(z1, x1, c=0, y.lim=c(0,100), binselect = "es", x.label = "Distance to the border", y.label = "Forest cover in 2017")

robust1 <- rdrobust(y1, x1, c = 0, all = TRUE)
summary(robust1)


# At higher bandwidths (> 100), it looks like the distribution of points is quite continuous
# in both plots for 2000 and 2017 - no effect? May need to experiment with a longer border.

# Experimenting with different bin sizes 
# Evenly-spaced bins
plot_ebin <- rdplot(y, x, c = 0, binselect = "es")
summary(plot_ebin)

# Quantile-spaced bins
plot_qbin <- rdplot(y, x, c = 0, binselect = "qs")
summary(plot_qbin)

plot_bin <- rdplot(y, x, c = 0, binselect = "qsmv")
summary(plot_bin)

# With binscatter package 
install.packages("binscatter")
library(binscatter)

# With binsreg package 
install.packages("binsreg")
library(binsreg)


