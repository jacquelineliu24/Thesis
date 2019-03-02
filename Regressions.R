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
library(stargazer)
library(rdrobust)

# Load datasets for analysis
tamonarang.df <- readRDS("testDataset.Rda")
lossdf <- readRDS("lossdf.Rda")
wide <- readRDS("tamwide.Rda")
panel <- readRDS("tampanel.Rda")

# Preliminary regression plots to inspect the pattern in the data
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

# year on year plots 
y1 <- wide$yr_1
y2 <- wide$yr_2 
y3 <- wide$yr_3
y4 <- wide$yr_4
y5 <- wide$yr_5
y6 <- wide$yr_6
y7 <- wide$yr_7
y8 <- wide$yr_8
y9 <- wide$yr_9
y10 <- wide$yr_10
y11 <- wide$yr_11
y12 <- wide$yr_12
y13 <- wide$yr_13
y14 <- wide$yr_14
y15 <- wide$yr_15
y16 <- wide$yr_16
y17 <- wide$yr_17

rdplot(y1, dist, c = 0)
rdplot(y10, dist, c = 0)

# Cumulative plots 
# in HD


# not in HD


# Diff-in-diff estimation 
# Create dummy variable to indicate the time when the treatment started (2008). 
panel$time = ifelse(panel$lossyear > 8, 1, 0)

# Dummy variable to indicate the group exposed to the treatment = panel$in_HD

# Create d-i-d interaction (time*treatment)
panel$did = panel$time * panel$in_HD

# Estimate the DID estimator 
y = panel$treecover 
treated = panel$in_HD
time = panel$time
did = panel$did

didreg = lm(y ~ treated + time + did, data = panel)
summary(didreg)

didreg1 = lm(y ~ treated*time, data = panel)
summary(didreg1)

panel$res <- resid(didreg)
resplot <- ggplot(panel, aes(did, res, colour = factor(in_HD))) + geom_jitter(alpha = 0.5)

# The DiD estimator is = -19.7062. 
# After 2008 and within the HD land title, forest cover decreased by 19%. 

# RD estimation 
library(rddtools)

# Without DiD
y00<- tamonarang.df$treecover2000
tamonarang.df$treecover2017 <- wide$yr_17
y17 <- tamonarang.df$treecover2017
dist <- tamonarang.df$distrelative
rdplot(y00, dist, c=0, y.lim = c(0, 100))
rdplot(y17, dist, c=0, y.lim = c(0,100))

rdreg17 <- rdrobust(y17, dist, c=0, all = TRUE)
summary(rdreg17)
# The rd treatment effect in 2017 is 2.809 (bias-corrected)
# The true treatment effect is: 2.809 - (-19.7062)
effect = 2.809-(-19.7062)
effect
# 22.5152
# As distance to the border increased by 1m, forest cover increased by 22%
# As distance to the border increased by 1km, forest cover increased by 0.022%

# OLS Fixed Effects

