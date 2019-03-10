# Dataset - Creating the main variables
# Set working directory
setwd("/Users/jacquelineliu/Desktop/Thesis_Data/Input")

# Load packages
library(sf)
library(ggplot2)
library(rgdal)
library(sp)
library(raster)
library(spex)
library(dplyr)
library(tidyverse)
library(rgeos)
library(geosphere)
library(rdrobust)

# Descriptive statistics - plots 
library(ggthemes)
library(gridExtra)

##################################################################################
# (T) Test area 

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

###################################################################################
# (1) Study Area 1

SA1.df <- readRDS("RFiles/StudyAreas/SA1full.Rda")

# year on year plots 
y0 <- SA1.df$yr_00
y1 <- SA1.df$yr_1
y2 <- SA1.df$yr_2 
y3 <- SA1.df$yr_3
y4 <- SA1.df$yr_4
y5 <- SA1.df$yr_5
y6 <- SA1.df$yr_6
y7 <- SA1.df$yr_7
y8 <- SA1.df$yr_8
y9 <- SA1.df$yr_9
y10 <- SA1.df$yr_10
y11 <- SA1.df$yr_11
y12 <- SA1.df$yr_12
y13 <- SA1.df$yr_13
y14 <- SA1.df$yr_14
y15 <- SA1.df$yr_15
y16 <- SA1.df$yr_16
y17 <- SA1.df$yr_17

x <- SA1.df$distrelative

par(mar=c(1,1,1,1))
par(mfrow = c(2,4))
rdplot(y1, x, c = 0, col.dots = "grey", p = 1, title = "2001", y.lim = c(60,120))
rdplot(y2, x, c = 0, col.dots = "grey", p = 1, title = "2002", y.lim = c(60,120))
rdplot(y3, x, c = 0, col.dots = "grey", p = 1, title = "2003", y.lim = c(60,120))
rdplot(y4, x, c = 0, col.dots = "grey", p = 1, title = "2004", y.lim = c(60,120))
rdplot(y5, x, c = 0, col.dots = "grey", p = 1, title = "2005", y.lim = c(60,120))
rdplot(y6, x, c = 0, col.dots = "grey", p = 1, title = "2006", y.lim = c(60,120))
rdplot(y7, x, c = 0, col.dots = "grey", p = 1, title = "2007", y.lim = c(60,120))
rdplot(y8, x, c = 0, col.dots = "grey", p = 1, title = "2008", y.lim = c(60,120))
rdplot(y9, x, c = 0, col.dots = "grey", p = 1, title = "2009", y.lim = c(60,120))
rdplot(y10, x, c = 0, col.dots = "grey", p = 1, title = "2010", y.lim = c(60,120))
rdplot(y11, x, c = 0, col.dots = "grey", p = 1, title = "2011", y.lim = c(60,120))
rdplot(y12, x, c = 0, col.dots = "grey", p = 1, title = "2012", y.lim = c(60,120))
rdplot(y13, x, c = 0, col.dots = "grey", p = 1, title = "2013", y.lim = c(60,120))
rdplot(y14, x, c = 0, col.dots = "grey", p = 1, title = "2014", y.lim = c(60,120))
rdplot(y15, x, c = 0, col.dots = "grey", p = 1, title = "2015", y.lim = c(60,120))
rdplot(y16, x, c = 0, col.dots = "grey", p = 1, title = "2016", y.lim = c(60,120))


# 2000 vs. 2017 
par(mfrow = c(1, 2))
rdplot(y0, x, c = 0, col.dots = "grey", p = 1, title = "2001", 
       y.lim = c(90,100), x.label = "Distance to the border (in m)", 
       y.label = "% of forest cover")
rdplot(y17, x, c = 0, col.dots = "grey", p = 1, title = "2017", 
       y.lim = c(90,100), x.label = "Distance to the border (in m)", 
       y.label = "% of forest cover")

###################################################################################
# (2) Study Area 2

# Load datasets
SA2.df <- readRDS("RFiles/StudyAreas/SA2full.Rda")

# year on year plots 
y0 <- SA2.df$yr_00
y1 <- SA2.df$yr_1
y2 <- SA2.df$yr_2 
y3 <- SA2.df$yr_3
y4 <- SA2.df$yr_4
y5 <- SA2.df$yr_5
y6 <- SA2.df$yr_6
y7 <- SA2.df$yr_7
y8 <- SA2.df$yr_8
y9 <- SA2.df$yr_9
y10 <- SA2.df$yr_10
y11 <- SA2.df$yr_11
y12 <- SA2.df$yr_12
y13 <- SA2.df$yr_13
y14 <- SA2.df$yr_14
y15 <- SA2.df$yr_15
y16 <- SA2.df$yr_16
y17 <- SA2.df$yr_17

x <- SA2.df$distrelative

par(mfrow = c(2,4))
rdplot(y1, x, c = 0, col.dots = "grey", p = 1, title = "2001", y.lim = c(60,120))
rdplot(y2, x, c = 0, col.dots = "grey", p = 1, title = "2002", y.lim = c(60,120))
rdplot(y3, x, c = 0, col.dots = "grey", p = 1, title = "2003", y.lim = c(60,120))
rdplot(y4, x, c = 0, col.dots = "grey", p = 1, title = "2004", y.lim = c(60,120))
rdplot(y5, x, c = 0, col.dots = "grey", p = 1, title = "2005", y.lim = c(60,120))
rdplot(y6, x, c = 0, col.dots = "grey", p = 1, title = "2006", y.lim = c(60,120))
rdplot(y7, x, c = 0, col.dots = "grey", p = 1, title = "2007", y.lim = c(60,120))
rdplot(y8, x, c = 0, col.dots = "grey", p = 1, title = "2008", y.lim = c(60,120))
rdplot(y9, x, c = 0, col.dots = "grey", p = 1, title = "2009", y.lim = c(60,120))
rdplot(y10, x, c = 0, col.dots = "grey", p = 1, title = "2010", y.lim = c(60,120))
rdplot(y11, x, c = 0, col.dots = "grey", p = 1, title = "2011", y.lim = c(60,120))
rdplot(y12, x, c = 0, col.dots = "grey", p = 1, title = "2012", y.lim = c(60,120))
rdplot(y13, x, c = 0, col.dots = "grey", p = 1, title = "2013", y.lim = c(60,120))
rdplot(y14, x, c = 0, col.dots = "grey", p = 1, title = "2014", y.lim = c(60,120))
rdplot(y15, x, c = 0, col.dots = "grey", p = 1, title = "2015", y.lim = c(60,120))
rdplot(y16, x, c = 0, col.dots = "grey", p = 1, title = "2016", y.lim = c(60,120))
rdplot(y17, x, c = 0, col.dots = "grey", p = 1, title = "2017", y.lim = c(60,120))

# 2000 vs. 2017 
par(mfrow = c(1, 2))
rdplot(y0, x, c = 0, col.dots = "grey", p = 1, title = "2001", 
       y.lim = c(80,100), x.label = "Distance to the border (in m)", 
       y.label = "% of forest cover")
rdplot(y17, x, c = 0, col.dots = "grey", p = 1, title = "2017", 
       y.lim = c(80,100), x.label = "Distance to the border (in m)", 
       y.label = "% of forest cover")

###################################################################################
# (3) Study Area 3

# Load datasets
SA3.df <- readRDS("RFiles/StudyAreas/SA3full.Rda")

# year on year plots 
y0 <- SA3.df$yr_00
y1 <- SA3.df$yr_1
y2 <- SA3.df$yr_2 
y3 <- SA3.df$yr_3
y4 <- SA3.df$yr_4
y5 <- SA3.df$yr_5
y6 <- SA3.df$yr_6
y7 <- SA3.df$yr_7
y8 <- SA3.df$yr_8
y9 <- SA3.df$yr_9
y10 <- SA3.df$yr_10
y11 <- SA3.df$yr_11
y12 <- SA3.df$yr_12
y13 <- SA3.df$yr_13
y14 <- SA3.df$yr_14
y15 <- SA3.df$yr_15
y16 <- SA3.df$yr_16
y17 <- SA3.df$yr_17

x <- SA3.df$distrelative

par(mfrow = c(2,4))
rdplot(y1, x, c = 0, col.dots = "grey", p = 1, title = "2001", y.lim = c(40,100))
rdplot(y2, x, c = 0, col.dots = "grey", p = 1, title = "2002", y.lim = c(40,100))
rdplot(y3, x, c = 0, col.dots = "grey", p = 1, title = "2003", y.lim = c(40,100))
rdplot(y4, x, c = 0, col.dots = "grey", p = 1, title = "2004", y.lim = c(40,100))
rdplot(y5, x, c = 0, col.dots = "grey", p = 1, title = "2005", y.lim = c(40,100))
rdplot(y6, x, c = 0, col.dots = "grey", p = 1, title = "2006", y.lim = c(40,100))
rdplot(y7, x, c = 0, col.dots = "grey", p = 1, title = "2007", y.lim = c(40,100))
rdplot(y8, x, c = 0, col.dots = "grey", p = 1, title = "2008", y.lim = c(40,100))
rdplot(y9, x, c = 0, col.dots = "grey", p = 1, title = "2009", y.lim = c(40,100))
rdplot(y10, x, c = 0, col.dots = "grey", p = 1, title = "2010", y.lim = c(40,100))
rdplot(y11, x, c = 0, col.dots = "grey", p = 1, title = "2011", y.lim = c(40,100))
rdplot(y12, x, c = 0, col.dots = "grey", p = 1, title = "2012", y.lim = c(40,100))
rdplot(y13, x, c = 0, col.dots = "grey", p = 1, title = "2013", y.lim = c(40,100))
rdplot(y14, x, c = 0, col.dots = "grey", p = 1, title = "2014", y.lim = c(40,100))
rdplot(y15, x, c = 0, col.dots = "grey", p = 1, title = "2015", y.lim = c(40,100))
rdplot(y16, x, c = 0, col.dots = "grey", p = 1, title = "2016", y.lim = c(40,100))
rdplot(y17, x, c = 0, col.dots = "grey", p = 1, title = "2017", y.lim = c(40,100))

# 2000 vs. 2017 
par(mfrow = c(1, 2))
rdplot(y0, x, c = 0, col.dots = "grey", p = 1, title = "2001", 
       y.lim = c(50,100), x.label = "Distance to the border (in m)", 
       y.label = "% of forest cover")
rdplot(y17, x, c = 0, col.dots = "grey", p = 1, title = "2017", 
       y.lim = c(50,100), x.label = "Distance to the border (in m)", 
       y.label = "% of forest cover")

###################################################################################
# (4) Study Area 4

# Load datasets
SA4.df <- readRDS("RFiles/StudyAreas/SA4full.Rda")

# year on year plots 
y0 <- SA4.df$yr_00
y1 <- SA4.df$yr_1
y2 <- SA4.df$yr_2 
y3 <- SA4.df$yr_3
y4 <- SA4.df$yr_4
y5 <- SA4.df$yr_5
y6 <- SA4.df$yr_6
y7 <- SA4.df$yr_7
y8 <- SA4.df$yr_8
y9 <- SA4.df$yr_9
y10 <- SA4.df$yr_10
y11 <- SA4.df$yr_11
y12 <- SA4.df$yr_12
y13 <- SA4.df$yr_13
y14 <- SA4.df$yr_14
y15 <- SA4.df$yr_15
y16 <- SA4.df$yr_16
y17 <- SA4.df$yr_17

x <- SA4.df$distrelative

par(mfrow = c(2,4))
rdplot(y1, x, c = 0, col.dots = "grey", p = 1, title = "2001", y.lim = c(40,100))
rdplot(y2, x, c = 0, col.dots = "grey", p = 1, title = "2002", y.lim = c(40,100))
rdplot(y3, x, c = 0, col.dots = "grey", p = 1, title = "2003", y.lim = c(40,100))
rdplot(y4, x, c = 0, col.dots = "grey", p = 1, title = "2004", y.lim = c(40,100))
rdplot(y5, x, c = 0, col.dots = "grey", p = 1, title = "2005", y.lim = c(40,100))
rdplot(y6, x, c = 0, col.dots = "grey", p = 1, title = "2006", y.lim = c(40,100))
rdplot(y7, x, c = 0, col.dots = "grey", p = 1, title = "2007", y.lim = c(40,100))
rdplot(y8, x, c = 0, col.dots = "grey", p = 1, title = "2008", y.lim = c(40,100))
rdplot(y9, x, c = 0, col.dots = "grey", p = 1, title = "2009", y.lim = c(40,100))
rdplot(y10, x, c = 0, col.dots = "grey", p = 1, title = "2010", y.lim = c(40,100))
rdplot(y11, x, c = 0, col.dots = "grey", p = 1, title = "2011", y.lim = c(40,100))
rdplot(y12, x, c = 0, col.dots = "grey", p = 1, title = "2012", y.lim = c(40,100))
rdplot(y13, x, c = 0, col.dots = "grey", p = 1, title = "2013", y.lim = c(40,100))
rdplot(y14, x, c = 0, col.dots = "grey", p = 1, title = "2014", y.lim = c(40,100))
rdplot(y15, x, c = 0, col.dots = "grey", p = 1, title = "2015", y.lim = c(40,100))
rdplot(y16, x, c = 0, col.dots = "grey", p = 1, title = "2016", y.lim = c(40,100))
rdplot(y17, x, c = 0, col.dots = "grey", p = 1, title = "2017", y.lim = c(40,100))

# 2000 vs. 2017 
par(mfrow = c(1, 2))
rdplot(y0, x, c = 0, col.dots = "grey", p = 1, title = "2001", 
       y.lim = c(50,100), x.label = "Distance to the border (in m)", 
       y.label = "% of forest cover")
rdplot(y17, x, c = 0, col.dots = "grey", p = 1, title = "2017", 
       y.lim = c(50,100), x.label = "Distance to the border (in m)", 
       y.label = "% of forest cover")


rdplot(y0, x, c = 0, col.dots = "grey", p = 4, title = "2001", 
       y.lim = c(50,100), x.label = "Distance to the border (in m)", 
       y.label = "% of forest cover")
rdplot(y17, x, c = 0, col.dots = "grey", p = 4, title = "2017", 
       y.lim = c(50,100), x.label = "Distance to the border (in m)", 
       y.label = "% of forest cover")


