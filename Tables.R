# Set working directory
setwd("/Users/jacquelineliu/Desktop/Thesis_Data/Input")

# Load packages
# Data manipulation packages
library(sf)
library(rgdal)
library(sp)
library(raster)
library(spex)
library(dplyr)
library(tidyverse)
library(rgeos)
library(geosphere)
library(readxl)
library(broom)
# RD estimation tools
library(rdrobust)
library(rddensity)
# Table tools
library(stargazer)

# Load data
pooled <- readRDS("RFiles/StudyAreas/pooled_reg.Rda")

# Table 2: Descriptive statistics - continuous variables
descriptive <- pooled %>% select(SFP, distrelative, rate, prerate, postrate)
descr_SFP <- descriptive %>% filter(SFP==TRUE)
descr_state <- descriptive %>% filter(SFP==FALSE)

library(stargazer)
stargazer(descriptive, type="latex", summary=TRUE)
stargazer(descr_SFP, type="latex", summary=TRUE)
stargazer(descr_state, type="latex", summary=TRUE)

# Table 3: Difference-in-Regression Discontinuity Estimation Results
x <- pooled$distrelative
y_pre <- pooled$prerate
x_pre <- pooled$distrelative

y_post <- pooled$postrate
x_post <- pooled$distrelative

bw = 304.836 

# rdrobust 
# Column 1
pre_reg <- rdrobust(y_pre, x_pre, c=0, h=bw, p=1)
pre_results <- cbind(pre_reg$coef, pre_reg$z, pre_reg$se, pre_reg$pv, pre_reg$ci)
pre_results

# Column 2
post_reg <- rdrobust(y_post, x_post, c=0, h=bw, p=1)
post_results <- cbind(post_reg$coef, post_reg$z, post_reg$se, post_reg$pv, post_reg$ci)
post_results

# Table 4: Results of McCrary Density Test
dens <- rddensity(x, c=0, p=1, h=305, all=TRUE)
summary(dens)

# Table 7: Frequency table - Provinces
stargazer(pooled$province)

# Table 8: Frequency table - Study areas 
freq <- pooled %>% select(SFPID) %>% arrange()

freq(freq, style="rmarkdown", report.nas = FALSE, headings=FALSE)

View(descriptive)
pairs(descriptive[,c(2:3)])

# Table 9: Year-on-year RD Estimates (2001-2017)
# Refer to Figure 13 in Figures.R

# Table 10: Difference-in-Regression Discontinuity Estimation Results
# with MSE-optimal bandwidth 
bw1 = 623.523
#Column 1
pre_reg_IK <- rdrobust(y_pre, x_pre, c=0, h=bw1, p=1)
pre_results_IK <- cbind(pre_reg_IK$coef, pre_reg_IK$z, pre_reg_IK$se, pre_reg_IK$pv, pre_reg_IK$ci)
pre_results_IK

# Column 2
post_reg_IK <- rdrobust(y_post, x_post, c=0, h=bw1, p=1)
post_results_IK <- cbind(post_reg_IK$coef, post_reg_IK$z, post_reg_IK$se, post_reg_IK$pv, post_reg_IK$ci)
post_results_IK

# Table 11: Sensitivity of DiRD coefficients to different bandwidths
pre_reg <- rdrobust(y_pre, x_pre, c=0, h=bw, p=1)
pre_results <- cbind(pre_reg$coef, pre_reg$z, pre_reg$se, pre_reg$pv, pre_reg$ci)
pre_results

post_reg <- rdrobust(y_post, x_post, c=0, h=bw, p=1)
post_results <- cbind(post_reg$coef, post_reg$z, post_reg$se, post_reg$pv, post_reg$ci)
post_results

pre_reg2 <- rdrobust(y_pre, x_pre, c=0, h=(bw-5), p=1)
pre_results2 <- cbind(pre_reg2$coef, pre_reg2$z, pre_reg2$se, pre_reg2$pv, pre_reg2$ci)
pre_results2

post_reg2 <- rdrobust(y_post, x_post, c=0, h=(bw-5), p=1)
post_results2 <- cbind(post_reg2$coef, post_reg2$z, post_reg2$se, post_reg2$pv, post_reg2$ci)
post_results2

pre_reg3 <- rdrobust(y_pre, x_pre, c=0, h=(bw+5), p=1)
pre_results3 <- cbind(pre_reg3$coef, pre_reg3$z, pre_reg3$se, pre_reg3$pv, pre_reg3$ci)
pre_results3

post_reg3 <- rdrobust(y_post, x_post, c=0, h=(bw+5), p=1)
post_results3 <- cbind(post_reg3$coef, post_reg3$z, post_reg3$se, post_reg3$pv, post_reg3$ci)
post_results3

pre_reg4 <- rdrobust(y_pre, x_pre, c=0, h=(bw+10), p=1)
pre_results4 <- cbind(pre_reg4$coef, pre_reg4$z, pre_reg4$se, pre_reg4$pv, pre_reg4$ci)
pre_results4

post_reg4 <- rdrobust(y_post, x_post, c=0, h=(bw+10), p=1)
post_results4 <- cbind(post_reg4$coef, post_reg4$z, post_reg4$se, post_reg4$pv, post_reg4$ci)
post_results4

# Table 12: Sensitivity of DiRD coefficients to different orders of polynomial
pre_reg <- rdrobust(y_pre, x_pre, c=0, h=bw, p=1)
pre_results <- cbind(pre_reg$coef, pre_reg$z, pre_reg$se, pre_reg$pv, pre_reg$ci)
pre_results

post_reg <- rdrobust(y_post, x_post, c=0, h=bw, p=1)
post_results <- cbind(post_reg$coef, post_reg$z, post_reg$se, post_reg$pv, post_reg$ci)
post_results

pre_poly1 <- rdrobust(y_pre, x_pre, c=0, h=bw, p=2)
pre_results_poly1 <- cbind(pre_poly1$coef, pre_poly1$z, pre_poly1$se, pre_poly1$pv, pre_poly1$ci)
pre_results_poly1

post_poly1 <- rdrobust(y_post, x_post, c=0, h=bw, p=2)
post_results_poly1 <- cbind(post_poly1$coef, post_poly1$z, post_poly1$se, post_poly1$pv, post_poly1$ci)
post_results_poly1

pre_poly2 <- rdrobust(y_pre, x_pre, c=0, h=bw, p=3)
pre_results_poly2 <- cbind(pre_poly2$coef, pre_poly2$z, pre_poly2$se, pre_poly2$pv, pre_poly2$ci)
pre_results_poly2

post_poly2 <- rdrobust(y_post, x_post, c=0, h=bw, p=3)
post_results_poly2 <- cbind(post_poly2$coef, post_poly2$z, post_poly2$se, post_poly2$pv, post_poly2$ci)
post_results_poly2

pre_poly3 <- rdrobust(y_pre, x_pre, c=0, h=bw, p=4)
pre_results_poly3 <- cbind(pre_poly3$coef, pre_poly3$z, pre_poly3$se, pre_poly3$pv, pre_poly3$ci)
pre_results_poly3

post_poly3 <- rdrobust(y_post, x_post, c=0, h=bw, p=4)
post_results_poly3 <- cbind(post_poly3$coef, post_poly3$z, post_poly3$se, post_poly3$pv, post_poly3$ci)
post_results_poly3

# Table 13: Comparison of DiRD estimates with fake cutoffs
# Refer to Figure 16 in Figures.R









