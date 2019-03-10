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
library(stargazer)
library(rdrobust)
library(rddtools)
library(plm)

###############################################################################
# (T) Test study area: Jambi - Tamonarang

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
data <- rdd_data(y17, dist, cutpoint = 0)

mod <- rdd_reg_lm(rdd_object = data, slope = "same", bw = IKbw)
summary(mod)
IKbw <- rdd_bw_ik(data, kernel = c("Triangular"))
IKbw

plot(data, cex = 0.35) 

# Without DiD
y00<- tamonarang.df$treecover2000
tamonarang.df$treecover2017 <- wide$yr_17
y17 <- tamonarang.df$treecover2017
dist <- tamonarang.df$distrelative
rdplot(y00, dist, c=0, y.lim = c(0, 100), col.dots = "grey", cex = 0.5)
rdplot(y17, dist, c=0, y.lim = c(0,100), col.dots = "grey", cex = 0.5)

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

###############################################################################
# (1) Study Area 1: Jambi - Near HT-25 

# Load datasets for analysis
SA1.df <- readRDS("RFiles/StudyAreas/SA1full.Rda")
panel <- readRDS("RFiles/StudyAreas/SA1fullpanel.Rda")

summary(SA1.df)
str(SA1.df)

# There are 116,391 observations in the dataset. 
# The length of the border of interest is 16 km. 

# Calculate optimal bandwidth based on full sample 
opt.bw <- rdbwselect(SA1.df$yr_17, SA1.df$distrelative, all = TRUE)
summary(opt.bw)
# Maximum bandwidth suggested: 229.860 (Using MSE estimates)
# Minimum bandwidth suggested: 77.665 (Using CE estimates)

y <- SA1.df$yr_00
z <- SA1.df$yr_17
x <- SA1.df$distrelative

# Constraining the sample to within 1km of the border to be able to visually inspect functional pattern
SA1.small <- SA1.df %>% filter(distrelative >=-1000 & distrelative <= 1000)
summary(SA1.small)

ys <- SA1.small$yr_00
zs <- SA1.small$yr_17
xs <- SA1.small$distrelative

# Preliminary RD plots: We assume a linear regression to avoid overfitting 
par(mar = c(5.1, 4.1, 4.1, 2.1))
par(mfrow = c(1, 2))
rdplot(ys, xs, c = 0, col.dots = "grey", y.lim = c(90, 100), p = 4, 
       title = "2000", y.label = "% forest cover in 2000", 
       x.label = "Distance to the border (in m)")
rdplot(zs, xs, c = 0, col.dots = "grey", y.lim = c(90, 100), p = 4, 
       title = "2017", y.label = "% forest cover in 2017", 
       x.label = "Distance to the border (in m)")

# McCrary Test to check discontinuities in kernel densities at the border 
library(rdd)
par(mfrow = c(1,1))
DCdensity(xs)
# p-value = 6.453486e-11 << 0.05 - significant discontinuities observed 

lmsmall <- lm(zs ~ xs)
summary(lmsmall)

#library(rddtools)
#data <- rdd_data(zs, xs, cutpoint = 0)

#mod <- rdd_reg_lm(rdd_object = data, slope = "same", bw = 229.860)
#summary(mod)

# Regression output using treecover2017 as dependent variable
# Using default bandwidth calculated by MSE - 256.575 on both sides 
mod1 <- rdrobust(z, x, c = 0)
summary(mod1)

# Using sum of MSE - p >= 4 yields coefficients with the correct sign 
mod2 <- rdrobust(z, x, c = 0, h = c(606.622,  221.638), p = 4, q = 4)
summary(mod2)

# Using CE estimates
mod3 <- rdrobust(z, x, c = 0, h = 143.192)
summary(mod3)


# With binscatter package 
install.packages("binscatter")
library(binscatter)

# With binsreg package 
install.packages("binsreg")
library(binsreg)


# Diff-in-diff estimation 
# Create dummy variable to indicate the time when the treatment started (2008). 
panel$time = ifelse(panel$lossyear1 > 8, 1, 0)

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

#panel$res <- resid(didreg)
#resplot <- ggplot(panel, aes(did, res, colour = factor(in_HD))) + geom_jitter(alpha = 0.5)

# The DiD estimator is = 20.97028. 
# After 2008 and within the HD land title, forest cover increased by 21%. 

# RD estimation 
# Using rddtools
library(rddtools)
data <- rdd_data(y17, x, cutpoint = 0)

mod <- rdd_reg_lm(rdd_object = data, slope = "same", bw = IKbw)
summary(mod)
IKbw <- rdd_bw_ik(data, kernel = c("Triangular"))
IKbw

plot(data, cex = 0.7) 

# Using rdrobust
# Without DiD
rdplot(y0, x, c=0, y.lim = c(0, 100), col.dots = "grey", cex = 0.5)
rdplot(y17, x, c=0, y.lim = c(0,100), col.dots = "grey", cex = 0.5)

rdreg17 <- rdrobust(y17, x, c=0, all = TRUE, p = 1) 
summary(rdreg17)
# The rd treatment effect in 2017 is 0.121 (bias-corrected)
# The true treatment effect is: 0.121 - (20.97028)
effect = 0.121 - 20.97028 
effect
# -20.84928
# As distance to the border increased by 1m, forest cover decreased by 20.8%
# As distance to the border increased by 1km, forest cover decreased by 0.021%

# Bandwidth suggested is very small at 229.860 m on each side. 
# This has resulted in insignificant results. 
# At much larger bandwidths of 1km, the results become much more significant. 
rdreg17b <- rdrobust(y17, x, c=0, all = TRUE, p = 1, h = 1000) 
summary(rdreg17b)

# OLS Fixed Effects - To be conducted later

###############################################################################
# (2) Study Area 2: Jambi Jambi - Lubuk Beringin 

# Load datasets for analysis
SA2.df <- readRDS("RFiles/StudyAreas/SA2full.Rda")
panel <- readRDS("RFiles/StudyAreas/SA2fullpanel.Rda")

summary(SA2.df)
str(SA2.df)

# There are 129,693 observations in the dataset. 
# The length of the border of interest is 28 km. 

# Calculate optimal bandwidth based on full sample 
opt.bw <- rdbwselect(SA2.df$yr_17, SA2.df$distrelative, all = TRUE)
summary(opt.bw)
# Maximum bandwidth suggested: 229.860 (Using MSE estimates)
# Minimum bandwidth suggested: 77.665 (Using CE estimates)

y <- SA2.df$yr_00
z <- SA2.df$yr_17
x <- SA2.df$distrelative

# Constraining the sample to within 1km of the border to be able to visually inspect functional pattern
SA2.small <- SA2.df %>% filter(distrelative >=-1000 & distrelative <= 1000)
summary(SA2.small)

ys <- SA2.small$yr_00
zs <- SA2.small$yr_17
xs <- SA2.small$distrelative

# Preliminary RD plots: We assume a linear regression to avoid overfitting 
par(mar = c(5.1, 4.1, 4.1, 2.1))
par(mfrow = c(1, 2))
rdplot(ys, xs, c = 0, col.dots = "grey", y.lim = c(90, 100), p = 1, 
       title = "2000", y.label = "% forest cover in 2000", 
       x.label = "Distance to the border (in m)")
rdplot(zs, xs, c = 0, col.dots = "grey", y.lim = c(90, 100), p = 1, 
       title = "2017", y.label = "% forest cover in 2017", 
       x.label = "Distance to the border (in m)")

# McCrary Test to check discontinuities in kernel densities at the border 
library(rdd)
par(mfrow = c(1,1))
DCdensity(xs)
# p-value = 4.011762e-156 << 0.05 - significant discontinuities observed 

# Simple OLS regression of distance to border on forest cover loss
lmsmall <- lm(zs ~ xs)
summary(lmsmall)

#library(rddtools)
#data <- rdd_data(zs, xs, cutpoint = 0)

#mod <- rdd_reg_lm(rdd_object = data, slope = "same", bw = 229.860)
#summary(mod)

# Regression output using treecover2017 as dependent variable
# Using default bandwidth calculated by MSE - 586.518 on both sides 
mod1 <- rdrobust(z, x, c = 0)
summary(mod1)

# Using sum of MSE
mod2 <- rdrobust(z, x, c = 0, h = 586.518)
summary(mod2)

# Using CE estimates
mod3 <- rdrobust(z, x, c = 0, h = 159.760)
summary(mod3)


# With binscatter package 
install.packages("binscatter")
library(binscatter)

# With binsreg package 
install.packages("binsreg")
library(binsreg)

# Diff-in-diff estimation 
# Create dummy variable to indicate the time when the treatment started (2008). 
panel$time = ifelse(panel$lossyear1 > 8, 1, 0)

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

#panel$res <- resid(didreg)
#resplot <- ggplot(panel, aes(did, res, colour = factor(in_HD))) + geom_jitter(alpha = 0.5)

# The DiD estimator is = 20.97028. 
# After 2008 and within the HD land title, forest cover increased by 21%. 

# RD estimation 
# Using rddtools
library(rddtools)
data <- rdd_data(y17, x, cutpoint = 0)

mod <- rdd_reg_lm(rdd_object = data, slope = "same", bw = IKbw)
summary(mod)
IKbw <- rdd_bw_ik(data, kernel = c("Triangular"))
IKbw

plot(data, cex = 0.7) 

# Using rdrobust
# Without DiD
rdplot(y0, x, c=0, y.lim = c(0, 100), col.dots = "grey", cex = 0.5)
rdplot(y17, x, c=0, y.lim = c(0,100), col.dots = "grey", cex = 0.5)

rdreg17 <- rdrobust(y17, x, c=0, all = TRUE, p = 1) 
summary(rdreg17)
# The rd treatment effect in 2017 is 0.121 (bias-corrected)
# The true treatment effect is: 0.121 - (20.97028)
effect = 0.121 - 20.97028 
effect
# -20.84928
# As distance to the border increased by 1m, forest cover decreased by 20.8%
# As distance to the border increased by 1km, forest cover decreased by 0.021%

# Bandwidth suggested is very small at 229.860 m on each side. 
# This has resulted in insignificant results. 
# At much larger bandwidths of 1km, the results become much more significant. 
rdreg17b <- rdrobust(y17, x, c=0, all = TRUE, p = 1, h = 1000) 
summary(rdreg17b)

# OLS Fixed Effects - To be conducted later

###############################################################################
# (3) Study Area 3: Jambi - Tamonarang

# Load datasets for analysis
SA3.df <- readRDS("RFiles/StudyAreas/SA3full.Rda")
panel <- readRDS("RFiles/StudyAreas/SA3fullpanel.Rda")

summary(SA3.df)
str(SA3.df)

# There are 90,228 observations in the dataset. 
# The length of the border of interest is 5 km. 

# Calculate optimal bandwidth based on full sample 
opt.bw <- rdbwselect(SA3.df$yr_17, SA3.df$distrelative, all = TRUE)
summary(opt.bw)
# Maximum bandwidth suggested: 900.206 (L) and 601.725 (R) (Using MSE estimates)
# Minimum bandwidth suggested: 351.348 (Using CE estimates)

y <- SA3.df$yr_00
z <- SA3.df$yr_17
x <- SA3.df$distrelative

# Constraining the sample to within 1km of the border to be able to visually inspect functional pattern
SA3.small <- SA3.df %>% filter(distrelative >=-1000 & distrelative <= 1000)
summary(SA3.small)

ys <- SA3.small$yr_00
zs <- SA3.small$yr_17
xs <- SA3.small$distrelative

# Preliminary RD plots: We assume a linear regression to avoid overfitting 
par(mar = c(5.1, 4.1, 4.1, 2.1))
par(mfrow = c(1, 2))
rdplot(ys, xs, c = 0, col.dots = "grey", y.lim = c(40, 90), p = 1, 
       title = "2000", y.label = "% forest cover in 2000", 
       x.label = "Distance to the border (in m)")
rdplot(zs, xs, c = 0, col.dots = "grey", y.lim = c(40, 90), p = 1, 
       title = "2017", y.label = "% forest cover in 2017", 
       x.label = "Distance to the border (in m)")

# McCrary Test to check discontinuities in kernel densities at the border 
library(rdd)
par(mfrow = c(1,1))
DCdensity(xs)
# p-value = 0.08135458 > 0.05 - insignificant discontinuities observed 

# Simple OLS regression of distance to border on forest cover loss
lmsmall <- lm(zs ~ xs)
summary(lmsmall)

#library(rddtools)
#data <- rdd_data(zs, xs, cutpoint = 0)

#mod <- rdd_reg_lm(rdd_object = data, slope = "same", bw = 229.860)
#summary(mod)

# Regression output using treecover2017 as dependent variable
# Using default bandwidth calculated by MSE - 621.591 on both sides 
mod1 <- rdrobust(z, x, c = 0)
summary(mod1)

# Using sum of MSE (900.206 - L, 601.725 - R)
mod2 <- rdrobust(z, x, c = 0, h = c(900.206, 601.725))
summary(mod2)

# Using CE estimates
mod3 <- rdrobust(z, x, c = 0, h = 479.430)
summary(mod3)


# With binscatter package 
install.packages("binscatter")
library(binscatter)

# With binsreg package 
install.packages("binsreg")
library(binsreg)

# Diff-in-diff estimation 
# Create dummy variable to indicate the time when the treatment started (2008). 
panel$time = ifelse(panel$lossyear1 > 8, 1, 0)

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

#panel$res <- resid(didreg)
#resplot <- ggplot(panel, aes(did, res, colour = factor(in_HD))) + geom_jitter(alpha = 0.5)

# The DiD estimator is = 20.97028. 
# After 2008 and within the HD land title, forest cover increased by 21%. 

# RD estimation 
# Using rddtools
library(rddtools)
data <- rdd_data(y17, x, cutpoint = 0)

mod <- rdd_reg_lm(rdd_object = data, slope = "same", bw = IKbw)
summary(mod)
IKbw <- rdd_bw_ik(data, kernel = c("Triangular"))
IKbw

plot(data, cex = 0.7) 

# Using rdrobust
# Without DiD
rdplot(y0, x, c=0, y.lim = c(0, 100), col.dots = "grey", cex = 0.5)
rdplot(y17, x, c=0, y.lim = c(0,100), col.dots = "grey", cex = 0.5)

rdreg17 <- rdrobust(y17, x, c=0, all = TRUE, p = 1) 
summary(rdreg17)
# The rd treatment effect in 2017 is 0.121 (bias-corrected)
# The true treatment effect is: 0.121 - (20.97028)
effect = 0.121 - 20.97028 
effect
# -20.84928
# As distance to the border increased by 1m, forest cover decreased by 20.8%
# As distance to the border increased by 1km, forest cover decreased by 0.021%

# Bandwidth suggested is very small at 229.860 m on each side. 
# This has resulted in insignificant results. 
# At much larger bandwidths of 1km, the results become much more significant. 
rdreg17b <- rdrobust(y17, x, c=0, all = TRUE, p = 1, h = 1000) 
summary(rdreg17b)

# OLS Fixed Effects - To be conducted later

#############################################################################################
# Pooled regressions 

# Jambi region only - HD on HL 
# RD regression without normalisation of distrelative variable 

# Create annual rate of deforestation between 2000 and 2017 
# Positive values mean deforestation 
jambi <- readRDS("RFiles/StudyAreas/pooledJambi.Rda")
jambi$rate <- -(jambi$yr_17 - jambi$yr_00)/17

# Create diff-in-diff interaction variable 
# Create dummy variable to indicate the time when the treatment started (2008). 
jambi$time = ifelse(jambi$lossyear1 > 8, 1, 0)

# Create d-i-d interaction (time*treatment)
jambi$did = jambi$time * jambi$in_HD

# Preliminary RD plot 
y <- jambi$rate
x <- jambi$distrelative
d <- jambi$did
z <- jambi$yr_17

rdplot(y, x, c = 0, p = 4, covs = d, col.dots = rgb(205/255, 205/255, 193/255, alpha = 0.5), 
       lwd = 2, y.lim = c(-2, 6), title = "Pooled RD Plot - Jambi", 
       x.label = "Distance to the border (in m)",
       y.label = "Annual rate of deforestation (%)")
rdplot(y, x, c = 0, p = 1, covs = d, col.dots = rgb(205/255, 205/255, 193/255, alpha = 0.5), 
       lwd = 2, y.lim = c(-2, 6), title = "Pooled RD Plot - Jambi", 
       x.label = "Distance to the border (in m)",
       y.label = "Annual rate of deforestation (%)")

# Constraining the sample to within 1km of the border to be able to visually inspect functional pattern
jambi.small <- jambi %>% filter(distrelative >=-1000 & distrelative <= 1000)
summary(jambi.small)

ys <- jambi.small$rate
zs <- jambi.small$yr_17
xs <- jambi.small$distrelative
ds <- jambi.small$did

rdplot(ys, xs, c = 0, p = 1, covs = ds, col.dots = rgb(205/255, 205/255, 193/255, alpha = 0.5), 
       lwd = 2, y.lim = c(-0.5, 1), title = "Pooled RD Plot - Jambi", 
       x.label = "Distance to the border (in m)",
       y.label = "Annual rate of deforestation (%)")

# Calculate bandwidth
opt.bw <- rdbwselect(y, x, all = TRUE)
summary(opt.bw)

# Estimate RD regression 
rd.mod1 = rdrobust(y, x, c = 0, p = 1, covs = d, h = c(734.764, 504.612), all = TRUE)
summary(rd.mod1)

rd.mod2 = rdrobust(y, x, c = 0, p = 1, covs = d, h = 325.745, all = TRUE)
summary(rd.mod2)

# RDplot with normalisation of variables 
library(standardize)
jambi$n_rate <- scale_by(rate ~ study_area, jambi)
jambi$n_dist <- scale_by(distrelative ~ study_area, jambi)
summary(jambi$n_rate)
summary(jambi$n_dist)

mean(jambi$n_dist)
sd(jambi$n_dist)

scaledy <- jambi$n_rate
scaledx <- jambi$n_dist
d <- jambi$did

rdplot(scaledy, scaledx, c = 0, p = 4, covs = d, col.dots = "grey", y.lim = c(-2, 6))

# 3 areas in Jambi and 1 area in Sumatra South - HD on HL
# RD regression without normalisation of distrelative variable 

# Create annual rate of deforestation between 2000 and 2017 
# Positive values mean deforestation 
pooled <- readRDS("RFiles/StudyAreas/pooled.Rda")
pooled$rate <- -(pooled$yr_17 - pooled$yr_00)/17

# Create diff-in-diff interaction variable 
# Create dummy variable to indicate the time when the treatment started (2008). 
pooled$time = ifelse(pooled$lossyear1 > 8, 1, 0)

# Create d-i-d interaction (time*treatment)
pooled$did = pooled$time * pooled$in_HD

# Preliminary RD plot 
y <- pooled$rate
x <- pooled$distrelative
d <- pooled$did
z <- pooled$yr_17

rdplot(y, x, c = 0, p = 4, covs = d, col.dots = rgb(205/255, 205/255, 193/255, alpha = 0.5), 
       lwd = 2, y.lim = c(-2, 6), title = "Pooled RD Plot", x.label = "Distance to the border (in m)",
       y.label = "Annual rate of deforestation (%)")
rdplot(y, x, c = 0, p = 1, covs = d, col.dots = rgb(205/255, 205/255, 193/255, alpha = 0.5), 
       lwd = 2, y.lim = c(-2, 6), title = "Pooled RD Plot", x.label = "Distance to the border (in m)",
       y.label = "Annual rate of deforestation (%)")

# Constraining the sample to within 1km of the border to be able to visually inspect functional pattern
pooled.small <- pooled %>% filter(distrelative >=-1000 & distrelative <= 1000)
summary(pooled.small)

ys <- pooled.small$rate
zs <- pooled.small$yr_17
xs <- pooled.small$distrelative
ds <- pooled.small$did

rdplot(ys, xs, c = 0, p = 1, covs = ds, col.dots = rgb(205/255, 205/255, 193/255, alpha = 0.5), 
       lwd = 2, y.lim = c(-0.5, 1), title = "Pooled RD Plot", x.label = "Distance to the border (in m)",
       y.label = "Annual rate of deforestation (%)")

# Calculate bandwidth
opt.bw <- rdbwselect(y, x, all = TRUE)
summary(opt.bw)

# Estimate RD regression 
rd.mod3 = rdrobust(y, x, c = 0, p = 1, covs = d, h = c(1887.622, 387.859), all = TRUE)
summary(rd.mod3)

rd.mod4 = rdrobust(y, x, c = 0, p = 1, covs = d, h = 234.830, all = TRUE)
summary(rd.mod4)

