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
# Data visualisation tools
library(ggthemes)
library(gridExtra)
library(ggpubr)
library(cowplot)
library(GGally)
library(ggridges)
library(RColorBrewer)
library(wesanderson)
library(ggplot2)

# Load datasets
pooled <- readRDS("RFiles/StudyAreas/pooled_reg.Rda")
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
covs_df <- readRDS("RFiles/StudyAreas/covs.Rda")

# Figure 1: Tree cover loss in Indonesia
# Source: Adapted from GFW (2018). 
gfw <- read_excel("Loss_GFWdata.xlsx")

plot <- ggplot(gfw, aes(x=Year, y=Loss)) + 
  geom_line() + labs(y="Tree cover loss in million hectares (Mha)") + 
  theme_pubclean()
plot

# Figure 4: Preliminary year-on-year RD plots of forest cover
pooled <- readRDS("RFiles/StudyAreas/pooled_reg.Rda")
pool <- pooled %>% filter(distrelative >=-5000 & distrelative <= 5000)
x <- pool$distrelative

par(oma=c(1,3,0,0),mar=c(3,3,2,2),mfrow=c(6,3))
rdplot(pool$yr_00, x, c = 0, p = 4, nbins=100, 
       y.lim = c(70,100), cex=0.5, col.dots="darkgrey", type.dots=16, 
       title = "2000", 
       x.label = "",y.label = "")

rdplot(pool$yr_1, x, c = 0, p = 4, nbins=100, 
       y.lim = c(70,100), cex=0.5, col.dots="darkgrey", type.dots=16, 
       title = "2001", 
       x.label = "",y.label = "")

rdplot(pool$yr_2, x, c = 0, p = 4, nbins=100, 
       y.lim = c(70,100), cex=0.5, col.dots="darkgrey", type.dots=16, 
       title = "2002", 
       x.label = "",y.label = "")

rdplot(pool$yr_3, x, c = 0, p = 4, nbins=100, 
       y.lim = c(70,100), cex=0.5, col.dots="darkgrey", type.dots=16, 
       title = "2003", 
       x.label = "",y.label = "")

rdplot(pool$yr_4, x, c = 0, p = 4, nbins=100, 
       y.lim = c(70,100), cex=0.5, col.dots="darkgrey", type.dots=16, 
       title = "2004", 
       x.label = "",y.label = "")

rdplot(pool$yr_5, x, c = 0, p = 4, nbins=100, 
       y.lim = c(70,100), cex=0.5, col.dots="darkgrey", type.dots=16, 
       title = "2005", 
       x.label = "",y.label = "")

rdplot(pool$yr_6, x, c = 0, p = 4, nbins=100, 
       y.lim = c(70,100), cex=0.5, col.dots="darkgrey", type.dots=16, 
       title = "2006", 
       x.label = "",y.label = "")

rdplot(pool$yr_7, x, c = 0, p = 4, nbins=100, 
       y.lim = c(70,100), cex=0.5, col.dots="darkgrey", type.dots=16, 
       title = "2007", 
       x.label = "",y.label = "")

rdplot(pool$yr_8, x, c = 0, p = 4, nbins=100, 
       y.lim = c(70,100), cex=0.5, col.dots="darkgrey", type.dots=16, 
       title = "2008", 
       x.label = "",y.label = "")

rdplot(pool$yr_9, x, c = 0, p = 4, nbins=100, 
       y.lim = c(70,100), cex=0.5, col.dots="darkgrey", type.dots=16, 
       title = "2009", 
       x.label = "",y.label = "")

rdplot(pool$yr_10, x, c = 0, p = 4, nbins=100, 
       y.lim = c(70,100), cex=0.5, col.dots="darkgrey", type.dots=16, 
       title = "2010", 
       x.label = "",y.label = "")

rdplot(pool$yr_11, x, c = 0, p = 4, nbins=100, 
       y.lim = c(70,100), cex=0.5, col.dots="darkgrey", type.dots=16, 
       title = "2011", 
       x.label = "",y.label = "")

rdplot(pool$yr_12, x, c = 0, p = 4, nbins=100, 
       y.lim = c(70,100), cex=0.5, col.dots="darkgrey", type.dots=16, 
       title = "2012", 
       x.label = "",y.label = "")

rdplot(pool$yr_13, x, c = 0, p = 4, nbins=100, 
       y.lim = c(70,100), cex=0.5, col.dots="darkgrey", type.dots=16, 
       title = "2013", 
       x.label = "",y.label = "")

rdplot(pool$yr_14, x, c = 0, p = 4, nbins=100, 
       y.lim = c(70,100), cex=0.5, col.dots="darkgrey", type.dots=16, 
       title = "2014", 
       x.label = "",y.label = "")

rdplot(pool$yr_15, x, c = 0, p = 4, nbins=100, 
       y.lim = c(70,100), cex=0.5, col.dots="darkgrey", type.dots=16, 
       title = "2015", 
       x.label = "",y.label = "")

rdplot(pool$yr_16, x, c = 0, p = 4, nbins=100, 
       y.lim = c(70,100), cex=0.5, col.dots="darkgrey", type.dots=16, 
       title = "2016", 
       x.label = "",y.label = "")

rdplot(pool$yr_17, x, c = 0, p = 4, nbins=100, 
       y.lim = c(70,100), cex=0.5, col.dots="darkgrey", type.dots=16, 
       title = "2017", 
       x.label = "",y.label = "")
mtext(text="Distance to the border (in m)",side=1,line=0,outer=TRUE)
mtext(text="% forest cover",side=2,line=0,outer=TRUE)

# Figure 5: Preliminary RD plots with different bin widths 
sample_1 <- pooled %>% filter(distrelative >=-304.836 & distrelative <= 304.836)
y_prelim <- sample_1$rate
x_prelim <- sample_1$distrelative


par(oma=c(3,3,0,0),mar=c(3,3,2,2),mfrow=c(2,2))
prelim_plot1 <- rdplot(y_prelim, x_prelim, c=0, p=4, binselect="esmv", y.lim=c(-0.2,1.2),
                       cex=0.5, type.dots=16,
                       lwd = 5,
                       title = "Automatically-selected bins [817; 815]", 
                       x.label = "", 
                       y.label = "")


prelim_plot2 <- rdplot(y_prelim, x_prelim, c=0, p=4, nbins=400, y.lim=c(-0.2,1.2),
                       cex=0.5, type.dots=16,
                       lwd = 5,
                       title = "400 bins", 
                       x.label = "", 
                       y.label = "")


prelim_plot3 <- rdplot(y_prelim, x_prelim, c=0, p=4, nbins=100, y.lim=c(-0.2,1.2),
                       cex=0.5, type.dots=16,
                       lwd = 5,
                       title = "100 bins", 
                       x.label = "", 
                       y.label = "")

prelim_plot4 <- rdplot(y_prelim, x_prelim, c=0, p=4, nbins=50, y.lim=c(-0.2,1.2),
                       cex=0.5,  type.dots=16,
                       lwd = 5,
                       title = "50 bins", 
                       x.label = "", 
                       y.label = "")
mtext(text="Distance to the border (in m)",side=1,line=0,outer=TRUE)
mtext(text="Average annual rate of deforestation (%)",side=2,line=0,outer=TRUE)


# Figures 6 & 7
## Figure 7: Difference in discontinuity at border 
pooled <- readRDS("RFiles/StudyAreas/pooled_reg.Rda")

# Create variables for estimation 
x <- pooled$distrelative
y_pre <- pooled$prerate
x_pre <- pooled$distrelative

y_post <- pooled$postrate
x_post <- pooled$distrelative

bw = 304.836 
bw1 = 623.523

# rdrobust 
pre_reg <- rdrobust(y_pre, x_pre, c=0, h=bw, p=1)
pre_results <- cbind(pre_reg$coef, pre_reg$z, pre_reg$se, pre_reg$pv, pre_reg$ci)
pre_results

post_reg <- rdrobust(y_post, x_post, c=0, h=bw, p=1)
post_results <- cbind(post_reg$coef, post_reg$z, post_reg$se, post_reg$pv, post_reg$ci)
post_results


pre_reg_IK <- rdrobust(y_pre, x_pre, c=0, h=bw1, p=1)
pre_results_IK <- cbind(pre_reg_IK$coef, pre_reg_IK$z, pre_reg_IK$se, pre_reg_IK$pv, pre_reg_IK$ci)
pre_results_IK

post_reg_IK <- rdrobust(y_post, x_post, c=0, h=bw1, p=1)
post_results_IK <- cbind(post_reg_IK$coef, post_reg_IK$z, post_reg_IK$se, post_reg_IK$pv, post_reg_IK$ci)
post_results_IK

par(op)
par(oma=c(3,3,0,0),mar=c(3,3,2,2),mfrow=c(1,2))
pre_plot <- rdplot(y_pre[abs(x)<=304.836], x_pre[abs(x)<=304.836], c=0, p=1,
                   binselect="es", ci=95, y.lim = c(0,0.8), 
                   cex=0.5,  type.dots=16, 
                   title = "2000-2008", 
                   x.label = "",
                   y.label = "")

post_plot <- rdplot(y_post[abs(x)<=304.836], x_post[abs(x)<=304.836], c=0, p=1,
                    binselect="es", ci=95,y.lim = c(0,0.8), 
                    cex=0.5,  type.dots=16, 
                    title = "2009-2017", 
                    x.label = "",
                    y.label = "")
mtext(text="Distance to the border (in m); Bandwidth = 305 m",side=1,line=0,outer=TRUE)
mtext(text="Average annual rate of forest cover loss (%)",side=2,line=0,outer=TRUE)

## Figure 6: Pre-treatment and Post-treatment Estimates with 95% c.i. 
pre.tidy <- as.data.frame(pre_results)
pre.tidy$Estimate <- rownames(robust.tidy[,0])
pre.tidy <- pre.tidy[,c(1:7)]
names(pre.tidy) <- c("Coeff", "z", "SE", "p", "ci.lower", "ci.upper", "Estimate")

ggplot(pre.tidy, mapping = aes(x =Estimate, y = Coeff, 
                               ymin = ci.lower, ymax = ci.upper)) + 
  geom_pointrange() + 
  coord_flip() + 
  labs(x="", y="RD Estimate")

pre.tidy1 <- as_tibble(pre_results, rownames="Estimate")

coef_pre <- pre.tidy1 %>% 
  mutate(Estimate = fct_reorder(Estimate, desc(Coeff))) %>% 
  ggplot(mapping = aes(x =Estimate, y = Coeff, ymin = `CI Lower`, 
                       ymax = `CI Upper`)) + 
  geom_errorbar(width=0.1, colour = "blue") + 
  geom_point() + 
  coord_flip() + 
  labs(x="", y="") + 
  geom_hline(yintercept = 0, colour = "grey60", linetype = 2) + 
  expand_limits(y = c(-0.2, 1.5)) + theme_bw()

coef_pre

post.tidy <- as_tibble(post_results, rownames="Estimate")

coef_post <- post.tidy %>% 
  mutate(Estimate = fct_reorder(Estimate, Coeff)) %>% 
  ggplot(mapping = aes(x =Estimate, y = Coeff, ymin = `CI Lower`, 
                       ymax = `CI Upper`)) + 
  geom_errorbar(width=0.1, colour = "blue") + 
  geom_point() + coord_flip() + 
  labs(x="", y="")+ 
  geom_hline(yintercept = 0, colour = "grey60", linetype = 2) + 
  expand_limits(y = c(0, 1.5)) + theme_bw()

coef_post

robust.tidy2 <- robust.tidy1 %>% 
  dplyr::rename(term = Estimate, estimate=Coeff, conf.low=`CI Lower`, 
                conf.high=`CI Upper`)
robust.tidy2


ggcoef(robust.tidy2, mapping = aes_string(y = "term", x = "estimate"), conf.int = TRUE,
       conf.level = 0.95, exponentiate = FALSE, exclude_intercept = FALSE,
       vline = TRUE, vline_intercept = "auto", vline_color = "gray50",
       vline_linetype = "dotted", vline_size = 1, errorbar_color = "gray25",
       errorbar_height = 0.25, errorbar_linetype = "solid", errorbar_size = 0.5)


pre.tidy1$Model <- c("Pre-treatment (2000-2008)")
post.tidy$Model <- c("Post-treatment (2009-2017)")

pptidy <- rbind(post.tidy, pre.tidy1)

pd <- position_dodge(width=0)
coefplotfinal <- pptidy %>% 
  mutate(Estimate = fct_reorder(Estimate, desc(Coeff))) %>% 
  ggplot(aes(x=Estimate,y=Coeff)) + coord_flip() + 
  geom_pointrange(aes(ymin=`CI Lower`,ymax=`CI Upper`),
                  size=0.3, position=pd) + 
  geom_hline(yintercept = 0, colour = "grey60", linetype = 2) + 
  expand_limits(y = c(-0.2, 0.4)) + 
  scale_y_continuous(name=NULL,breaks=c(-0.2,-0.1,0,0.1,0.2,0.3,0.4)) + theme_bw()

coefplotfinal <- coefplotfinal + 
  labs(x="Type of confidence interval", y="Size of coefficient")+ facet_grid(Model~.)
coefplotfinal

par(mar=c(0,0,0,0),mfrow=c(1,1))
coefplotfinal

# Figure 8: Density tests for running variable 
dens <- rddensity(x, c=0, p=1, h=305, all=TRUE)
densplot <- rdplotdensity(dens, x, type="both", 
                          xlabel="Distance to the border (m)", ylabel="Density")
densplot1 <- densplot$Estplot
hist <- ggplot(pooled, aes(distrelative)) + 
  geom_histogram(bins=60, colour="darkcyan", fill="darkcyan", alpha=0.5) + 
  geom_vline(xintercept=0, linetype="solid", colour="black") + 
  labs(x="Distance to the border (m)", y="Count") + 
  theme_tufte(base_family="Arial", base_size=10)

denstest <- densplot1 + 
  geom_vline(xintercept=0, linetype="solid", colour="black") + 
  theme_classic() + guides(fill = FALSE, color = FALSE, linetype = FALSE, shape = FALSE)

combinedplot <- plot_grid(denstest, hist)

# Figure 11: Distribution of land titles by province 
type1 <- data.frame(id=1:dim(SA1.df), 
                    study_area=SA1.df$study_area, 
                    province=SA1.df$province)
type1$SFP <- ifelse(SA1.df$in_HD==TRUE, "HD", "State")
head(type1)

type2 <- data.frame(id=1:dim(SA2.df), 
                    study_area=SA2.df$study_area, 
                    province=SA2.df$province)
type2$SFP <- ifelse(SA2.df$in_HD==TRUE, "HD", "State")
head(type2)

type3 <- data.frame(id=1:dim(SA3.df), 
                    study_area=SA3.df$study_area, 
                    province=SA3.df$province)
type3$SFP <- ifelse(SA3.df$in_HD==TRUE, "HD", "State")
head(type3)

type4 <- data.frame(id=1:dim(SA4.df), 
                    study_area=SA4.df$study_area, 
                    province=SA4.df$province)
type4$SFP <- ifelse(SA4.df$in_HD==TRUE, "HD", "State")
head(type4)

type5 <- data.frame(id=1:dim(SA5.df), 
                    study_area=SA5.df$study_area, 
                    province=SA5.df$province)
type5$SFP <- ifelse(SA5.df$in_HD==TRUE, "HD", "State")
head(type5)

type6 <- data.frame(id=1:dim(SA6.df), 
                    study_area=SA6.df$study_area, 
                    province=SA6.df$province)
type6$SFP <- ifelse(SA6.df$in_HD==TRUE, "HD", "State")
head(type6)

type7 <- data.frame(id=1:dim(SA7.df), 
                    study_area=SA7.df$study_area, 
                    province=SA7.df$province)
type7$SFP <- ifelse(SA7.df$in_HD==TRUE, "HD", "State")
head(type7)

type8 <- data.frame(id=1:dim(SA8.df), 
                    study_area=SA8.df$study_area, 
                    province=SA8.df$province)
type8$SFP <- ifelse(SA8.df$in_HD==TRUE, "HKm", "State")
head(type8)

type9 <- data.frame(id=1:dim(SA9.df), 
                    study_area=SA9.df$study_area, 
                    province=SA9.df$province)
type9$SFP <- ifelse(SA9.df$in_HD==TRUE, "HKm", "State")
head(type9)

type10 <- data.frame(id=1:dim(SA10.df), 
                     study_area=SA10.df$study_area, 
                     province=SA10.df$province)
type10$SFP <- ifelse(SA10.df$in_HD==TRUE, "HKm", "State")
head(type10)

type11 <- data.frame(id=1:dim(SA11.df), 
                     study_area=SA11.df$study_area, 
                     province=SA11.df$province)
type11$SFP <- ifelse(SA11.df$in_HD==TRUE, "HD", "State")
head(type11)

type.df <- bind_rows(type1, type2, type3, type4, type5, type6, type7,
                     type8, type9, type10, type11)

type.df <- type.df[,-1] %>% arrange(study_area)
head(type.df)

SFPplot <- ggplot(type.df, aes(x=SFP)) + 
  geom_bar(aes(fill=province)) + theme_pubclean(base_size=10) +
  labs(x="Land tenure title", y="")

SFPplot + facet_wrap(~province) + theme(legend.position = "none")

# Figure 12: Frequency distributions by province 
desc.test <- descriptive %>% select(distrelative, SFP, rate, prerate, postrate)
desc.test$SFPID <- pooled$SFPID
desc.test$province <- pooled$province

ggplot(desc.test, aes(distrelative, fill = SFP)) + geom_density(alpha=0.5)

theme_set(theme_ridges())
ridge1 <- ggplot(desc.test, aes(x = distrelative, y = province, fill=SFP)) +
  geom_density_ridges(aes(fill = SFP), scale=0.8) + 
  scale_fill_manual(values=wes_palette("Darjeeling1")) + 
  theme_tufte(base_family="Arial", base_size=10) + 
  labs(x="Relative distance from border (m)", y="") 

ridge2 <- ggplot(desc.test, aes(x = rate, y = province)) +
  geom_density_ridges(aes(fill = SFP), scale=1) + 
  scale_fill_manual(values=wes_palette("Darjeeling1")) +
  theme_tufte(base_family="Arial", base_size=10) + 
  labs(x="Rate of forest cover loss (%) ('00-'17)", y="") 

ridge3 <- ggplot(desc.test, aes(x = prerate, y = province)) +
  geom_density_ridges(aes(fill = SFP), scale=1) + 
  scale_fill_manual(values=wes_palette("Darjeeling1")) +
  theme_tufte(base_family="Arial", base_size=10) + 
  labs(x="Rate of forest cover loss (%) ('00-'08)", y="") 

ridge4 <- ggplot(desc.test, aes(x = postrate, y = province)) +
  geom_density_ridges(aes(fill = SFP), scale=1) + 
  scale_fill_manual(values=wes_palette("Darjeeling1")) +
  theme_tufte(base_family="Arial", base_size=10) + 
  labs(x="Rate of forest cover loss (%) ('09-'17)", y="") 

plot_grid(ridge1, ridge2, ridge3, ridge4, labels=c('A', 'B', 'C', 'D'), ncol=1)

# Figure 13: Year-on-year RD Estimates
## Prepare data
# Year 2001
pooled_01 <- pooled
pooled_01$rate <- -((pooled$yr_1 - pooled$yr_00)/1)

# Year 2002
pooled_02 <- pooled
pooled_02$rate <- -((pooled$yr_2 - pooled$yr_1)/1)

# Year 2004
pooled_03 <- pooled
pooled_03$rate <- -((pooled$yr_3 - pooled$yr_2)/1)

# Year 2004
pooled_04 <- pooled
pooled_04$rate <- -((pooled$yr_4 - pooled$yr_3)/1)

# Year 2005
pooled_05 <- pooled
pooled_05$rate <- -((pooled$yr_5 - pooled$yr_4)/1)

# Year 2006
pooled_06 <- pooled
pooled_06$rate <- -((pooled$yr_6 - pooled$yr_5)/1)

# Year 2007
pooled_07 <- pooled
pooled_07$rate <- -((pooled$yr_7 - pooled$yr_6)/1)

# Year 2008
pooled_08 <- pooled
pooled_08$rate <- -((pooled$yr_8 - pooled$yr_7)/1)

# Year 2009
pooled_09 <- pooled
pooled_09$rate <- -((pooled$yr_9 - pooled$yr_8)/1)

# Year 2010
pooled_10 <- pooled
pooled_10$rate <- -((pooled$yr_10 - pooled$yr_9)/1)

# Year 2011
pooled_11 <- pooled
pooled_11$rate <- -((pooled$yr_11 - pooled$yr_10)/1)

# Year 2012
pooled_12 <- pooled
pooled_12$rate <- -((pooled$yr_12 - pooled$yr_11)/1)

# Year 2013
pooled_13 <- pooled
pooled_13$rate <- -((pooled$yr_13 - pooled$yr_12)/1)

# Year 2014
pooled_14 <- pooled
pooled_14$rate <- -((pooled$yr_14 - pooled$yr_13)/1)

# Year 2015
pooled_15 <- pooled
pooled_15$rate <- -((pooled$yr_15 - pooled$yr_14)/1)

# Year 2016
pooled_16 <- pooled
pooled_16$rate <- -((pooled$yr_16 - pooled$yr_15)/1)

# Year 2017
pooled_17 <- pooled
pooled_17$rate <- -((pooled$yr_17 - pooled$yr_16)/1)

#2001
# Create variables
y <- pooled_01$rate
x <- pooled_01$distrelative

# 2001
# Bandwidth = 304.836
rd_yr1 <- rdrobust(y, x, c=0, p=1, h=304.836)
yr1_conv = rd_yr1$coef[1,]
yr1_convci = rd_yr1$ci[1,]
yr1_bias = rd_yr1$coef[2,]
yr1_biasci = rd_yr1$ci[2,]
yr1_rob = rd_yr1$coef[3,]
yr1_robci = rd_yr1$ci[3,]

# 2002
# Create variables
y <- pooled_02$rate
x <- pooled_02$distrelative

# 2002
# Bandwidth = 304.836
rd_yr2 <- rdrobust(y, x, p=1, c=0, h=304.836)
yr2_conv = rd_yr2$coef[1,]
yr2_convci = rd_yr2$ci[1,]
yr2_bias = rd_yr2$coef[2,]
yr2_biasci = rd_yr2$ci[2,]
yr2_rob = rd_yr2$coef[3,]
yr2_robci = rd_yr2$ci[3,]

# 2003
# Create variables
y <- pooled_03$rate
x <- pooled_03$distrelative

# 2003
# Bandwidth = 304.836
rd_yr3 <- rdrobust(y, x, c=0, p=1, h=304.836)
yr3_conv = rd_yr3$coef[1,]
yr3_convci = rd_yr3$ci[1,]
yr3_bias = rd_yr3$coef[2,]
yr3_biasci = rd_yr3$ci[2,]
yr3_rob = rd_yr3$coef[3,]
yr3_robci = rd_yr3$ci[3,]

# 2004
# Create variables
y <- pooled_04$rate
x <- pooled_04$distrelative

# 2004
# Bandwidth = 304.836
rd_yr4 <- rdrobust(y, x, c=0, p=1, h=304.836)
yr4_conv = rd_yr4$coef[1,]
yr4_convci = rd_yr4$ci[1,]
yr4_bias = rd_yr4$coef[2,]
yr4_biasci = rd_yr4$ci[2,]
yr4_rob = rd_yr4$coef[3,]
yr4_robci = rd_yr4$ci[3,]

# 2005
# Create variables
y <- pooled_05$rate
x <- pooled_05$distrelative

# 2005
# Bandwidth = 304.836
rd_yr5 <- rdrobust(y, x, c=0, p=1, h=304.836)
yr5_conv = rd_yr5$coef[1,]
yr5_convci = rd_yr5$ci[1,]
yr5_bias = rd_yr5$coef[2,]
yr5_biasci = rd_yr5$ci[2,]
yr5_rob = rd_yr5$coef[3,]
yr5_robci = rd_yr5$ci[3,]

# 2006
# Create variables
y <- pooled_06$rate
x <- pooled_06$distrelative

# 2006
# Bandwidth = 304.836
rd_yr6 <- rdrobust(y, x, c=0, p=1, h=304.836)
yr6_conv = rd_yr6$coef[1,]
yr6_convci = rd_yr6$ci[1,]
yr6_bias = rd_yr6$coef[2,]
yr6_biasci = rd_yr6$ci[2,]
yr6_rob = rd_yr6$coef[3,]
yr6_robci = rd_yr6$ci[3,]

# 2007
# Create variables
y <- pooled_07$rate
x <- pooled_07$distrelative

# 2007
# Bandwidth = 304.836
rd_yr7 <- rdrobust(y, x, c=0, p=1, h=304.836)
yr7_conv = rd_yr7$coef[1,]
yr7_convci = rd_yr7$ci[1,]
yr7_bias = rd_yr7$coef[2,]
yr7_biasci = rd_yr7$ci[2,]
yr7_rob = rd_yr7$coef[3,]
yr7_robci = rd_yr7$ci[3,]

# 2008
# Create variables
y <- pooled_08$rate
x <- pooled_08$distrelative

# 2008
# Bandwidth = 304.836
rd_yr8 <- rdrobust(y, x, c=0, p=1, h=304.836)
yr8_conv = rd_yr8$coef[1,]
yr8_convci = rd_yr8$ci[1,]
yr8_bias = rd_yr8$coef[2,]
yr8_biasci = rd_yr8$ci[2,]
yr8_rob = rd_yr8$coef[3,]
yr8_robci = rd_yr8$ci[3,]

# 2009
# Create variables
y <- pooled_09$rate
x <- pooled_09$distrelative

# 2009
# Bandwidth = 304.836
rd_yr9 <- rdrobust(y, x, c=0, p=1, h=304.836)
yr9_conv = rd_yr9$coef[1,]
yr9_convci = rd_yr9$ci[1,]
yr9_bias = rd_yr9$coef[2,]
yr9_biasci = rd_yr9$ci[2,]
yr9_rob = rd_yr9$coef[3,]
yr9_robci = rd_yr9$ci[3,]

# 2010
# Create variables
y <- pooled_10$rate
x <- pooled_10$distrelative

# 2010
# Bandwidth = 304.836
rd_yr10 <- rdrobust(y, x, c=0, p=1, h=304.836)
yr10_conv = rd_yr10$coef[1,]
yr10_convci = rd_yr10$ci[1,]
yr10_bias = rd_yr10$coef[2,]
yr10_biasci = rd_yr10$ci[2,]
yr10_rob = rd_yr10$coef[3,]
yr10_robci = rd_yr10$ci[3,]

# 2011
# Create variables
y <- pooled_11$rate
x <- pooled_11$distrelative

# 2011
# Bandwidth = 304.836
rd_yr11 <- rdrobust(y, x, c=0, p=1, h=304.836)
yr11_conv = rd_yr11$coef[1,]
yr11_convci = rd_yr11$ci[1,]
yr11_bias = rd_yr11$coef[2,]
yr11_biasci = rd_yr11$ci[2,]
yr11_rob = rd_yr11$coef[3,]
yr11_robci = rd_yr11$ci[3,]

# 2012
# Create variables
y <- pooled_12$rate
x <- pooled_12$distrelative

# 2012
# Bandwidth = 304.836
rd_yr12 <- rdrobust(y, x, c=0, p=1, h=304.836)
yr12_conv = rd_yr12$coef[1,]
yr12_convci = rd_yr12$ci[1,]
yr12_bias = rd_yr12$coef[2,]
yr12_biasci = rd_yr12$ci[2,]
yr12_rob = rd_yr12$coef[3,]
yr12_robci = rd_yr12$ci[3,]

# 2013
# Create variables
y <- pooled_13$rate
x <- pooled_13$distrelative

# 2013
# Bandwidth = 304.836
rd_yr13 <- rdrobust(y, x, c=0, p=1, h=304.836)
yr13_conv = rd_yr13$coef[1,]
yr13_convci = rd_yr13$ci[1,]
yr13_bias = rd_yr13$coef[2,]
yr13_biasci = rd_yr13$ci[2,]
yr13_rob = rd_yr13$coef[3,]
yr13_robci = rd_yr13$ci[3,]

# 2014
# Create variables
y <- pooled_14$rate
x <- pooled_14$distrelative

# 2014
# Bandwidth = 304.836
rd_yr14 <- rdrobust(y, x, c=0, p=1, h=304.836)
yr14_conv = rd_yr14$coef[1,]
yr14_convci = rd_yr14$ci[1,]
yr14_bias = rd_yr14$coef[2,]
yr14_biasci = rd_yr14$ci[2,]
yr14_rob = rd_yr14$coef[3,]
yr14_robci = rd_yr14$ci[3,]

# 2015
# Create variables
y <- pooled_15$rate
x <- pooled_15$distrelative

# 2015
# Bandwidth = 304.836
rd_yr15 <- rdrobust(y, x, c=0, p=1, h=304.836)
yr15_conv = rd_yr15$coef[1,]
yr15_convci = rd_yr15$ci[1,]
yr15_bias = rd_yr15$coef[2,]
yr15_biasci = rd_yr15$ci[2,]
yr15_rob = rd_yr15$coef[3,]
yr15_robci = rd_yr15$ci[3,]

# 2016
# Create variables
y <- pooled_16$rate
x <- pooled_16$distrelative

# 2016
# Bandwidth = 304.836
rd_yr16 <- rdrobust(y, x, c=0, p=1, h=304.836)
yr16_conv = rd_yr16$coef[1,]
yr16_convci = rd_yr16$ci[1,]
yr16_bias = rd_yr16$coef[2,]
yr16_biasci = rd_yr16$ci[2,]
yr16_rob = rd_yr16$coef[3,]
yr16_robci = rd_yr16$ci[3,]

# 2017
# Create variables
y <- pooled_17$rate
x <- pooled_17$distrelative

# 2017
# Bandwidth = 304.836
rd_yr17 <- rdrobust(y, x, c=0, p=1, h=304.836)
yr17_conv = rd_yr17$coef[1,]
yr17_convci = rd_yr17$ci[1,]
yr17_bias = rd_yr17$coef[2,]
yr17_biasci = rd_yr17$ci[2,]
yr17_rob = rd_yr17$coef[3,]
yr17_robci = rd_yr17$ci[3,]

# Preparing the dataframe for the plot - conventional estimates
#2001
coef <- cbind(yr1_conv, yr1_convci[1], yr1_convci[2])
df <- coef %>% as.data.frame() %>% mutate(year=2001)
df1 <- df %>% dplyr::rename(CI_Lower=V2, CI_Upper=V3)
names(df1)[1] <- "coef"
#2002
coef <- cbind(yr2_conv, yr2_convci[1], yr2_convci[2])
df <- coef %>% as.data.frame() %>% mutate(year=2002)
df2 <- df %>% dplyr::rename(CI_Lower=V2, CI_Upper=V3)
names(df2)[1] <- "coef"
#2003
coef <- cbind(yr3_conv, yr3_convci[1], yr3_convci[2])
df <- coef %>% as.data.frame() %>% mutate(year=2003)
df3 <- df %>% dplyr::rename(CI_Lower=V2, CI_Upper=V3)
names(df3)[1] <- "coef"
#2004
coef <- cbind(yr4_conv, yr4_convci[1], yr4_convci[2])
df <- coef %>% as.data.frame() %>% mutate(year=2004)
df4 <- df %>% dplyr::rename(CI_Lower=V2, CI_Upper=V3)
names(df4)[1] <- "coef"
#2005
coef <- cbind(yr5_conv, yr5_convci[1], yr5_convci[2])
df <- coef %>% as.data.frame() %>% mutate(year=2005)
df5 <- df %>% dplyr::rename(CI_Lower=V2, CI_Upper=V3)
names(df5)[1] <- "coef"
#2006
coef <- cbind(yr6_conv, yr6_convci[1], yr6_convci[2])
df <- coef %>% as.data.frame() %>% mutate(year=2006)
df6 <- df %>% dplyr::rename(CI_Lower=V2, CI_Upper=V3)
names(df6)[1] <- "coef"
#2007
coef <- cbind(yr7_conv, yr7_convci[1], yr7_convci[2])
df <- coef %>% as.data.frame() %>% mutate(year=2007)
df7 <- df %>% dplyr::rename(CI_Lower=V2, CI_Upper=V3)
names(df7)[1] <- "coef"
#2008
coef <- cbind(yr8_conv, yr8_convci[1], yr8_convci[2])
df <- coef %>% as.data.frame() %>% mutate(year=2008)
df8 <- df %>% dplyr::rename(CI_Lower=V2, CI_Upper=V3)
names(df8)[1] <- "coef"
#2009
coef <- cbind(yr9_conv, yr9_convci[1], yr9_convci[2])
df <- coef %>% as.data.frame() %>% mutate(year=2009)
df9 <- df %>% dplyr::rename(CI_Lower=V2, CI_Upper=V3)
names(df9)[1] <- "coef"
#2010
coef <- cbind(yr10_conv, yr10_convci[1], yr10_convci[2])
df <- coef %>% as.data.frame() %>% mutate(year=2010)
df10 <- df %>% dplyr::rename(CI_Lower=V2, CI_Upper=V3)
names(df10)[1] <- "coef"
#2011
coef <- cbind(yr11_conv, yr11_convci[1], yr11_convci[2])
df <- coef %>% as.data.frame() %>% mutate(year=2011)
df11 <- df %>% dplyr::rename(CI_Lower=V2, CI_Upper=V3)
names(df11)[1] <- "coef"
#2012
coef <- cbind(yr12_conv, yr12_convci[1], yr12_convci[2])
df <- coef %>% as.data.frame() %>% mutate(year=2012)
df12 <- df %>% dplyr::rename(CI_Lower=V2, CI_Upper=V3)
names(df12)[1] <- "coef"
#2013
coef <- cbind(yr13_conv, yr13_convci[1], yr13_convci[2])
df <- coef %>% as.data.frame() %>% mutate(year=2013)
df13 <- df %>% dplyr::rename(CI_Lower=V2, CI_Upper=V3)
names(df13)[1] <- "coef"
#2014
coef <- cbind(yr14_conv, yr14_convci[1], yr14_convci[2])
df <- coef %>% as.data.frame() %>% mutate(year=2014)
df14 <- df %>% dplyr::rename(CI_Lower=V2, CI_Upper=V3)
names(df14)[1] <- "coef"
#2015
coef <- cbind(yr15_conv, yr15_convci[1], yr15_convci[2])
df <- coef %>% as.data.frame() %>% mutate(year=2015)
df15 <- df %>% dplyr::rename(CI_Lower=V2, CI_Upper=V3)
names(df15)[1] <- "coef"
#2016
coef <- cbind(yr16_conv, yr16_convci[1], yr16_convci[2])
df <- coef %>% as.data.frame() %>% mutate(year=2016)
df16 <- df %>% dplyr::rename(CI_Lower=V2, CI_Upper=V3)
names(df16)[1] <- "coef"
#2017
coef <- cbind(yr17_conv, yr17_convci[1], yr17_convci[2])
df <- coef %>% as.data.frame() %>% mutate(year=2017)
df17 <- df %>% dplyr::rename(CI_Lower=V2, CI_Upper=V3)
names(df17)[1] <- "coef"

dfconv <- rbind(df1, df2, df3, df4, df5, df6, df7, df8, df9, df10, df11, df12, df13, df14, df15, df16, df17)
dfconv$type = c("Conventional")

# Preparing the dataframe for the plot - Bias-corrected estimates
#2001
coef <- cbind(yr1_bias, yr1_biasci[1], yr1_biasci[2])
df <- coef %>% as.data.frame() %>% mutate(year=2001)
df1 <- df %>% dplyr::rename(CI_Lower=V2, CI_Upper=V3)
names(df1)[1] <- "coef"
#2002
coef <- cbind(yr2_bias, yr2_biasci[1], yr2_biasci[2])
df <- coef %>% as.data.frame() %>% mutate(year=2002)
df2 <- df %>% dplyr::rename(CI_Lower=V2, CI_Upper=V3)
names(df2)[1] <- "coef"
#2003
coef <- cbind(yr3_bias, yr3_biasci[1], yr3_biasci[2])
df <- coef %>% as.data.frame() %>% mutate(year=2003)
df3 <- df %>% dplyr::rename(CI_Lower=V2, CI_Upper=V3)
names(df3)[1] <- "coef"
#2004
coef <- cbind(yr4_bias, yr4_biasci[1], yr4_biasci[2])
df <- coef %>% as.data.frame() %>% mutate(year=2004)
df4 <- df %>% dplyr::rename(CI_Lower=V2, CI_Upper=V3)
names(df4)[1] <- "coef"
#2005
coef <- cbind(yr5_bias, yr5_biasci[1], yr5_biasci[2])
df <- coef %>% as.data.frame() %>% mutate(year=2005)
df5 <- df %>% dplyr::rename(CI_Lower=V2, CI_Upper=V3)
names(df5)[1] <- "coef"
#2006
coef <- cbind(yr6_bias, yr6_biasci[1], yr6_biasci[2])
df <- coef %>% as.data.frame() %>% mutate(year=2006)
df6 <- df %>% dplyr::rename(CI_Lower=V2, CI_Upper=V3)
names(df6)[1] <- "coef"
#2007
coef <- cbind(yr7_bias, yr7_biasci[1], yr7_biasci[2])
df <- coef %>% as.data.frame() %>% mutate(year=2007)
df7 <- df %>% dplyr::rename(CI_Lower=V2, CI_Upper=V3)
names(df7)[1] <- "coef"
#2008
coef <- cbind(yr8_bias, yr8_biasci[1], yr8_biasci[2])
df <- coef %>% as.data.frame() %>% mutate(year=2008)
df8 <- df %>% dplyr::rename(CI_Lower=V2, CI_Upper=V3)
names(df8)[1] <- "coef"
#2009
coef <- cbind(yr9_bias, yr9_biasci[1], yr9_biasci[2])
df <- coef %>% as.data.frame() %>% mutate(year=2009)
df9 <- df %>% dplyr::rename(CI_Lower=V2, CI_Upper=V3)
names(df9)[1] <- "coef"
#2010
coef <- cbind(yr10_bias, yr10_biasci[1], yr10_biasci[2])
df <- coef %>% as.data.frame() %>% mutate(year=2010)
df10 <- df %>% dplyr::rename(CI_Lower=V2, CI_Upper=V3)
names(df10)[1] <- "coef"
#2011
coef <- cbind(yr11_bias, yr11_biasci[1], yr11_biasci[2])
df <- coef %>% as.data.frame() %>% mutate(year=2011)
df11 <- df %>% dplyr::rename(CI_Lower=V2, CI_Upper=V3)
names(df11)[1] <- "coef"
#2012
coef <- cbind(yr12_bias, yr12_biasci[1], yr12_biasci[2])
df <- coef %>% as.data.frame() %>% mutate(year=2012)
df12 <- df %>% dplyr::rename(CI_Lower=V2, CI_Upper=V3)
names(df12)[1] <- "coef"
#2013
coef <- cbind(yr13_bias, yr13_biasci[1], yr13_biasci[2])
df <- coef %>% as.data.frame() %>% mutate(year=2013)
df13 <- df %>% dplyr::rename(CI_Lower=V2, CI_Upper=V3)
names(df13)[1] <- "coef"
#2014
coef <- cbind(yr14_bias, yr14_biasci[1], yr14_biasci[2])
df <- coef %>% as.data.frame() %>% mutate(year=2014)
df14 <- df %>% dplyr::rename(CI_Lower=V2, CI_Upper=V3)
names(df14)[1] <- "coef"
#2015
coef <- cbind(yr15_bias, yr15_biasci[1], yr15_biasci[2])
df <- coef %>% as.data.frame() %>% mutate(year=2015)
df15 <- df %>% dplyr::rename(CI_Lower=V2, CI_Upper=V3)
names(df15)[1] <- "coef"
#2016
coef <- cbind(yr16_bias, yr16_biasci[1], yr16_biasci[2])
df <- coef %>% as.data.frame() %>% mutate(year=2016)
df16 <- df %>% dplyr::rename(CI_Lower=V2, CI_Upper=V3)
names(df16)[1] <- "coef"
#2017
coef <- cbind(yr17_bias, yr17_biasci[1], yr17_biasci[2])
df <- coef %>% as.data.frame() %>% mutate(year=2017)
df17 <- df %>% dplyr::rename(CI_Lower=V2, CI_Upper=V3)
names(df17)[1] <- "coef"

dfbias <- rbind(df1, df2, df3, df4, df5, df6, df7, df8, df9, df10, df11, df12, df13, df14, df15, df16, df17)
dfbias$type = c("Bias-corrected")

# Preparing the dataframe for the plot - Robust estimates
#2001
coef <- cbind(yr1_rob, yr1_robci[1], yr1_robci[2])
df <- coef %>% as.data.frame() %>% mutate(year=2001)
df1 <- df %>% dplyr::rename(CI_Lower=V2, CI_Upper=V3)
names(df1)[1] <- "coef"
#2002
coef <- cbind(yr2_rob, yr2_robci[1], yr2_robci[2])
df <- coef %>% as.data.frame() %>% mutate(year=2002)
df2 <- df %>% dplyr::rename(CI_Lower=V2, CI_Upper=V3)
names(df2)[1] <- "coef"
#2003
coef <- cbind(yr3_rob, yr3_robci[1], yr3_robci[2])
df <- coef %>% as.data.frame() %>% mutate(year=2003)
df3 <- df %>% dplyr::rename(CI_Lower=V2, CI_Upper=V3)
names(df3)[1] <- "coef"
#2004
coef <- cbind(yr4_rob, yr4_robci[1], yr4_robci[2])
df <- coef %>% as.data.frame() %>% mutate(year=2004)
df4 <- df %>% dplyr::rename(CI_Lower=V2, CI_Upper=V3)
names(df4)[1] <- "coef"
#2005
coef <- cbind(yr5_rob, yr5_robci[1], yr5_robci[2])
df <- coef %>% as.data.frame() %>% mutate(year=2005)
df5 <- df %>% dplyr::rename(CI_Lower=V2, CI_Upper=V3)
names(df5)[1] <- "coef"
#2006
coef <- cbind(yr6_rob, yr6_robci[1], yr6_robci[2])
df <- coef %>% as.data.frame() %>% mutate(year=2006)
df6 <- df %>% dplyr::rename(CI_Lower=V2, CI_Upper=V3)
names(df6)[1] <- "coef"
#2007
coef <- cbind(yr7_rob, yr7_robci[1], yr7_robci[2])
df <- coef %>% as.data.frame() %>% mutate(year=2007)
df7 <- df %>% dplyr::rename(CI_Lower=V2, CI_Upper=V3)
names(df7)[1] <- "coef"
#2008
coef <- cbind(yr8_rob, yr8_robci[1], yr8_robci[2])
df <- coef %>% as.data.frame() %>% mutate(year=2008)
df8 <- df %>% dplyr::rename(CI_Lower=V2, CI_Upper=V3)
names(df8)[1] <- "coef"
#2009
coef <- cbind(yr9_rob, yr9_robci[1], yr9_robci[2])
df <- coef %>% as.data.frame() %>% mutate(year=2009)
df9 <- df %>% dplyr::rename(CI_Lower=V2, CI_Upper=V3)
names(df9)[1] <- "coef"
#2010
coef <- cbind(yr10_rob, yr10_robci[1], yr10_robci[2])
df <- coef %>% as.data.frame() %>% mutate(year=2010)
df10 <- df %>% dplyr::rename(CI_Lower=V2, CI_Upper=V3)
names(df10)[1] <- "coef"
#2011
coef <- cbind(yr11_rob, yr11_robci[1], yr11_robci[2])
df <- coef %>% as.data.frame() %>% mutate(year=2011)
df11 <- df %>% dplyr::rename(CI_Lower=V2, CI_Upper=V3)
names(df11)[1] <- "coef"
#2012
coef <- cbind(yr12_rob, yr12_robci[1], yr12_robci[2])
df <- coef %>% as.data.frame() %>% mutate(year=2012)
df12 <- df %>% dplyr::rename(CI_Lower=V2, CI_Upper=V3)
names(df12)[1] <- "coef"
#2013
coef <- cbind(yr13_rob, yr13_robci[1], yr13_robci[2])
df <- coef %>% as.data.frame() %>% mutate(year=2013)
df13 <- df %>% dplyr::rename(CI_Lower=V2, CI_Upper=V3)
names(df13)[1] <- "coef"
#2014
coef <- cbind(yr14_rob, yr14_robci[1], yr14_robci[2])
df <- coef %>% as.data.frame() %>% mutate(year=2014)
df14 <- df %>% dplyr::rename(CI_Lower=V2, CI_Upper=V3)
names(df14)[1] <- "coef"
#2015
coef <- cbind(yr15_rob, yr15_robci[1], yr15_robci[2])
df <- coef %>% as.data.frame() %>% mutate(year=2015)
df15 <- df %>% dplyr::rename(CI_Lower=V2, CI_Upper=V3)
names(df15)[1] <- "coef"
#2016
coef <- cbind(yr16_rob, yr16_robci[1], yr16_robci[2])
df <- coef %>% as.data.frame() %>% mutate(year=2016)
df16 <- df %>% dplyr::rename(CI_Lower=V2, CI_Upper=V3)
names(df16)[1] <- "coef"
#2017
coef <- cbind(yr17_rob, yr17_robci[1], yr17_robci[2])
df <- coef %>% as.data.frame() %>% mutate(year=2017)
df17 <- df %>% dplyr::rename(CI_Lower=V2, CI_Upper=V3)
names(df17)[1] <- "coef"

dfrob <- rbind(df1, df2, df3, df4, df5, df6, df7, df8, df9, df10, df11, df12, df13, df14, df15, df16, df17)
dfrob$type = c("Robust")

df <- rbind(dfconv, dfbias, dfrob)


p1 <- ggplot(dfconv, aes(x=year,y=coef)) + geom_line() + geom_point() + 
  geom_errorbar(aes(ymin=CI_Lower, ymax=CI_Upper), width=.2) + 
  geom_hline(yintercept = 0, color = "red") + 
  scale_x_continuous(name="Year", breaks = seq(2001, 2017, 1)) + 
  scale_y_continuous("Estimate") + 
  theme(axis.text=element_text(size=8), axis.title=element_text(size=8)) + 
  theme_tufte(base_family="Arial", base_size=10)

p2 <- ggplot(df, aes(x=year,y=coef, group=type, colour=type)) + 
  geom_line() + 
  geom_point() + 
  geom_errorbar(aes(ymin=CI_Lower, ymax=CI_Upper), width=.2) + 
  geom_hline(yintercept = 0, color = "red") + 
  scale_x_continuous(name="Year", breaks = seq(2001, 2017, 1)) + 
  scale_y_continuous(name="Estimate", breaks = seq(-0.5, 1.5, 0.5)) + 
  theme(axis.text=element_text(size=8), axis.title=element_text(size=8)) + 
  theme_tufte(base_family="Arial")  

p1 
p2

# Figure 14: Average annual rate of forest cover loss by land title type
panel_2 <- pooled %>% select(SFP, distrelative, yr_00, yr_1, yr_2, yr_3, yr_4, yr_5, yr_6, yr_7, yr_8, yr_9, yr_10, yr_11, yr_12, yr_13, yr_14, yr_15, yr_16, yr_17, study_area, SFPID, rate, time, did)

panel_2$rate_1 = -((panel_2$yr_1-panel_2$yr_00)/1)
panel_2$rate_2 = -((panel_2$yr_2-panel_2$yr_1)/1)
panel_2$rate_3 = -((panel_2$yr_3-panel_2$yr_2)/1)
panel_2$rate_4 = -((panel_2$yr_4-panel_2$yr_3)/1)
panel_2$rate_5 = -((panel_2$yr_5-panel_2$yr_4)/1)
panel_2$rate_6 = -((panel_2$yr_6-panel_2$yr_5)/1)
panel_2$rate_7 = -((panel_2$yr_7-panel_2$yr_6)/1)
panel_2$rate_8 = -((panel_2$yr_8-panel_2$yr_7)/1)
panel_2$rate_9 = -((panel_2$yr_9-panel_2$yr_8)/1)
panel_2$rate_10 = -((panel_2$yr_10-panel_2$yr_9)/1)
panel_2$rate_11 = -((panel_2$yr_11-panel_2$yr_10)/1)
panel_2$rate_12 = -((panel_2$yr_12-panel_2$yr_11)/1)
panel_2$rate_13 = -((panel_2$yr_13-panel_2$yr_12)/1)
panel_2$rate_14 = -((panel_2$yr_14-panel_2$yr_13)/1)
panel_2$rate_15 = -((panel_2$yr_15-panel_2$yr_14)/1)
panel_2$rate_16 = -((panel_2$yr_16-panel_2$yr_15)/1)
panel_2$rate_17 = -((panel_2$yr_17-panel_2$yr_16)/1)

panel_2 <- panel_2 %>% select(-c(yr_00, yr_1, yr_2, yr_3, yr_4, yr_5, yr_6, yr_7, yr_8, yr_9, yr_10, yr_11, yr_12, yr_13, yr_14, yr_15, yr_16, yr_17))
panel_2 <- panel_2 %>% gather(key=Year, value=Loss, -c(SFP, distrelative, study_area, SFPID, rate, time, did))
head(panel_2)

parallel2 <- panel_2 %>% group_by(Year, SFP) %>% summarise_at(vars(Loss), funs(mean(., na.rm=TRUE)))

par_sub2 <- panel_2 %>% filter(distrelative >= -304.836 & distrelative <= 304.836) %>% group_by(Year, SFP) %>% summarise_at(vars(Loss), funs(mean(., na.rm=TRUE)))

par_sub2$Year <- dplyr::recode(par_sub2$Year, rate_1="2001", rate_2="2002", rate_3="2003", rate_4="2004", rate_5="2005", rate_6="2006", rate_7="2007", rate_8="2008", rate_9="2009", rate_10="2010", rate_11="2011", rate_12="2012", rate_13="2013", rate_14="2014", rate_15="2015", rate_16="2016", rate_17="2017")
par_sub2 <- par_sub2 %>% arrange(Year)
par_sub2 <- par_sub2 %>% dplyr::rename(Mean=Loss)

par_sub2$Year <- as.numeric(par_sub2$Year)
ptrends_sub2 <- ggplot(par_sub2, aes(x=Year, y=Mean, colour=SFP)) + geom_line() + geom_point() + geom_vline(xintercept=2008) + scale_y_continuous(name = "Average annual rate of forest cover loss (%)", limits = c(0, 1.2)) + scale_x_continuous(name="Year", limits=c(2001, 2017), breaks=c(2001, 2008, 2017)) + theme_classic()

ptrends_sub2

# Figure 15: Mean forest cover levels per year (in %) by land title type
panel <- pooled %>% select(SFP, distrelative, yr_00, yr_1, yr_2, yr_3, yr_4, yr_5, yr_6, yr_7, yr_8, yr_9, yr_10, yr_11, yr_12, yr_13, yr_14, yr_15, yr_16, yr_17, study_area, SFPID, rate, time, did)

panel <- panel %>% gather(key=Year, value=Cover, -c(SFP, distrelative, study_area, SFPID, rate, time, did))
head(panel)

parallel <- panel %>% group_by(Year, SFP) %>% summarise_at(vars(Cover), funs(mean(., na.rm=TRUE)))

par_sub <- panel %>% filter(distrelative >= -304.836 & distrelative <= 304.836) %>% group_by(Year, SFP) %>% summarise_at(vars(Cover), funs(mean(., na.rm=TRUE)))

parallel$Year <- dplyr::recode(parallel$Year, yr_00="2000", yr_1="2001", yr_2="2002", yr_3="2003", yr_4="2004", yr_5="2005", yr_6="2006", yr_7="2007", yr_8="2008", yr_9="2009", yr_10="2010", yr_11="2011", yr_12="2012", yr_13="2013", yr_14="2014", yr_15="2015", yr_16="2016", yr_17="2017")
parallel <- parallel %>% arrange(Year)
parallel <- parallel %>% dplyr::rename(Mean=Cover)

parallel$Year <- as.numeric(parallel$Year)
ptrends <- ggplot(parallel, aes(x=Year, y=Mean, colour=SFP)) + geom_line() + geom_point() + geom_vline(xintercept=2008) + scale_y_continuous(name = "Mean % forest cover", limits = c(77,92)) + scale_x_continuous(name="Year", limits=c(2000, 2017), breaks=c(2000, 2008, 2017)) + theme_classic()

ptrends

#Constraining sample to 305 m bandwidth: 
par_sub$Year <- dplyr::recode(par_sub$Year, yr_00="2000", yr_1="2001", yr_2="2002", yr_3="2003", yr_4="2004", yr_5="2005", yr_6="2006", yr_7="2007", yr_8="2008", yr_9="2009", yr_10="2010", yr_11="2011", yr_12="2012", yr_13="2013", yr_14="2014", yr_15="2015", yr_16="2016", yr_17="2017")
par_sub <- par_sub %>% arrange(Year)
par_sub <- par_sub %>% dplyr::rename(Mean=Cover)

par_sub$Year <- as.numeric(par_sub$Year)
ptrends_sub <- ggplot(par_sub, aes(x=Year, y=Mean, colour=SFP)) + geom_line() + geom_point() + geom_vline(xintercept=2008) + scale_y_continuous(name = "Mean % forest cover", limits = c(77,92)) + scale_x_continuous(name="Year", limits=c(2000, 2017), breaks=c(2000, 2008, 2017)) + theme_classic()

ptrends_sub

plot_grid(ptrends, ptrends_sub, labels=c("A", "B"))

# Figure 16: Placebo test: Sensitivity to fake cutoff points
postconv = post_reg$coef[1]
postci = post_reg$ci[1,]
postpv = post_reg$pv[1]

placebo1 = rdrobust(y_post[x>=0], x_post[x>=0], c=1, h=bw)
plac1conv = placebo1$coef[1]
plac1ci = placebo1$ci[1,]
plac1pv = placebo1$pv[1]

placebo2 = rdrobust(y_post[x<=0], x_post[x<=0], c=-1, h=bw)
plac2conv = placebo2$coef[1]
plac2ci = placebo2$ci[1,]
plac2pv = placebo2$pv[1]

placebo3 = rdrobust(y_post[x<=0], x_post[x<=0], c=-2, h=bw)
plac3conv = placebo3$coef[1]
plac3ci = placebo3$ci[1,]
plac3pv = placebo3$pv[1]

placebo4 = rdrobust(y_post[x<=0], x_post[x<=0], c=-3, h=bw)
plac4conv = placebo4$coef[1]
plac4ci = placebo4$ci[1,]
plac4pv = placebo4$pv[1]

placebo5 = rdrobust(y_post[x>=0], x_post[x>=0], c=2, h=bw)
plac5conv = placebo5$coef[1]
plac5ci = placebo5$ci[1,]
plac5pv = placebo5$pv[1]

placebo6 = rdrobust(y_post[x>=0], x_post[x>=0], c=3, h=bw)
plac6conv = placebo6$coef[1]
plac6ci = placebo6$ci[1,]
plac6pv = placebo6$pv[1]

#Table
row1 = cbind(plac4conv, plac4pv, plac4ci[1], plac4ci[2])
row2 = cbind(plac3conv, plac3pv, plac3ci[1], plac3ci[2])
row3 = cbind(plac2conv, plac2pv, plac2ci[1], plac2ci[2])
row4 = cbind(postconv, postpv, postci[1], postci[2])
row5 = cbind(plac1conv, plac1pv, plac1ci[1], plac1ci[2])
row6 = cbind(plac5conv, plac5pv, plac5ci[1], plac5ci[2])
row7 = cbind(plac6conv, plac6pv, plac6ci[1], plac6ci[2])

plactable <- rbind(row1, row2, row3, row4, row5, row6, row7)
plactable <- as.data.frame(plactable)
plactable <- plactable %>% dplyr::rename("DiRD"= "plac4conv", 
                                         "p"="plac4pv", 
                                         "CIUpper"="V3", 
                                         "CILower"="V4")
plactable$Cutoff <- c(-3, -2, -1, 0, 1, 2, 3)
plactable <- round(plactable,3)

# Preparing the dataframe for the placebo plot
#c=0
coef <- cbind(postconv, postci[1], postci[2], postpv)
df <- as.data.frame(coef)
df0 <- df %>% 
  dplyr::rename(CI_Lower=V2, CI_Upper=V3, p=postpv) %>% 
  mutate(Cutoff=0)
names(df0)[1] <- "coef"

#c=1
coef <- cbind(plac1conv, plac1ci[1], plac1ci[2], plac1pv)
df <- as.data.frame(coef)
df1 <- df %>% 
  dplyr::rename(CI_Lower=V2, CI_Upper=V3, p=plac1pv) %>% 
  mutate(Cutoff=1)
names(df1)[1] <- "coef"

#c=-1
coef <- cbind(plac2conv, plac2ci[1], plac2ci[2], plac2pv)
df <- as.data.frame(coef)
df2 <- df %>% 
  dplyr::rename(CI_Lower=V2, CI_Upper=V3, p=plac2pv) %>% 
  mutate(Cutoff=-1)
names(df2)[1] <- "coef"

#c=-2
coef <- cbind(plac3conv, plac3ci[1], plac3ci[2], plac3pv)
df <- as.data.frame(coef)
df3 <- df %>% 
  dplyr::rename(CI_Lower=V2, CI_Upper=V3, p=plac3pv) %>% 
  mutate(Cutoff=-2)
names(df3)[1] <- "coef"

#c=-3
coef <- cbind(plac4conv, plac4ci[1], plac4ci[2], plac4pv)
df <- as.data.frame(coef)
df4 <- df %>% 
  dplyr::rename(CI_Lower=V2, CI_Upper=V3, p=plac4pv) %>% 
  mutate(Cutoff=-3)
names(df4)[1] <- "coef"

#c=2
coef <- cbind(plac5conv, plac5ci[1], plac5ci[2], plac5pv)
df <- as.data.frame(coef)
df5 <- df %>% 
  dplyr::rename(CI_Lower=V2, CI_Upper=V3, p=plac5pv) %>% 
  mutate(Cutoff=2)
names(df5)[1] <- "coef"

#c=3
coef <- cbind(plac6conv, plac6ci[1], plac6ci[2], plac6pv)
df <- as.data.frame(coef)
df6 <- df %>% 
  dplyr::rename(CI_Lower=V2, CI_Upper=V3, p=plac6pv) %>% 
  mutate(Cutoff=3)
names(df6)[1] <- "coef"

dfplacebo <- rbind(df0, df1, df2, df3, df4, df5, df6)

placebo.plot <- ggplot(dfplacebo, aes(x=Cutoff,y=coef)) + geom_point() + 
  geom_errorbar(aes(ymin=CI_Lower, ymax=CI_Upper), width=.2) + 
  geom_vline(xintercept = 0, linetype="dashed", color = "grey") + 
  geom_hline(yintercept=0, color="red") + 
  scale_x_continuous(breaks = seq(-3, 3, 1)) + 
  labs(x="Cutoff (x=0 true cutoff)", y="DiRD Estimate") + 
  theme(axis.text=element_text(size=8), axis.title=element_text(size=10)) + 
  theme_bw()

placebo.plot

# Figure 17: Covariate balance - Soil type
ggplot(covs_df, aes(x=soil)) + geom_bar()
ggplot(covs_df, aes(x=soil, y=distrelative)) + geom_boxplot() + coord_flip()

covs_df1 <- covs_df
covs_df1$soil <- factor(covs_df1$soil)

covs_df1$SFP <- ifelse(covs_df1$distrelative > 0, TRUE, FALSE)


theme_set(theme_ridges())
soilplot <- ggplot(covs_df1, aes(x=distrelative)) + geom_histogram(bins=60, alpha=0.8, colour="darkorange", fill="darkorange") + geom_vline(xintercept = 0, colour="black", linetype="solid") + theme_bw() + labs(x="Relative distance from border (m)", y="") 

soilplot + facet_wrap(~soil, ncol=2) + removeGrid()






