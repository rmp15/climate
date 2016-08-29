rm(list=ls())

library(maptools)
library(mapproj)
library(rgeos)
library(rgdal)
library(RColorBrewer)
library(ggplot2)
library(raster)
library(sp)
library(plyr)
library(graticule)
library(spatialEco)
library(reshape)

# arguments from Rscript
year <- as.character(args[1])

# list of months for file name loading
month <- as.character(c('01','02','03','04','05','06','07','08','09','10','11','12'))

# load the data
dat.merged <- data.frame()
for(i in c(1:12)){
    dummy <- readRDS(paste0('../../output/bil_files/era_prism_correlaton_',month[i],year,'.rds'))
    dat.merged <- rbind(dat.merged,dummy)
}

# carry out linear regression
fit <- lm(prism.temp ~ era.temp, data=dat.merged)

# get R squared values
r.squared <- summary(fit)$r.squared
print(paste0('R squared value is ',round(r.squared,2)))

# plot with line of best fit overlaid
pdf(paste0("../../output/bil_files/era_prism_correlation_all_months_",year,'.pdf'))
ggplot() +
geom_point(data=dat.merged,aes(x=prism.temp,y=era.temp)) +
geom_abline(intercept=coef(fit)[1],slope=coef(fit)[2],color='red') +
ggtitle(paste0('ERA-Interim derived values against PRISM derived values ',year,':\n R^2=',round(r.squared,2))) +
xlab('PRISM Temperature') +
ylab('ERA Temperature') +
theme_bw()
dev.off()

# overlay grid on raster map if required
#plot(grat.poly);plot(dat.prism,add=1)
# and test to add highlight grid square in blue
#plot(grat.poly[grat.poly@data$layer==7938,],border='blue',lwd=2,add=TRUE)