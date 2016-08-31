rm(list=ls())

library(ggplot2)
library(RColorBrewer)

# arguments from Rscript
year <- as.character(args[1])

# list of months for file name loading
month <- as.character(c('01','02','03','04','05','06','07','08','09','10','11','12'))
month.short <- c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')

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
geom_point(data=dat.merged,aes(color=as.factor(month),x=prism.temp,y=era.temp)) +
geom_abline(intercept=coef(fit)[1],slope=coef(fit)[2],color='red') +
ggtitle(paste0('ERA-Interim derived values against PRISM derived values ',year,':\n R^2=',round(r.squared,2))) +
scale_colour_manual(values=colorRampPalette(rev(brewer.pal(12,"RdYlBu")[c(9:10,2:1,1:2,10:9)]))(12),guide = guide_legend(title = 'month'),labels=month.short) +
xlab('PRISM Temperature') +
ylab('ERA Temperature') +
theme_bw()
dev.off()