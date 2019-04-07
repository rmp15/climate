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

# load lookup tables for polygons grid points and lon lat
poly.lookup <- readRDS('../../output/grid_county_intersection/point_poly_lookup.rds')

# merge poly lookup with file to get rid of everything apart from contiguous USA
dat.merged = merge(dat.merged,poly.lookup,by='poly.id')
dat.merged = subset(dat.merged,)

# carry out linear regression
# fit <- lm(prism.temp ~ era.temp, data=dat.merged)
#
# # get R squared values
# r.squared <- summary(fit)$r.squared
# print(paste0('R squared value is ',round(r.squared,2)))

# figure out correlation value
correlation = with(dat.merged,cor(prism.temp,era.temp))

# plot with line of best fit overlaid
pdf(paste0("../../output/bil_files/era_prism_correlation_all_months_",year,'.pdf'))
ggplot() +
geom_point(data=dat.merged,aes(color=as.factor(month),x=prism.temp,y=era.temp),alpha=0.6) +
geom_abline() +
# geom_abline(intercept=coef(fit)[1],slope=coef(fit)[2],color='red') +
# ggtitle(paste0('ERA-Interim derived values against PRISM derived values ',year,':\n R^2=',round(r.squared,2))) +
scale_colour_manual(values=colorRampPalette(rev(brewer.pal(12,"RdYlBu")[c(9:10,2:1,1:2,10:9)]))(12),guide = guide_legend(title = 'month'),labels=month.short) +
xlab('PRISM Temperature') +
ylab('ERA Temperature') +
guides(colour = guide_legend(nrow = 2,title='Month')) +
theme_bw() + theme(panel.grid.major = element_blank(),axis.text.x = element_text(angle=0),
plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
panel.border = element_rect(colour = "black"),strip.background = element_blank(),
legend.position = 'bottom',legend.justification='center',
legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))
dev.off()