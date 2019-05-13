rm(list=ls())

library(ggplot2)
library(foreign)
library(plyr)
library(maptools)
library(mapproj)
library(rgeos)
library(rgdal)
library(RColorBrewer)
library(scales)

# arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)
dname <- as.character(args[1])
year.start <- as.numeric(args[2])
year.end <- as.numeric(args[3])
metric.1 <- as.character(args[4])
metric.2 <- as.character(args[4])

# create directory
dir = paste0("../../output/metrics_compare_county_state/",dname,'/',metric.1,'/')
ifelse(!dir.exists(dir), dir.create(dir, recursive=TRUE), FALSE)

# load file with climate data
dat.1 <- readRDS(paste0('../../output/metrics_development/',dname,'/',metric.1,'_',dname,'/state_weighted_summary_',metric.1,'_',dname,'_',year.start,'_',year.end,'.rds'))
dat.1$state.fips <- as.numeric(as.character(dat.1$state.fips))
dat.2 <- readRDS(paste0('../../output/metrics_development/',dname,'/',metric.2,'_',dname,'/state_weighted_summary_',metric.2,'_',dname,'_',year.start,'_',year.end,'.rds'))
dat.2$state.fips <- as.numeric(as.character(dat.2$state.fips))

dat.both = merge(dat.1,dat.2)
dat.both$diff = with(dat.both,t2m.meanc3-t2m.meanc4)
dat.both = subset(dat.both,!(state.fips%in%c(2,15)))

pdf(paste0(dir,'/comparison_',year.start,'_',year.end,'.pdf'),height=0,width=0,paper='a4r')

ggplot(data=subset(dat.both,!(state.fips%in%c(2,15))),aes(x=t2m.meanc3,y=t2m.meanc4)) + facet_wrap(~month) + geom_point() + geom_abline(slope=1)

ggplot(data=subset(dat.both,!(state.fips%in%c(2,15,32))),aes(x=t2m.meanc3,y=t2m.meanc4)) + facet_wrap(~month) + geom_point() + geom_abline(slope=1)

# load file with difference data
# dat = readRDS(paste0('../../output/metrics_development/',dname,'/',metric.1,'_',dname,'/diff/',year.start,'_',year.end,'.rds'))

dat <- data.frame()
years=year.start:year.end
for (i in seq(length(years))) {
        file.name <- paste0('../../output/metrics_development/',dname,'/',metric.1,'_',dname,'/diff/',years[i])
        current.file <- readRDS(file.name)
        dat <- rbind(dat,current.file)
}

dat.summary = ddply(dat,.(month,sex,age,state.fips,state.county.fips),summarize,diff=mean(diff),pop.weighted=mean(pop.weighted))
month.short <- c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')
dat.summary$month.short <- mapvalues(dat.summary$month,from=sort(unique(dat.summary$month)),to=c(as.character(month.short)))
dat.summary$month.short <- reorder(dat.summary$month.short,dat.summary$month)

# save in case
saveRDS(dat.summary,paste0('../../output/metrics_development/',dname,'/',metric.1,'_',dname,'/diff/summary_',year.start,'_',year.end))

# load map and attach information to the map for plotting

##############################################################
# PREPARING MAP
##############################################################

# for theme_map
#devtools::source_gist("33baa3a79c5cfef0f6df")
theme_map <- function(base_size=9, base_family=""){
    require(grid)
    theme_bw(base_size=base_size,base_family=base_family) %+replace%
    theme(axis.line=element_blank(),
    axis.text=element_blank(),
    axis.ticks=element_blank(),
    axis.title=element_blank(),
    panel.background=element_blank(),
    panel.border=element_blank(),
    panel.grid=element_blank(),
    panel.margin=unit(0,"lines"),
    plot.background=element_blank(),
    legend.justification = c(0,0),
    legend.position = c(0,0)
    )
}

# load shapefile
us = readOGR(dsn='../shapefiles',layer="cb_2017_us_county_500k")

# convert shapefile to Albers equal area
us_aea <- spTransform(us, CRS("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"))
us_aea@data$id <- rownames(us_aea@data)

# remove Alaska and Hawaii
us_aea <- us_aea[!us_aea$STATEFP %in% c("66","69","72","78","60","02", "15"),]

# fortify to prepare for ggplot
map <- fortify(us_aea)

# extract data from shapefile
shapefile.data <- us_aea@data

# merge selected data to map dataframe for colouring of ggplot
USA.df = merge(map, shapefile.data, by='id')
USA.df$state.county.fips = as.character(paste0(USA.df$STATEFP,USA.df$COUNTYFP))

# attach summary data to map (testing one age sex group)
dat.summary$state.county.fips = as.numeric(dat.summary$state.county.fips)
dat.test = subset(dat.summary,age==65&sex==2&!(state.fips%in%c('02','15')))
USA.df$state.county.fips = as.numeric(USA.df$state.county.fips)
dat.test = merge(USA.df,dat.test,by='state.county.fips',all.x=TRUE)

plot <- with(dat.test, dat.test[order(month,piece,group,order),])

# map of the use by state in case needed
pdf(paste0(dir,'/county_deviations_map_',year.start,'_',year.end,'.pdf'),height=0,width=0,paper='a4r')
for(i in c(1:12)){
    print(ggplot(data=subset(plot,month==i),aes(x=long,y=lat,group=group)) +
    geom_polygon(aes(fill=diff),color='black',size=0.1) +
    scale_fill_gradient2(limits=c(0.5,-0.5),low="blue", mid="white",high="red",midpoint=0,guide = guide_legend(title = '')) +
    theme_map() +
    theme(text = element_text(size = 15),legend.justification=c(1,0), legend.position='bottom',
        legend.background = element_rect(fill = "grey95"),legend.box = "horizontal")
    )
}
dev.off()

###############################################################
# PLOTTING
###############################################################

# attach state names
state.lookup <- read.csv('~/git/mortality/USA/state/data/fips_lookup/name_fips_lookup.csv')
dat.summary$fips = as.numeric(dat.summary$state.fips)
dat.summary = merge(dat.summary,state.lookup,by='fips',all.x=TRUE)

pdf(paste0(dir,'/county_deviations',year.start,'_',year.end,'.pdf'),height=0,width=0,paper='a4r')

# ggplot(data=subset(dat.summary,age==65&sex==2&!(state.fips%in%c('02','15')))) +
#     geom_point(aes(x=state.fips,y=diff),alpha=0.5) +
#     geom_hline(yintercept=0,linetype='dotted') +
#     facet_wrap(~month.short)

ggplot(data=subset(dat.summary,age==65&sex==2&!(state.fips%in%c('02','15')))) +
    geom_point(aes(x=code_name,y=diff,alpha=0.1,size=(pop.weighted))) +
    geom_hline(yintercept=0,linetype='dotted') +
    facet_wrap(~month.short) +
    ylim(c(-1,1)) +
    xlab('State') + ylab('Average county anomaly deviation from state anomaly (°C)') +
    guides(size=FALSE,alpha=FALSE) +
    theme_bw() + theme( panel.grid.major = element_blank(),axis.text.x = element_text(size=5,angle=90),
    axis.ticks.x=element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="white", size=.5, linetype="dotted"))
dev.off()

pdf(paste0(dir,'/county_deviations_',year.start,'_',year.end,'month_separate.pdf'),height=0,width=0,paper='a4r')

for(i in sort(unique(dat.summary$month))){
    print(
    ggplot(data=subset(dat.summary,month==i&age==65&sex==2&!(state.fips%in%c('02','15')))) +
    geom_point(aes(x=code_name,y=diff,alpha=0.5,size=(pop.weighted))) +
    geom_point(data=subset(dat.summary,abs(diff)>=0.1&month==i&age==65&sex==2&!(state.fips%in%c('02','15'))),aes(x=code_name,y=diff,alpha=0.5,size=(pop.weighted)),color='red') +
    geom_hline(yintercept=0,linetype='dotted') +
    facet_wrap(~month.short) +
    ylim(c(-1,1)) +
    xlab('State') + ylab('Average county anomaly deviation from state anomaly (°C)') +
    guides(size=FALSE,alpha=FALSE) +
    theme_bw() + theme( panel.grid.major = element_blank(),axis.text.x = element_text(size=5,angle=90),
    axis.ticks.x=element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="white", size=.5, linetype="dotted"))
    )
}
dev.off()

# if needed summary again for thesis
dat.sub =subset(dat.summary,age==65&sex==2&!(state.fips%in%c('02','15')))
dim(subset(dat.sub,abs(diff)<0.1)) / dim(dat.sub)
