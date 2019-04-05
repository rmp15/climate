rm(list=ls())

library(ggplot2)
library(foreign)
library(plyr)
library(lubridate)
library(maptools)
library(mapproj)
library(rgeos)
library(rgdal)
library(RColorBrewer)
library(scales)

# arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)
year.start <- as.numeric(args[1])
year.end <- as.numeric(args[2])
dname <- as.character(args[3])
metric <- as.character(args[4])

year.start=1980 ; year.end=2009 ; dname='t2m' ; metric='mean'

cat('plotting for years',year.start,year.end)

directory = paste0("../../output/multiyear_normals_plots/",dname,"/",metric)
ifelse(!dir.exists(directory), dir.create(directory, recursive=TRUE), FALSE)

# replace name of var
var <- paste0('mean_',dname)

# load data
dat = readRDS(paste0("../../output/multiyear_normals/",dname,"/",metric,'/state_longterm_95_nonnormals_',var,'_',year.start,'_',year.end,'.rds'))

# isolate one age-sex group and remove alaska and hawaii
dat = subset(dat,age==65&sex==1)
dat = subset(dat, !(state.fips%in%c('02','15')))

library(plyr)

# short names for months
month.short = c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')
dat$month.short = mapvalues(dat$month,from=sort(unique(dat$month)),to=month.short)
dat$month.short <- reorder(dat$month.short,dat$month)

# attach state names
state.lookup <- read.csv('~/git/mortality/USA/state/data/fips_lookup/name_fips_lookup.csv')
dat$fips = as.numeric(dat$state.fips)
dat = merge(dat,state.lookup,by='fips',all.x=TRUE)

# generalise names of columns
names(dat)[6]='variable'
names(dat)[7]='variable.min'
names(dat)[8]='variable.max'

library(ggplot2)

# plot by average values by month
pdf(paste0(directory,'/longterm_nonnormals.pdf'),height=0,width=0,paper='a4r')
ggplot(data=subset(dat,!(fips%in%c(2,15)))) +
    geom_point(aes(x=code_name,y=variable)) +
    geom_hline(yintercept=0,linetype='dotted') +
    geom_errorbar(aes(x=code_name,ymin=variable.min,ymax=variable.max)) +
    ylab('Mean temperature (degrees Celsius)') + xlab('State') +
    facet_wrap(~month.short) +
    theme_bw() + theme( panel.grid.major = element_blank(),axis.text.x = element_text(size=5,angle=90),
    axis.ticks.x=element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))

ggplot(data=subset(dat,!(fips%in%c(2,15)))) +
    geom_point(aes(x=month.short,y=variable)) +
    geom_hline(yintercept=0,linetype='dotted') +
    geom_errorbar(aes(x=month.short,ymin=variable.min,ymax=variable.max)) +
    ylab('Mean temperature (degrees Celsius)') + xlab('Month') +
    facet_wrap(~full_name) +
    theme_bw() + theme( panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
    axis.ticks.x=element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))
dev.off()

###############################################################
# PREPARING MAP
###############################################################

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
us <- readOGR(dsn="../../../../../mortality/USA/state/data/shapefiles",layer="states")

# convert shapefile to Albers equal area
us_aea <- spTransform(us, CRS("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"))
us_aea@data$id <- rownames(us_aea@data)

# extract, then rotate, shrink & move alaska (and reset projection)
alaska <- us_aea[us_aea$STATE_FIPS=="02",]
alaska <- elide(alaska,rotate=-50)
alaska <- elide(alaska,scale=max(apply(bbox(alaska), 1, diff)) / 2.3)
alaska <- elide(alaska, shift=c(-2100000, -2500000))
proj4string(alaska) <- proj4string(us_aea)

# extract, then rotate & shift hawaii
hawaii <- us_aea[us_aea$STATE_FIPS=="15",]
hawaii <- elide(hawaii, rotate=-35)
hawaii <- elide(hawaii, shift=c(5400000, -1400000))
proj4string(hawaii) <- proj4string(us_aea)

# remove Alaska and Hawaii (don't put back in now)
us_aea <- us_aea[!us_aea$STATE_FIPS %in% c("02", "15"),]
# us_aea <- rbind(us_aea, alaska, hawaii)

# fortify to prepare for ggplot
map <- fortify(us_aea)

# extract data from shapefile
shapefile.data <- us_aea@data

# reinsert shapefile.data with climate regions back into shapefile if changed
us_aea@data <- shapefile.data

# merge selected data to map dataframe for colouring of ggplot
USA.df <- merge(map, shapefile.data, by='id')
USA.df$STATE_FIPS <- as.integer(as.character(USA.df$STATE_FIPS))

dat$state.fips = as.integer(dat$state.fips)

USA.df.month = merge(USA.df,dat,by.y='state.fips',by.x='STATE_FIPS')
USA.df.month <- with(USA.df.month, USA.df.month[order(month,DRAWSEQ,order),])

# good colours
f <- function(pal) brewer.pal(brewer.pal.info[pal, "maxcolors"], pal)
mycols <- c(f("Dark2"), f("Set1")[1:8], f("Set2"), f("Set3"),"#89C5DA", "#DA5724", "#74D944", "#CE50CA", "#3F4921", "#C0717C", "#CBD588", "#5F7FC7", "#673770", "#D3D93E", "#38333E", "#508578", "#D7C1B1", "#689030", "#AD6F3B", "#CD9BCD", "#D14285", "#6DDE88", "#652926", "#7FDCC0", "#C84248", "#8569D5", "#5E738F", "#D1A33D", "#8A7C64", "#599861" )
#plot(1:length(mycols),col=mycols[1:length(mycols)],cex=4,pch=20); abline(v=c(10,20,30,40,50,60))

# find limits for map
min.plot <- min(USA.df.month$variable)
max.plot <- max(USA.df.month$variable)

colorfunc = colorRampPalette(c('dark blue','light blue', 'orange','red'))
ASDRpalette = colorfunc(max.plot-min.plot)

pdf(paste0(directory,'/longterm_nonnormals_map.pdf'),height=0,width=0,paper='a4r')
print(ggplot(data=subset(USA.df.month),aes(x=long,y=lat,group=group)) +
geom_polygon(aes(fill=variable),color='black',size=0.01) +
# scale_fill_gradient2(high="dark red",mid="light blue",low="dark blue",guide = 'colorbar',guide_legend(title="Temperature (°C)")) +
scale_fill_gradientn(colors=ASDRpalette,guide_legend(title="Mean temperature (°C)")) +
guides(fill=guide_colorbar(barwidth=30)) +
facet_wrap(~month.short) +
xlab('') +
ylab('') +
theme_map() +
theme(text = element_text(size = 15),legend.position = 'bottom',legend.justification=c(0.5,0.5),strip.background = element_blank(),legend.background = element_rect(fill = "grey95")))
dev.off()
