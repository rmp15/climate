rm(list=ls())

library(ggplot2)
library(RColorBrewer)
library(gridExtra)
library(scales)


# arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)
year.start <- as.numeric(args[1])
year.end <- as.numeric(args[2])
dname <- as.character(args[3])
metric <- as.character(args[4])

# create output directory
ifelse(!dir.exists("../../output/plots_against_time"), dir.create("../../output/plots_against_time"), FALSE)
ifelse(!dir.exists(paste0("../../output/plots_against_time/",dname)), dir.create(paste0("../../output/plots_against_time/",dname)), FALSE)
ifelse(!dir.exists(paste0("../../output/plots_against_time/",dname,'/',metric)), dir.create(paste0("../../output/plots_against_time/",dname,'/',metric)), FALSE)

# load state fips lookup code
fips.lookup <- read.csv('~/git/mortality/USA/state/data/fips_lookup/name_fips_lookup.csv')

# load dataset with population weighted temperature values
dat <- readRDS(paste0('../../output/metrics_development/',dname,'/',metric,'_',dname,'/state_weighted_summary_',metric,'_',dname,'_',year.start,'_',year.end,'.rds'))
dat$state.fips <- as.numeric(dat$state.fips)
dat <- merge(dat,fips.lookup,by.x='state.fips',by.y='fips',all.x=1)
names(dat)[grep(dname,names(dat))] <- 'variable'

# get rid of Hawaii and Alaska
dat = subset(dat,!(state.fips%in%c(2,15)))

# plot facetting by state and colouring by age
age.filter <- unique(dat$age)
colourCount <- length(age.filter)

# isolate a single age-sex combination
dat = subset(dat,age==65&sex==1)

# 1. over time by month and state
pdf(paste0('../../output/plots_against_time/',dname,'/',metric,'/',dname,'_',metric,'_',year.start,'_',year.end,'.pdf'),height=0,width=0,paper='a4r')
for (i in c(1:12)){
    print(ggplot(data=subset(dat,month==i),aes(x=year,y=variable)) +
    geom_point(aes(color=as.factor(month))) +
    geom_smooth(method='lm') +
    geom_hline(yintercept=0, linetype=2,alpha=0.5) +
    xlab('Year') +
    ylab(paste0(dname,'.',metric)) +
    facet_wrap(~full_name) +
    scale_colour_manual(values=colorRampPalette(rev(brewer.pal(12,"RdYlGn")[c(1:5,7:9)]))(colourCount),guide = guide_legend(title = 'Month')) +
    ggtitle(month.short) +
    scale_x_discrete(labels=c(1:12)) +
    theme_bw())
}
dev.off()

var <- paste0('mean_',dname)
metric.2 = 'mean'
year.start.2 = 1980
year.end.2 = 2009

# load long-term average values
dat.normal = readRDS(paste0("../../output/multiyear_normals/",dname,"/",metric.2,'/state_longterm_95_nonnormals_',var,'_',year.start.2,'_',year.end.2,'.rds'))
dat.normal = subset(dat.normal,!(state.fips%in%c(2,15)))
dat.normal = subset(dat.normal,age==65&sex==1)


dat.merged = merge(dat,dat.normal)
dat.merged$value=with(dat.merged,variable+t2m.30yr.mean)

# isolate New York in July and California in January?
pdf(paste0('../../output/plots_against_time/',dname,'/new_york_schematic.pdf'),height=0,width=0,paper='a4r')

    mean.value = unique(subset(dat.merged,month==7&state.fips==36)$t2m.30yr.mean)

    p1 = ggplot(data=subset(dat.merged,month==7&state.fips==36&year>=1980),aes(x=year,y=(value))) +
    geom_point(size=4,color='Blue') +
    geom_hline(yintercept=mean.value, linetype=2,alpha=0.5,color='red') +
    annotate('text',x=1980+7,y=mean.value+0.05,label='Long-term mean temperature') +
    xlab('Year') +
    scale_y_continuous(name=expression(paste("Temperature (",degree,"C)"))) +
    scale_colour_manual(values=colorRampPalette(rev(brewer.pal(12,"RdYlGn")[c(1:5,7:9)]))(colourCount),guide = FALSE) +
    theme_bw() +
    theme(panel.grid.major = element_blank(),text = element_text(size = 15),
    axis.ticks.x=element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))

    p2 = ggplot(data=subset(dat.merged,month==7&state.fips==36&year>=1980),aes(x=year,y=variable)) +
    geom_point(size=4,color='Blue') +
    # geom_line(arrow=arrow(length=unit(0.30,'cm'),ends='first',type='closed'))+
    geom_segment(aes(x=year,xend=year,y=0,yend=variable))+
    geom_hline(yintercept=0, linetype=2,alpha=0.5) +
    xlab('Year') +
    scale_y_continuous(name=expression(paste("Temperature anomaly (",degree,"C)"))) +
    scale_colour_manual(values=colorRampPalette(rev(brewer.pal(12,"RdYlGn")[c(1:5,7:9)]))(colourCount),guide = FALSE) +
    theme_bw() +
    theme(panel.grid.major = element_blank(),text = element_text(size = 15),
    axis.ticks.x=element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))

    grid.arrange(p1,p2,nrow=1)

dev.off()