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
ifelse(!dir.exists("../../output/plots_against_time_one_state_all_months"), dir.create("../../output/plots_against_time_one_state_all_months"), FALSE)
ifelse(!dir.exists(paste0("../../output/plots_against_time_one_state_all_months/",dname)), dir.create(paste0("../../output/plots_against_time_one_state_all_months/",dname)), FALSE)
ifelse(!dir.exists(paste0("../../output/plots_against_time_one_state_all_months/",dname,'/',metric)), dir.create(paste0("../../output/plots_against_time_one_state_all_months/",dname,'/',metric)), FALSE)
ifelse(!dir.exists(paste0("../../output/plots_against_time_one_state_all_months/schematics/")), dir.create(paste0("../../output/plots_against_time_one_state_all_months/schematics/")), FALSE)

# load state fips lookup code
fips.lookup <- read.csv('~/git/mortality/USA/state/data/fips_lookup/name_fips_lookup.csv')
fips.lookup = fips.lookup[!(fips.lookup$fips%in%c(2,15)),]
month.lookup <- c('January','February','March','April','May','June','July','August','September','October','November','December')
month.short <- c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')

# load dataset with population weighted temperature values
dat <- readRDS(paste0('../../output/metrics_development_era5/',dname,'/',metric,'_',dname,'/state_weighted_summary_',metric,'_',dname,'_',year.start,'_',year.end,'.rds'))
dat$state.fips <- as.numeric(dat$state.fips)
dat <- merge(dat,fips.lookup,by.x='state.fips',by.y='fips',all.x=1)
names(dat)[grep(dname,names(dat))] <- 'variable'

# get rid of Hawaii and Alaska
dat = subset(dat,!(state.fips%in%c(2,15)))

# plot facetting by state and colouring by age
age.filter <- unique(dat$age)
colourCount <- length(age.filter)

# isolate a single age-sex combination
dat = subset(dat,age==55&sex==1)

# 1. over time by month and state
# pdf(paste0('../../output/plots_against_time/',dname,'/',metric,'/',dname,'_',metric,'_',year.start,'_',year.end,'.pdf'),height=0,width=0,paper='a4r')
# for (i in c(1:12)){
#     print(ggplot(data=subset(dat,month==i),aes(x=year,y=variable)) +
#     geom_point(aes(color=as.factor(month))) +
#     geom_smooth(method='lm') +
#     geom_hline(yintercept=0, linetype=2,alpha=0.5) +
#     xlab('Year') +
#     ylab(paste0(dname,'.',metric)) +
#     facet_wrap(~full_name) +
#     scale_colour_manual(values=colorRampPalette(rev(brewer.pal(12,"RdYlGn")[c(1:5,7:9)]))(colourCount),guide = guide_legend(title = 'Month')) +
#     ggtitle(month.short) +
#     scale_x_discrete(labels=c(1:12)) +
#     theme_bw())
# }
# dev.off()

var <- paste0('mean_',dname)
metric.2 = 'mean'
year.start.2 = 1980
year.end.2 = 2017

# load long-term average values
dat.normal = readRDS(paste0("../../output/multiyear_normals_era5/",dname,"/",metric.2,'/state_longterm_nonnormals_',var,'_',year.start.2,'_',year.end.2,'.rds'))
dat.normal = subset(dat.normal,!(state.fips%in%c(2,15)))
dat.normal = subset(dat.normal,age==55&sex==1)
dat.normal$state.fips= as.numeric(dat.normal$state.fips)

dat.merged = merge(dat,dat.normal)
dat.merged$value=with(dat.merged,variable+t2m.38yr.mean)

plot_anomaly=function(state,min=-3,max=3){
# isolate New York in July and California in January?

dat.merged <- dat.merged[order(dat.merged$year,dat.merged$month),]

dat.merged <- dat.merged[order(dat.merged$year,dat.merged$month),]

    mean.value.1 = unique(subset(dat.merged[order(dat.merged$month),], state.fips==state)$t2m.38yr.mean)

    p1 = ggplot() +

    geom_point(data=subset(dat.merged,month==5&state.fips==state&year>=1980),aes(x=year,y=(value)),size=4,color='green') +
    geom_line(data=subset(dat.merged,month==5&state.fips==state&year>=1980),aes(x=year,y=(value)),color='green',linetype='dashed',alpha=0.5) +
    geom_hline(yintercept=mean.value.1[5], linetype='dotted',,alpha=0.5,color='green') +

    geom_point(data=subset(dat.merged,month==6&state.fips==state&year>=1980),aes(x=year,y=(value)),size=4,color='orange') +
    geom_line(data=subset(dat.merged,month==6&state.fips==state&year>=1980),aes(x=year,y=(value)),color='orange',linetype='dashed',alpha=0.5) +
    geom_hline(yintercept=mean.value.1[6], linetype='dotted',,alpha=0.5,color='orange') +

    geom_point(data=subset(dat.merged,month==7&state.fips==state&year>=1980),aes(x=year,y=(value)),size=4,color='red') +
    geom_line(data=subset(dat.merged,month==7&state.fips==state&year>=1980),aes(x=year,y=(value)),color='red',linetype='dashed',alpha=0.5) +
    geom_hline(yintercept=mean.value.1[7], linetype='dotted',,alpha=0.5,color='red') +

    geom_point(data=subset(dat.merged,month==8&state.fips==state&year>=1980),aes(x=year,y=(value)),size=4,color='yellow') +
    geom_line(data=subset(dat.merged,month==8&state.fips==state&year>=1980),aes(x=year,y=(value)),color='orange',linetype='dashed',alpha=0.5) +
    geom_hline(yintercept=mean.value.1[8], linetype='dotted',,alpha=0.5,color='orange') +

    geom_point(data=subset(dat.merged,month==9&state.fips==state&year>=1980),aes(x=year,y=(value)),size=4,color='blue') +
    geom_line(data=subset(dat.merged,month==9&state.fips==state&year>=1980),aes(x=year,y=(value)),color='green',linetype='dashed',alpha=0.5) +
    geom_hline(yintercept=mean.value.1[9], linetype='dotted',,alpha=0.5,color='green') +

    geom_hline(yintercept=0, linetype=2,alpha=0,color='Black') +

    # annotate('text',x=1980+7,y=mean.value.1+1,label='New York') +
    xlab('Year') +# xlim(c(-1, 30)) +
    scale_y_continuous(name=expression(paste("Temperature (",degree,"C)"))) +
    scale_colour_manual(values=colorRampPalette(rev(brewer.pal(12,"RdYlGn")[c(1:5,7:9)]))(colourCount),guide = FALSE) +
    theme_bw() +
    theme(panel.grid.major = element_blank(),text = element_text(size = 15),
    axis.ticks.x=element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))

    # p2 = ggplot(data=subset(dat.merged,month==7&state.fips==36&year>=1980),aes(x=year,y=(value))) +
    # geom_point(size=4,color='Blue') +
    # geom_hline(yintercept=mean.value.1, linetype=2,alpha=0.5,color='red') +
    # annotate('text',x=1980+7,y=mean.value+0.05,label='Long-term mean temperature') +
    # xlab('Year') +
    # scale_y_continuous(name=expression(paste("Temperature (",degree,"C)"))) +
    # scale_colour_manual(values=colorRampPalette(rev(brewer.pal(12,"RdYlGn")[c(1:5,7:9)]))(colourCount),guide = FALSE) +
    # theme_bw() +
    # theme(panel.grid.major = element_blank(),text = element_text(size = 15),
    # axis.ticks.x=element_blank(),
    # panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    # panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    # legend.position = 'bottom',legend.justification='center',
    # legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))

    p3 = ggplot(data=subset(dat.merged,month==month1&state.fips==state1&year>=1980),aes(x=year,y=variable)) +
    geom_point(size=4,color='Red') +
    geom_segment(aes(x=year,xend=year,y=0,yend=variable))+
    geom_hline(yintercept=0, linetype=2,alpha=0.5) +
    xlab('Year') +
    scale_y_continuous(name=expression(paste("Temperature anomaly (",degree,"C)")),limits=c(min,max)) +
    scale_colour_manual(values=colorRampPalette(rev(brewer.pal(12,"RdYlGn")[c(1:5,7:9)]))(colourCount),guide = FALSE) +
    ggtitle(paste0(as.character(fips.lookup[fips.lookup$fips == state1,]$full_name),' ', month.lookup[month1],' anomalies')) +
    theme_bw() +
    theme(panel.grid.major = element_blank(),text = element_text(size = 15),
    axis.ticks.x=element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))

    p4 = ggplot(data=subset(dat.merged,month==month2&state.fips==state2&year>=1980),aes(x=year,y=variable)) +
    geom_point(size=4,color='Blue') +
    geom_segment(aes(x=year,xend=year,y=0,yend=variable))+
    geom_hline(yintercept=0, linetype=2,alpha=0.5) +
    xlab('Year') +
    scale_y_continuous(name=expression(paste("Temperature anomaly (",degree,"C)")),limits=c(min,max)) +
    scale_colour_manual(values=colorRampPalette(rev(brewer.pal(12,"RdYlGn")[c(1:5,7:9)]))(colourCount),guide = FALSE) +
    ggtitle(paste0(as.character(fips.lookup[fips.lookup$fips == state2,]$full_name),' ', month.lookup[month2],' anomalies')) +
    theme_bw() +
    theme(panel.grid.major = element_blank(),text = element_text(size = 15),
    axis.ticks.x=element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))

    grid.arrange(p1,p3,p4,widths=c(1,1),layout_matrix=rbind(c(1,2),c(1,3)),nrow=2)

}

# isolate 2 months and 2 states
pdf(paste0('../../output/plots_against_time/schematics/new_york_california_schematic.pdf'),height=0,width=0,paper='a4r')
plot_anomaly(7,7,6,36,-5,5)
dev.off()

# isolate 2 months and 2 states
pdf(paste0('../../output/plots_against_time/schematics/florida_washington_july_schematic.pdf'),height=0,width=0,paper='a4r')
plot_anomaly(7,7,12,53,-4,3)
dev.off()

# isolate 2 months and 2 states
pdf(paste0('../../output/plots_against_time/schematics/florida_minnesota_july_schematic.pdf'),height=0,width=0,paper='a4r')
plot_anomaly(7,7,12,27,-5,3)
dev.off()

# # isolate 2 months and 2 states
# states = unique(fips.lookup$fips)
# pdf(paste0('../../output/plots_against_time/schematics/figure3_schematic_options.pdf'),height=0,width=0,paper='a4r')
# for (i in c(12,6,36,53)){
#     for (j in c(12,6,36,53)) {
#             for (k in c(1:12)) {
#                 if(i!=j){
#                 plot_anomaly(k,k,i,j,-5,5)
#                 }
#     }}}
# dev.off()