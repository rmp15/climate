rm(list=ls())

library(ggplot2)
library(RColorBrewer)
library(gridExtra)
library(scales)
library(plyr)

# arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)
year.start <- as.numeric(args[1])
year.end <- as.numeric(args[2])
dname <- as.character(args[3])
metric <- as.character(args[4])

# create output directory
output.dir = paste0("../../output/anomaly_summaries/",dname,'/',metric,'/')
ifelse(!dir.exists(output.dir), dir.create(output.dir,recursive=TRUE), FALSE)

# load state fips lookup code
fips.lookup <- read.csv('~/git/mortality/USA/state/data/fips_lookup/name_fips_lookup.csv')
fips.lookup = fips.lookup[!(fips.lookup$fips%in%c(2,15)),]
month.lookup <- c('January','February','March','April','May','June','July','August','September','October','November','December')
month.short <- c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')

# load dataset with population weighted temperature values
dat <- readRDS(paste0('../../output/metrics_development/',dname,'/',metric,'_',dname,'/state_weighted_summary_',metric,'_',dname,'_',year.start,'_',year.end,'.rds'))
dat$state.fips <- as.numeric(dat$state.fips)
dat <- merge(dat,fips.lookup,by.x='state.fips',by.y='fips',all.x=1)
names(dat)[grep(dname,names(dat))] <- 'variable'

# get rid of Hawaii and Alaska
dat = subset(dat,!(state.fips%in%c(2,15)))

# restrict to years of study
dat = subset(dat,year>1979&year<2017)

# isolate a single age-sex combination
dat = subset(dat,age==65&sex==1)

# bespoke colourway
colorway = c("navy","deepskyblue2","deepskyblue3","gold","orange","red","darkred")

############# STANDARD DEVIATION #############

# process statistics
dat.sd = ddply(dat,.(full_name,month),summarise,sd=sd(variable))

# dots
pdf(paste0(output.dir,'dots_plot_',year.start,'_',year.end,'_',dname,'_sd.pdf'),paper='a4r',height=0,width=0)
ggplot(data=dat.sd) +
    geom_point(aes(x=month,y=sd)) +
    scale_x_continuous(breaks=c(seq(1,12,by=1)),labels=month.short)   +
    xlab("Month") + ylab('Standard deviation of anomaly') +
    theme_bw() + theme(panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
    plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))
dev.off()

# heat map
pdf(paste0(output.dir,'heatmap_plot_',year.start,'_',year.end,'_',dname,'_sd.pdf'),paper='a4r',height=0,width=0)
ggplot(data=dat.sd)+
    geom_tile(aes(x=month,y=full_name,fill=sd)) +
    coord_fixed() +
    scale_fill_gradientn(colours=colorway,
    breaks=seq(-0.5,5,0.5),
    # na.value = "grey98",limits = c(-0.027, 0.027),
    # breaks=c(-0.08, -0.06, -0.04, -0.02, 0, 0.02, 0.04, 0.06, 0.08),
    # na.value = "grey98",limits = c(-0.1, 0.1),
    guide = guide_legend(nrow = 1,title = paste0("Standard deviation of anomalies"))) +
    guides(fill = guide_colorbar(barwidth = 30, barheight = 1,title = paste0("Standard deviation"))) +
    scale_x_continuous(breaks=c(seq(1,12,by=1)),labels=month.short)   +
    coord_flip() +
    xlab("Month") + ylab('State') +
    theme_bw() + theme(panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
    plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))
dev.off()

############# POSITIVE ANOMALY #############

# process statistics
dat.anom.pos = ddply(subset(dat,variable>=0),.(full_name,month),summarise,anom.pos=mean(variable))

# dots
pdf(paste0(output.dir,'dots_plot_',year.start,'_',year.end,'_',dname,'_anom_pos.pdf'),paper='a4r',height=0,width=0)
ggplot(data=dat.anom.pos) +
    geom_point(aes(x=month,y=anom.pos)) +
    scale_x_continuous(breaks=c(seq(1,12,by=1)),labels=month.short)   +
    xlab("Month") + ylab('Average positive anomaly') +
    theme_bw() + theme(panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
    plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))
dev.off()

# heat map
pdf(paste0(output.dir,'heatmap_plot_',year.start,'_',year.end,'_',dname,'_anom_pos.pdf'),paper='a4r',height=0,width=0)
ggplot(data=dat.anom.pos)+
    geom_tile(aes(x=month,y=full_name,fill=anom.pos)) +
    coord_fixed() +
    scale_fill_gradientn(colours=colorway,
    breaks=seq(-0.5,5,0.5),
    # na.value = "grey98",limits = c(-0.027, 0.027),
    # breaks=c(-0.08, -0.06, -0.04, -0.02, 0, 0.02, 0.04, 0.06, 0.08),
    # na.value = "grey98",limits = c(-0.1, 0.1),
    guide = guide_legend(nrow = 1,title = paste0("Average positive anomaly"))) +
    guides(fill = guide_colorbar(barwidth = 30, barheight = 1,title = paste0("Average positive anomaly"))) +
    scale_x_continuous(breaks=c(seq(1,12,by=1)),labels=month.short)   +
    coord_flip() +
    xlab("Month") + ylab('State') +
    theme_bw() + theme(panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
    plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))
dev.off()

############# NEGATIVE ANOMALY #############

# process statistics
dat.anom.neg = ddply(subset(dat,variable<0),.(full_name,month),summarise,anom.neg=mean(variable))

# dots
pdf(paste0(output.dir,'dots_plot_',year.start,'_',year.end,'_',dname,'_anom_neg.pdf'),paper='a4r',height=0,width=0)
ggplot(data=dat.anom.neg) +
    geom_point(aes(x=month,y=abs(anom.neg))) +
    scale_x_continuous(breaks=c(seq(1,12,by=1)),labels=month.short)   +
    xlab("Month") + ylab('Average negative anomaly') +
    theme_bw() + theme(panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
    plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))
dev.off()

# heat map
pdf(paste0(output.dir,'heatmap_plot_',year.start,'_',year.end,'_',dname,'_anom_neg.pdf'),paper='a4r',height=0,width=0)
ggplot(data=dat.anom.neg)+
    geom_tile(aes(x=month,y=full_name,fill=abs(anom.neg)))
    coord_fixed() +
    scale_fill_gradientn(colours=colorway,
    breaks=seq(-0.5,5,0.5),
    # na.value = "grey98",limits = c(-0.027, 0.027),
    # breaks=c(-0.08, -0.06, -0.04, -0.02, 0, 0.02, 0.04, 0.06, 0.08),
    # na.value = "grey98",limits = c(-0.1, 0.1),
    guide = guide_legend(nrow = 1,title = paste0("Average negative anomaly"))) +
    guides(fill = guide_colorbar(barwidth = 30, barheight = 1,title = paste0("Average negative anomaly"))) +
    scale_x_continuous(breaks=c(seq(1,12,by=1)),labels=month.short)   +
    coord_flip() +
    xlab("Month") + ylab('State') +
    theme_bw() + theme(panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
    plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))
dev.off()

############# ABSOLUTE ANOMALY #############

# process statistics
dat.anom.abs = ddply(subset(dat),.(full_name,month),summarise,anom.abs=mean(abs(variable)))

# dots
pdf(paste0(output.dir,'dots_plot_',year.start,'_',year.end,'_',dname,'_anom_abs.pdf'),paper='a4r',height=0,width=0)
ggplot(data=dat.anom.abs) +
    geom_point(aes(x=month,y=abs(anom.abs))) +
    scale_x_continuous(breaks=c(seq(1,12,by=1)),labels=month.short)   +
    xlab("Month") + ylab('Average absolute anomaly') +
    theme_bw() + theme(panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
    plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))
dev.off()

# heat map
pdf(paste0(output.dir,'heatmap_plot_',year.start,'_',year.end,'_',dname,'_anom_abs.pdf'),paper='a4r',height=0,width=0)
ggplot(data=dat.anom.abs)+
    geom_tile(aes(x=month,y=full_name,fill=abs(anom.abs))) +
    coord_fixed() +
    scale_fill_gradientn(colours=colorway,
    breaks=seq(-0.5,5,0.5),
    # na.value = "grey98",limits = c(-0.027, 0.027),
    # breaks=c(-0.08, -0.06, -0.04, -0.02, 0, 0.02, 0.04, 0.06, 0.08),
    # na.value = "grey98",limits = c(-0.1, 0.1),
    guide = guide_legend(nrow = 1,title = paste0("Average absolute anomaly"))) +
    guides(fill = guide_colorbar(barwidth = 30, barheight = 1,title = paste0("Average absolute anomaly"))) +
    scale_x_continuous(breaks=c(seq(1,12,by=1)),labels=month.short)   +
    coord_flip() +
    xlab("Month") + ylab('State') +
    theme_bw() + theme(panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
    plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))
dev.off()

############# ANY ANOMALY #############

# process statistics
dat = subset(dat,year>1979&year<2010&state.fips!=32)
dat.anom = ddply(subset(dat),.(full_name,month),summarise,anom=mean(variable))

# dots
pdf(paste0(output.dir,'dots_plot_',year.start,'_',year.end,'_',dname,'_anom.pdf'),paper='a4r',height=0,width=0)
ggplot(data=dat.anom) +
    geom_point(aes(x=month,y=abs(anom))) +
    scale_x_continuous(breaks=c(seq(1,12,by=1)),labels=month.short)   +
    xlab("Month") + ylab('Average anomaly') +
    ylim(c(0,2)) +
    theme_bw() + theme(panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
    plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))
dev.off()

# heat map
pdf(paste0(output.dir,'heatmap_plot_',year.start,'_',year.end,'_',dname,'_anom.pdf'),paper='a4r',height=0,width=0)
ggplot(data=dat.anom)+
    geom_tile(aes(x=month,y=full_name,fill=abs(anom))) +
    coord_fixed() +
    scale_fill_gradientn(colours=colorway,
    breaks=seq(0,5,0.5),
    # na.value = "grey98",limits = c(-0.027, 0.027),
    # breaks=c(-0.08, -0.06, -0.04, -0.02, 0, 0.02, 0.04, 0.06, 0.08),
    # na.value = "grey98",limits = c(-0.1, 0.1),
    guide = guide_legend(nrow = 1,title = paste0("Average anomaly"))) +
    guides(fill = guide_colorbar(barwidth = 30, barheight = 1,title = paste0("Average anomaly"))) +
    scale_x_continuous(breaks=c(seq(1,12,by=1)),labels=month.short)   +
    coord_flip() +
    xlab("Month") + ylab('State') +
    theme_bw() + theme(panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
    plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))
dev.off()


