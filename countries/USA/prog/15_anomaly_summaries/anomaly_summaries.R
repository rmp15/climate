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
dat <- readRDS(paste0('../../output/metrics_development_era5/',dname,'/',metric,'_',dname,'/state_weighted_summary_',metric,'_',dname,'_',year.start,'_',year.end,'.rds'))
dat$state.fips <- as.numeric(dat$state.fips)
dat <- merge(dat,fips.lookup,by.x='state.fips',by.y='fips',all.x=1)
names(dat)[grep(dname,names(dat))] <- 'variable'

# get rid of Hawaii and Alaska
dat = subset(dat,!(state.fips%in%c(2,15)))

# restrict to years of study
# dat = subset(dat,year>1979&year<=2017)

# isolate a single age-sex combination
dat = subset(dat,age==65&sex==1)

# to correct colours
f <- function(pal) brewer.pal(brewer.pal.info[pal, "maxcolors"], pal)
mycols <- c(f("Dark2"), f("Set1")[1:8], f("Set2"), f("Set3"),"#89C5DA", "#DA5724", "#74D944", "#CE50CA", "#3F4921", "#C0717C", "#CBD588", "#5F7FC7", "#673770", "#D3D93E", "#38333E", "#508578", "#D7C1B1", "#689030", "#AD6F3B", "#CD9BCD", "#D14285", "#6DDE88", "#652926", "#7FDCC0", "#C84248", "#8569D5", "#5E738F", "#D1A33D", "#8A7C64", "#599861" )
#to make picking the number of the colour you want easier:
# plot(1:length(mycols),col=mycols[1:length(mycols)],cex=4,pch=20); abline(v=c(10,20,30,40,50,60))

# bespoke colourway (old)
# colorway = c("navy","deepskyblue2","deepskyblue3","gold","orange","red","darkred")

colorway = mycols[c(    33,  # Low
                        33,  # Low-mid
                        6,  # Mid-low (28 other possibility)
                        6,  # Mid     (28 other possibility)
                        3,  # Mid-high
                        3,  # High-mid
                        40)] # High

############# ABSOLUTE ANOMALY #############

# process statistics
dat.anom.abs = ddply(subset(dat),.(full_name,month),summarise,anom.abs=mean(abs(variable)))
write.csv(dat.anom.abs,paste0(output.dir,'anom_abs_',year.start,'_',year.end,'_',dname,'_anom_abs.csv'))

# heat map square
pdf(paste0(output.dir,'heatmap_square_plot_',year.start,'_',year.end,'_',dname,'_anom_abs.pdf'),paper='a4',height=0,width=0)
ggplot(data=dat.anom.abs)+
    geom_tile(aes(x=month,y=full_name,fill=anom.abs)) +
    coord_equal() +
    scale_fill_gradientn(colours=c("#ffffe5","#ffffe5","#fe9929","#fe9929","#993404","#993404","#662506"),
    breaks=seq(0,3.5,1), limits = c(0,3.5),
    guide = guide_legend(nrow = 1,title = paste0("Average absolute anomaly"))) +
    guides(fill = guide_colorbar(title.position="top",barwidth = 10, barheight = 1,title = expression("Average size of anomaly " (degree*C)))) +
    scale_x_continuous(breaks=c(seq(1,12,by=1)),labels=month.short) +
    # coord_flip() +
    xlab("Month") + ylab('') +
    theme_bw() + theme(panel.grid.major = element_blank(),axis.text.x = element_text(angle=90, vjust=0.5),
    plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="white", size=.5, linetype="dotted"))
dev.off()

pdf(paste0(output.dir,'heatmap_square_plot_uneven_scale_',year.start,'_',year.end,'_',dname,'_anom_abs.pdf'),paper='a4',height=0,width=0)
ggplot(data=dat.anom.abs)+
    geom_tile(aes(x=month,y=full_name,fill=anom.abs)) +
    coord_equal() +
    scale_fill_gradientn(colours = c("#ffffe5","#fff7bc","#fee391","#fec44f","#fe9929","#ec7014","#cc4c02","#993404","#993404","#662506"),
    values = c(0,0.2,0.25,0.3,0.35,0.4,0.5,0.7,0.8,0.9,1),
    breaks=seq(0,3.5,1), limits = c(0,3.5),
    guide = guide_legend(nrow = 1,title = paste0("Average absolute anomaly"))) +
    guides(fill = guide_colorbar(title.position="top",barwidth = 10, barheight = 1,title = expression("Average size of anomaly " (degree*C)))) +
    scale_x_continuous(breaks=c(seq(1,12,by=1)),labels=month.short) +
    # coord_flip() +
    xlab("Month") + ylab('') +
    theme_bw() + theme(panel.grid.major = element_blank(),axis.text.x = element_text(angle=90, vjust=0.5),
    plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="white", size=.5, linetype="dotted"))
dev.off()

pdf(paste0(output.dir,'heatmap_square_plot_uneven_scale_alt_',year.start,'_',year.end,'_',dname,'_anom_abs.pdf'),paper='a4',height=0,width=0)
ggplot(data=dat.anom.abs)+
    geom_tile(aes(x=month,y=full_name,fill=anom.abs)) +
    coord_equal() +
    scale_fill_gradientn(colours = c("#3f007d","yellow3","gold","orange","brown3","brown4"),
    values = c(0,0.2,0.25,0.3,0.35,0.4,0.5,0.7,0.8,0.9,1),
    breaks=seq(0,3.5,1), limits = c(0,3.5),
    guide = guide_legend(nrow = 1,title = paste0("Average absolute anomaly"))) +
    guides(fill = guide_colorbar(title.position="top",barwidth = 10, barheight = 1,title = expression("Average size of anomaly " (degree*C)))) +
    scale_x_continuous(breaks=c(seq(1,12,by=1)),labels=month.short) +
    # coord_flip() +
    xlab("Month") + ylab('') +
    theme_bw() + theme(panel.grid.major = element_blank(),axis.text.x = element_text(angle=90, vjust=0.5),
    plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="white", size=.5, linetype="dotted"))
dev.off()

pdf(paste0(output.dir,'heatmap_square_plot_uneven_scale_alt3_',year.start,'_',year.end,'_',dname,'_anom_abs.pdf'),paper='a4',height=0,width=0)
ggplot(data=dat.anom.abs)+
    geom_tile(aes(x=month,y=full_name,fill=anom.abs)) +
    coord_equal() +
    scale_fill_gradientn(colours = c('#fff7f3','#fde0dd','#fcc5c0','#fa9fb5','#f768a1','#dd3497','#ae017e','#7a0177','#49006a'),
    values = c(0,0.2,0.25,0.3,0.35,0.4,0.5,0.7,0.8,0.9,1),
    breaks=seq(0,3.5,1), limits = c(0,3.5),
    guide = guide_legend(nrow = 1,title = paste0("Average absolute anomaly"))) +
    guides(fill = guide_colorbar(title.position="top",barwidth = 10, barheight = 1,title = expression("Average size of anomaly " (degree*C)))) +
    scale_x_continuous(breaks=c(seq(1,12,by=1)),labels=month.short) +
    # coord_flip() +
    xlab("Month") + ylab('') +
    theme_bw() + theme(panel.grid.major = element_blank(),axis.text.x = element_text(angle=90, vjust=0.5),
    plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="white", size=.5, linetype="dotted"))
dev.off()

pdf(paste0(output.dir,'heatmap_square_plot_uneven_scale_alt4_',year.start,'_',year.end,'_',dname,'_anom_abs.pdf'),paper='a4',height=0,width=0)
ggplot(data=dat.anom.abs)+
    geom_tile(aes(x=month,y=full_name,fill=anom.abs)) +
    coord_equal() +
    scale_fill_gradientn(colours = c("#ffffe5","#fff7bc","#fee391","#fec44f","#fe9929","#ec7014","#662506",'#dd3497','#ae017e','#7a0177','#49006a'),
    values = c(0,0.2,0.25,0.3,0.35,0.4,0.5,0.6,0.65,0.7,0.8,1),
    breaks=seq(0,3.5,1), limits = c(0,3.5),
    guide = guide_legend(nrow = 1,title = paste0("Average absolute anomaly"))) +
    guides(fill = guide_colorbar(title.position="top",barwidth = 10, barheight = 1,title = expression("Average size of anomaly " (degree*C)))) +
    scale_x_continuous(breaks=c(seq(1,12,by=1)),labels=month.short) +
    # coord_flip() +
    xlab("Month") + ylab('') +
    theme_bw() + theme(panel.grid.major = element_blank(),axis.text.x = element_text(angle=90, vjust=0.5),
    plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="white", size=.5, linetype="dotted"))
dev.off()

pdf(paste0(output.dir,'heatmap_square_plot_uneven_scale_alt5_',year.start,'_',year.end,'_',dname,'_anom_abs.pdf'),paper='a4',height=0,width=0)
ggplot(data=dat.anom.abs)+
    geom_tile(aes(x=month,y=full_name,fill=anom.abs)) +
    coord_equal() +
    scale_fill_gradientn(colours = c("#ffffe5","#fff7bc","#fee391","#fec44f","#fe9929","#ec7014","#662506",'#1d91c0','#225ea8','#253494','#081d58'),
    values = c(0,0.2,0.25,0.3,0.35,0.4,0.5,0.6,0.65,0.7,0.8,1),
    breaks=seq(0,3.5,1), limits = c(0,3.5),
    guide = guide_legend(nrow = 1,title = paste0("Average absolute anomaly"))) +
    guides(fill = guide_colorbar(title.position="top",barwidth = 10, barheight = 1,title = expression("Average size of anomaly " (degree*C)))) +
    scale_x_continuous(breaks=c(seq(1,12,by=1)),labels=month.short) +
    # coord_flip() +
    xlab("Month") + ylab('') +
    theme_bw() + theme(panel.grid.major = element_blank(),axis.text.x = element_text(angle=90, vjust=0.5),
    plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="white", size=.5, linetype="dotted"))
dev.off()

pdf(paste0(output.dir,'heatmap_square_plot_uneven_scale_alt6_',year.start,'_',year.end,'_',dname,'_anom_abs.pdf'),paper='a4',height=0,width=0)
ggplot(data=dat.anom.abs)+
    geom_tile(aes(x=month,y=full_name,fill=anom.abs)) +
    coord_equal() +
    scale_fill_gradientn(colours = c("#fff5eb","#fee6ce","#fdd0a2","#fdae6b","#fd8d3c","#f16913","#d94801",'#a63603','#7f2704'),
    values = c(0,0.2,0.25,0.3,0.35,0.4,0.5,0.6,0.65,0.7,0.8,1),
    breaks=seq(0,3.5,1), limits = c(0,3.5),
    guide = guide_legend(nrow = 1,title = paste0("Average absolute anomaly"))) +
    guides(fill = guide_colorbar(title.position="top",barwidth = 10, barheight = 1,title = expression("Average size of anomaly " (degree*C)))) +
    scale_x_continuous(breaks=c(seq(1,12,by=1)),labels=month.short) +
    # coord_flip() +
    xlab("Month") + ylab('') +
    theme_bw() + theme(panel.grid.major = element_blank(),axis.text.x = element_text(angle=90, vjust=0.5),
    plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="white", size=.5, linetype="dotted"))
dev.off()

pdf(paste0(output.dir,'heatmap_square_plot_uneven_scale_alt7_',year.start,'_',year.end,'_',dname,'_anom_abs.pdf'),paper='a4',height=0,width=0)
ggplot(data=dat.anom.abs)+
    geom_tile(aes(x=month,y=full_name,fill=anom.abs)) +
    coord_equal() +
    scale_fill_gradientn(colours = c('#fff5eb',"#fee6ce","#fdd0a2","#fdae6b","#fd8d3c","#f16913","#d94801",'#a63603','#7f2704',"brown","coral4"),
    values = c(0,0.2,0.25,0.3,0.35,0.4,0.5,0.6,0.65,0.7,0.8,1),
    breaks=seq(0,3.5,1), limits = c(0,3.5),
    guide = guide_legend(nrow = 1,title = paste0("Average absolute anomaly"))) +
    guides(fill = guide_colorbar(title.position="top",barwidth = 10, barheight = 1,title = expression("Average size of anomaly " (degree*C)))) +
    scale_x_continuous(breaks=c(seq(1,12,by=1)),labels=month.short) +
    # coord_flip() +
    xlab("Month") + ylab('') +
    theme_bw() + theme(panel.grid.major = element_blank(),axis.text.x = element_text(angle=90, vjust=0.5),
    plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="white", size=.5, linetype="dotted"))
dev.off()

pdf(paste0(output.dir,'heatmap_square_plot_uneven_scale_alt8_',year.start,'_',year.end,'_',dname,'_anom_abs.pdf'),paper='a4',height=0,width=0)
ggplot(data=dat.anom.abs)+
    geom_tile(aes(x=month,y=full_name,fill=anom.abs)) +
    coord_equal() +
    scale_fill_gradientn(colours = c('#fff5eb',"#fee6ce","#fdd0a2","#fdae6b","#fd8d3c","#f16913","#d94801",'#a63603','#7f2704',"brown","coral4","sienna4","chocolate4"),
    values = c(0,0.2,0.25,0.3,0.35,0.4,0.5,0.6,0.65,0.7,0.8,0.9,1),
    breaks=seq(0,3.5,1), limits = c(0,3.5),
    guide = guide_legend(nrow = 1,title = paste0("Average absolute anomaly"))) +
    guides(fill = guide_colorbar(title.position="top",barwidth = 10, barheight = 1,title = expression("Average size of anomaly " (degree*C)))) +
    scale_x_continuous(breaks=c(seq(1,12,by=1)),labels=month.short) +
    # coord_flip() +
    xlab("Month") + ylab('') +
    theme_bw() + theme(panel.grid.major = element_blank(),axis.text.x = element_text(angle=90, vjust=0.5),
    plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="white", size=.5, linetype="dotted"))
dev.off()

pdf(paste0(output.dir,'heatmap_square_plot_uneven_scale_alt9_',year.start,'_',year.end,'_',dname,'_anom_abs.pdf'),paper='a4',height=0,width=0)
ggplot(data=dat.anom.abs)+
    geom_tile(aes(x=month,y=full_name,fill=anom.abs)) +
    coord_equal() +
    scale_fill_gradientn(colours = c('goldenrod',"goldenrod1","goldenrod2","goldenrod3","goldenrod4","firebrick","firebrick1",'firebrick2','firebrick3',"firebrick4","chocolate4","coral4"),
    values = c(0,0.2,0.25,0.3,0.35,0.4,0.5,0.6,0.65,0.7,0.8,0.9,1),
    breaks=seq(0,3.5,1), limits = c(0,3.5),
    guide = guide_legend(nrow = 1,title = paste0("Average absolute anomaly"))) +
    guides(fill = guide_colorbar(title.position="top",barwidth = 10, barheight = 1,title = expression("Average size of anomaly " (degree*C)))) +
    scale_x_continuous(breaks=c(seq(1,12,by=1)),labels=month.short) +
    # coord_flip() +
    xlab("Month") + ylab('') +
    theme_bw() + theme(panel.grid.major = element_blank(),axis.text.x = element_text(angle=90, vjust=0.5),
    plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="white", size=.5, linetype="dotted"))
dev.off()

pdf(paste0(output.dir,'heatmap_square_plot_uneven_scale_alt10_',year.start,'_',year.end,'_',dname,'_anom_abs.pdf'),paper='a4',height=0,width=0)
ggplot(data=dat.anom.abs)+
    geom_tile(aes(x=month,y=full_name,fill=anom.abs)) +
    coord_equal() +
    scale_fill_gradientn(colours = c('lightgoldenrod',"lightgoldenrod2","lightgoldenrod3","lightgoldenrod4",'goldenrod',"goldenrod1","goldenrod2","goldenrod3","goldenrod4","chocolate4","coral4"),
    values = c(0,0.2,0.25,0.3,0.35,0.4,0.5,0.6,0.65,0.7,0.8,0.9,1),
    breaks=seq(0,3.5,1), limits = c(0,3.5),
    guide = guide_legend(nrow = 1,title = paste0("Average absolute anomaly"))) +
    guides(fill = guide_colorbar(title.position="top",barwidth = 10, barheight = 1,title = expression("Average size of anomaly " (degree*C)))) +
    scale_x_continuous(breaks=c(seq(1,12,by=1)),labels=month.short) +
    # coord_flip() +
    xlab("Month") + ylab('') +
    theme_bw() + theme(panel.grid.major = element_blank(),axis.text.x = element_text(angle=90, vjust=0.5),
    plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="white", size=.5, linetype="dotted"))
dev.off()

pdf(paste0(output.dir,'heatmap_square_plot_uneven_scale_alt10_',year.start,'_',year.end,'_',dname,'_anom_abs.pdf'),paper='a4',height=0,width=0)
ggplot(data=dat.anom.abs)+
    geom_tile(aes(x=month,y=full_name,fill=anom.abs)) +
    coord_equal() +
    scale_fill_gradientn(colours = c('goldenrod',"goldenrod1","goldenrod2","goldenrod3","goldenrod4",'gold','gold1','gold2','gold3','gold4',"chocolate4","coral4"),
    values = c(0,0.2,0.25,0.3,0.35,0.4,0.5,0.6,0.65,0.7,0.8,0.9,1),
    breaks=seq(0,3.5,1), limits = c(0,3.5),
    guide = guide_legend(nrow = 1,title = paste0("Average absolute anomaly"))) +
    guides(fill = guide_colorbar(title.position="top",barwidth = 10, barheight = 1,title = expression("Average size of anomaly " (degree*C)))) +
    scale_x_continuous(breaks=c(seq(1,12,by=1)),labels=month.short) +
    # coord_flip() +
    xlab("Month") + ylab('') +
    theme_bw() + theme(panel.grid.major = element_blank(),axis.text.x = element_text(angle=90, vjust=0.5),
    plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="white", size=.5, linetype="dotted"))
dev.off()

pdf(paste0(output.dir,'heatmap_square_plot_uneven_scale_alt11_',year.start,'_',year.end,'_',dname,'_anom_abs.pdf'),paper='a4',height=0,width=0)
ggplot(data=dat.anom.abs)+
    geom_tile(aes(x=month,y=full_name,fill=anom.abs)) +
    coord_equal() +
    scale_fill_gradientn(colours = c('grey99',"grey90","grey80","grey70","grey60",'grey50','grey40','grey30','grey20','grey10',"grey0","black"),
    values = c(0,0.2,0.25,0.3,0.35,0.4,0.5,0.6,0.65,0.7,0.8,0.9,1),
    breaks=seq(0,3.5,1), limits = c(0,3.5),
    guide = guide_legend(nrow = 1,title = paste0("Average absolute anomaly"))) +
    guides(fill = guide_colorbar(title.position="top",barwidth = 10, barheight = 1,title = expression("Average size of anomaly " (degree*C)))) +
    scale_x_continuous(breaks=c(seq(1,12,by=1)),labels=month.short) +
    # coord_flip() +
    xlab("Month") + ylab('') +
    theme_bw() + theme(panel.grid.major = element_blank(),axis.text.x = element_text(angle=90, vjust=0.5),
    plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="white", size=.5, linetype="dotted"))
dev.off()

pdf(paste0(output.dir,'heatmap_square_plot_uneven_scale_alt12_',year.start,'_',year.end,'_',dname,'_anom_abs.pdf'),paper='a4',height=0,width=0)
ggplot(data=dat.anom.abs)+
    geom_tile(aes(x=month,y=full_name,fill=anom.abs)) +
    coord_equal() +
    scale_fill_gradientn(colours = c('cornsilk',"darkseagreen","darkseagreen1","darkseagreen2","darkseagreen3",'darkseagreen4','dodgerblue','dodgerblue1','dodgerblue2','dodgerblue3','dodgerblue4'),
    values = c(0,0.2,0.25,0.3,0.35,0.4,0.5,0.6,0.65,0.7,0.8,0.9,1),
    breaks=seq(0,3.5,1), limits = c(0,3.5),
    guide = guide_legend(nrow = 1,title = paste0("Average absolute anomaly"))) +
    guides(fill = guide_colorbar(title.position="top",barwidth = 10, barheight = 1,title = expression("Average size of anomaly " (degree*C)))) +
    scale_x_continuous(breaks=c(seq(1,12,by=1)),labels=month.short) +
    # coord_flip() +
    xlab("Month") + ylab('') +
    theme_bw() + theme(panel.grid.major = element_blank(),axis.text.x = element_text(angle=90, vjust=0.5),
    plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="white", size=.5, linetype="dotted"))
dev.off()

pdf(paste0(output.dir,'heatmap_square_plot_uneven_scale_alt13_',year.start,'_',year.end,'_',dname,'_anom_abs.pdf'),paper='a4',height=0,width=0)
ggplot(data=dat.anom.abs)+
    geom_tile(aes(x=month,y=full_name,fill=anom.abs)) +
    coord_equal() +
    scale_fill_gradientn(colours = c('cornsilk',"coral","coral1","coral2","coral3",'brown','brown1','brown2','brown3','brown4'),
    values = c(0,0.2,0.25,0.3,0.35,0.4,0.5,0.6,0.65,0.7,0.8,0.9,1),
    breaks=seq(0,3.5,1), limits = c(0,3.5),
    guide = guide_legend(nrow = 1,title = paste0("Average absolute anomaly"))) +
    guides(fill = guide_colorbar(title.position="top",barwidth = 10, barheight = 1,title = expression("Average size of anomaly " (degree*C)))) +
    scale_x_continuous(breaks=c(seq(1,12,by=1)),labels=month.short) +
    # coord_flip() +
    xlab("Month") + ylab('') +
    theme_bw() + theme(panel.grid.major = element_blank(),axis.text.x = element_text(angle=90, vjust=0.5),
    plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="white", size=.5, linetype="dotted"))
dev.off()

pdf(paste0(output.dir,'heatmap_square_plot_uneven_scale_alt14_',year.start,'_',year.end,'_',dname,'_anom_abs.pdf'),paper='a4',height=0,width=0)
ggplot(data=dat.anom.abs)+
    geom_tile(aes(x=month,y=full_name,fill=anom.abs)) +
    coord_equal() +
    scale_fill_gradientn(colours = c('cornsilk',"lightsteelblue","lightsteelblue1","lightsteelblue2","lightsteelblue3",'lightsteelblue4','steelblue','steelblue1','steelblue2','steelblue3'),
    values = c(0,0.2,0.25,0.3,0.35,0.4,0.5,0.6,0.65,0.7,0.8,0.9,1),
    breaks=seq(0,3.5,1), limits = c(0,3.5),
    guide = guide_legend(nrow = 1,title = paste0("Average absolute anomaly"))) +
    guides(fill = guide_colorbar(title.position="top",barwidth = 10, barheight = 1,title = expression("Average size of anomaly " (degree*C)))) +
    scale_x_continuous(breaks=c(seq(1,12,by=1)),labels=month.short) +
    # coord_flip() +
    xlab("Month") + ylab('') +
    theme_bw() + theme(panel.grid.major = element_blank(),axis.text.x = element_text(angle=90, vjust=0.5),
    plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="white", size=.5, linetype="dotted"))
dev.off()

pdf(paste0(output.dir,'heatmap_square_plot_uneven_scale_alt15_',year.start,'_',year.end,'_',dname,'_anom_abs.pdf'),paper='a4',height=0,width=0)
ggplot(data=dat.anom.abs)+
    geom_tile(aes(x=month,y=full_name,fill=anom.abs)) +
    coord_equal() +
    scale_fill_gradientn(colours = c('cornsilk',"springgreen","springgreen1","springgreen2","springgreen3",'springgreen4','steelblue','steelblue1','steelblue2','darkgreen'),
    values = c(0,0.2,0.25,0.3,0.35,0.4,0.5,0.6,0.65,0.7,0.8,0.9,1),
    breaks=seq(0,3.5,1), limits = c(0,3.5),
    guide = guide_legend(nrow = 1,title = paste0("Average absolute anomaly"))) +
    guides(fill = guide_colorbar(title.position="top",barwidth = 10, barheight = 1,title = expression("Average size of anomaly " (degree*C)))) +
    scale_x_continuous(breaks=c(seq(1,12,by=1)),labels=month.short) +
    # coord_flip() +
    xlab("Month") + ylab('') +
    theme_bw() + theme(panel.grid.major = element_blank(),axis.text.x = element_text(angle=90, vjust=0.5),
    plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="white", size=.5, linetype="dotted"))
dev.off()

pdf(paste0(output.dir,'heatmap_square_plot_uneven_scale_alt16_',year.start,'_',year.end,'_',dname,'_anom_abs.pdf'),paper='a4',height=0,width=0)
ggplot(data=dat.anom.abs)+
    geom_tile(aes(x=month,y=full_name,fill=anom.abs)) +
    coord_equal() +
    scale_fill_gradientn(colours = c('cornsilk','#F6BFF3','#E3B2E3','#D0A5D3','#BE99C3','#AD8CB4','#9C80A4','#8B7495','#7C6785','#6C5C76','#5D5068','#4F4459','#42394B'),
    values = c(0,0.2,0.25,0.3,0.35,0.4,0.5,0.6,0.65,0.7,0.8,0.9,1),
    breaks=seq(0,3.5,1), limits = c(0,3.5),
    guide = guide_legend(nrow = 1,title = paste0("Average absolute anomaly"))) +
    guides(fill = guide_colorbar(title.position="top",barwidth = 10, barheight = 1,title = expression("Average size of anomaly " (degree*C)))) +
    scale_x_continuous(breaks=c(seq(1,12,by=1)),labels=month.short) +
    # coord_flip() +
    xlab("Month") + ylab('') +
    theme_bw() + theme(panel.grid.major = element_blank(),axis.text.x = element_text(angle=90, vjust=0.5),
    plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="white", size=.5, linetype="dotted"))
dev.off()

pdf(paste0(output.dir,'heatmap_square_plot_uneven_scale_alt17_',year.start,'_',year.end,'_',dname,'_anom_abs.pdf'),paper='a4',height=0,width=0)
ggplot(data=dat.anom.abs)+
    geom_tile(aes(x=month,y=full_name,fill=anom.abs)) +
    coord_equal() +
    scale_fill_gradientn(colours = c('cornsilk','#FFECB3','#E82582','#6A1B9A'),
    values = c(0,0.28,0.56,0.7,1),
    breaks=seq(0,3.5,1), limits = c(0,3.5),
    guide = guide_legend(nrow = 1,title = paste0("Average absolute anomaly"))) +
    guides(fill = guide_colorbar(title.position="top",barwidth = 10, barheight = 1,title = expression("Average size of anomaly " (degree*C)))) +
    scale_x_continuous(breaks=c(seq(1,12,by=1)),labels=month.short) +
    # coord_flip() +
    xlab("Month") + ylab('') +
    theme_bw() + theme(panel.grid.major = element_blank(),axis.text.x = element_text(angle=90, vjust=0.5),
    plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="white", size=.5, linetype="dotted"))
dev.off()

pdf(paste0(output.dir,'heatmap_square_plot_uneven_scale_alt18_',year.start,'_',year.end,'_',dname,'_anom_abs.pdf'),paper='a4',height=0,width=0)
ggplot(data=dat.anom.abs)+
    geom_tile(aes(x=month,y=full_name,fill=anom.abs)) +
    coord_equal() +
    scale_fill_gradientn(colours = c('#f7fcf5',"#e5f5e0","#c7e9c0","#a1d99b","#74c476","#41ab5d","#238b45",'#006d2c','#00441b',"#00441b","#00441b","darkgreen","#252525"),
    values = c(0,0.2,0.25,0.3,0.35,0.4,0.5,0.6,0.65,0.7,0.8,0.9,1),
    breaks=seq(0,3.5,1), limits = c(0,3.5),
    guide = guide_legend(nrow = 1,title = paste0("Average absolute anomaly"))) +
    guides(fill = guide_colorbar(title.position="top",barwidth = 10, barheight = 1,title = expression("Average size of anomaly " (degree*C)))) +
    scale_x_continuous(breaks=c(seq(1,12,by=1)),labels=month.short) +
    # coord_flip() +
    xlab("Month") + ylab('') +
    theme_bw() + theme(panel.grid.major = element_blank(),axis.text.x = element_text(angle=90, vjust=0.5),
    plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="white", size=.5, linetype="dotted"))
dev.off()

pdf(paste0(output.dir,'heatmap_square_plot_uneven_scale_alt19_',year.start,'_',year.end,'_',dname,'_anom_abs.pdf'),paper='a4',height=0,width=0)
ggplot(data=dat.anom.abs)+
    geom_tile(aes(x=month,y=full_name,fill=anom.abs)) +
    coord_equal() +
    scale_fill_gradientn(colours = c('#fcfbfd',"#efedf5","#dadaeb","#bcbddc","#9e9ac8","#807dba","#6a51a3",'#54278f','#3f007d',"#3f007d","#3f007d","darkviolet","darkviolet"),
    values = c(0,0.2,0.25,0.3,0.35,0.4,0.5,0.6,0.65,0.7,0.8,0.9,1),
    breaks=seq(0,3.5,1), limits = c(0,3.5),
    guide = guide_legend(nrow = 1,title = paste0("Average absolute anomaly"))) +
    guides(fill = guide_colorbar(title.position="top",barwidth = 10, barheight = 1,title = expression("Average size of anomaly " (degree*C)))) +
    scale_x_continuous(breaks=c(seq(1,12,by=1)),labels=month.short) +
    # coord_flip() +
    xlab("Month") + ylab('') +
    theme_bw() + theme(panel.grid.major = element_blank(),axis.text.x = element_text(angle=90, vjust=0.5),
    plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="white", size=.5, linetype="dotted"))
dev.off()

pdf(paste0(output.dir,'heatmap_square_plot_uneven_scale_alt20_',year.start,'_',year.end,'_',dname,'_anom_abs.pdf'),paper='a4',height=0,width=0)
ggplot(data=dat.anom.abs)+
    geom_tile(aes(x=month,y=full_name,fill=anom.abs)) +
    coord_equal() +
    scale_fill_gradientn(colours = c('cornsilk',"#e5f5e0","#c7e9c0","#a1d99b","#74c476","#41ab5d","#238b45",'#006d2c',"darkgreen",'#00441b',"#00441b","#00441b","#252525"),
    values = c(0,0.2,0.25,0.3,0.35,0.4,0.5,0.6,0.65,0.7,0.8,0.9,1),
    breaks=seq(0,3.5,1), limits = c(0,3.5),
    guide = guide_legend(nrow = 1,title = paste0("Average absolute anomaly"))) +
    guides(fill = guide_colorbar(title.position="top",barwidth = 10, barheight = 1,title = expression("Average size of anomaly " (degree*C)))) +
    scale_x_continuous(breaks=c(seq(1,12,by=1)),labels=month.short) +
    # coord_flip() +
    xlab("Month") + ylab('') +
    theme_bw() + theme(panel.grid.major = element_blank(),axis.text.x = element_text(angle=90, vjust=0.5),
    plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="white", size=.5, linetype="dotted"))
dev.off()

pdf(paste0(output.dir,'heatmap_square_plot_uneven_scale_alt21_',year.start,'_',year.end,'_',dname,'_anom_abs.pdf'),paper='a4',height=0,width=0)
ggplot(data=dat.anom.abs)+
    geom_tile(aes(x=month,y=full_name,fill=anom.abs)) +
    coord_equal() +
    scale_fill_gradientn(colours = c('cornsilk',"#efedf5","#dadaeb","#bcbddc","#9e9ac8","#807dba","#6a51a3",'#54278f','#521886','#521886','#3f007d',"#3f007d","#3f007d"),
    values = c(0,0.2,0.25,0.3,0.35,0.4,0.5,0.6,0.65,0.7,0.8,0.9,1),
    breaks=seq(0,3.5,1), limits = c(0,3.5),
    guide = guide_legend(nrow = 1,title = paste0("Average absolute anomaly"))) +
    guides(fill = guide_colorbar(title.position="top",barwidth = 10, barheight = 1,title = expression("Average size of anomaly " (degree*C)))) +
    scale_x_continuous(breaks=c(seq(1,12,by=1)),labels=month.short) +
    # coord_flip() +
    xlab("Month") + ylab('') +
    theme_bw() + theme(panel.grid.major = element_blank(),axis.text.x = element_text(angle=90, vjust=0.5),
    plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="white", size=.5, linetype="dotted"))
dev.off()

purpleramp = colorRampPalette(c("cornsilk", "#3f007d"))(11)

pdf(paste0(output.dir,'heatmap_square_plot_uneven_scale_alt22_',year.start,'_',year.end,'_',dname,'_anom_abs.pdf'),paper='a4',height=0,width=0)
ggplot(data=dat.anom.abs)+
    geom_tile(aes(x=month,y=full_name,fill=anom.abs)) +
    coord_equal() +
    scale_fill_gradientn(colours = purpleramp,
    values = c(0,0.2,0.25,0.3,0.35,0.4,0.5,0.6,0.65,0.7,0.8,0.9,1),
    breaks=seq(0,3.5,1), limits = c(0,3.5),
    guide = guide_legend(nrow = 1,title = paste0("Average absolute anomaly"))) +
    guides(fill = guide_colorbar(title.position="top",barwidth = 10, barheight = 1,title = expression("Average size of anomaly " (degree*C)))) +
    scale_x_continuous(breaks=c(seq(1,12,by=1)),labels=month.short) +
    # coord_flip() +
    xlab("Month") + ylab('') +
    theme_bw() + theme(panel.grid.major = element_blank(),axis.text.x = element_text(angle=90, vjust=0.5),
    plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="white", size=.5, linetype="dotted"))
dev.off()



# LEGACY CODE BELOW

# # dots
# pdf(paste0(output.dir,'dots_plot_',year.start,'_',year.end,'_',dname,'_anom_abs.pdf'),paper='a4r',height=0,width=0)
# ggplot(data=dat.anom.abs) +
#     geom_point(aes(x=month,y=abs(anom.abs))) +
#     scale_x_continuous(breaks=c(seq(1,12,by=1)),labels=month.short)   +
#     xlab("Month") + ylab('Average absolute anomaly') +
#     theme_bw() + theme(panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
#     plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
#     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
#     panel.border = element_rect(colour = "black"),strip.background = element_blank(),
#     legend.position = 'bottom',legend.justification='center',
#     legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))
# dev.off()
#
# # heat map
# pdf(paste0(output.dir,'heatmap_plot_',year.start,'_',year.end,'_',dname,'_anom_abs.pdf'),paper='a4r',height=0,width=0)
# ggplot(data=dat.anom.abs)+
#     geom_tile(aes(x=month,y=full_name,fill=abs(anom.abs))) +
#     # coord_equal() +
#     scale_fill_gradientn(colours=colorway,
#     breaks=seq(-0.5,4,0.5), limits = c(0,3.5),
#     guide = guide_legend(nrow = 1,title = paste0(""))) +
#     guides(fill = guide_colorbar(barwidth = 30, barheight = 1,title = expression(paste0("Average size of anomaly (",degree,"C)")))) +
#     scale_x_continuous(breaks=c(seq(1,12,by=1)),labels=month.short)   +
#     # coord_flip() +
#     xlab("Month") + ylab('State') +
#     theme_bw() + theme(panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
#     plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
#     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
#     panel.border = element_rect(colour = "black"),strip.background = element_blank(),
#     legend.position = 'bottom',legend.justification='center',
#     legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))

# LEGACY CODE BELOW

# ############# STANDARD DEVIATION #############
#
# # process statistics
# dat.sd = ddply(dat,.(full_name,month),summarise,sd=sd(variable))
#
# # dots
# pdf(paste0(output.dir,'dots_plot_',year.start,'_',year.end,'_',dname,'_sd.pdf'),paper='a4r',height=0,width=0)
# ggplot(data=dat.sd) +
#     geom_point(aes(x=month,y=sd)) +
#     scale_x_continuous(breaks=c(seq(1,12,by=1)),labels=month.short)   +
#     xlab("Month") + ylab('Standard deviation of anomaly') +
#     theme_bw() + theme(panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
#     plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
#     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
#     panel.border = element_rect(colour = "black"),strip.background = element_blank(),
#     legend.position = 'bottom',legend.justification='center',
#     legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))
# dev.off()
#
# # heat map
# pdf(paste0(output.dir,'heatmap_plot_',year.start,'_',year.end,'_',dname,'_sd.pdf'),paper='a4r',height=0,width=0)
# ggplot(data=dat.sd)+
#     geom_tile(aes(x=month,y=full_name,fill=sd)) +
#     scale_fill_gradientn(colours=colorway,
#     breaks=seq(-0.5,4,0.5), limits = c(0,4),
#     # na.value = "grey98",limits = c(-0.027, 0.027),
#     # breaks=c(-0.08, -0.06, -0.04, -0.02, 0, 0.02, 0.04, 0.06, 0.08),
#     # na.value = "grey98",limits = c(-0.1, 0.1),
#     guide = guide_legend(nrow = 1,title = paste0("Standard deviation of anomalies"))) +
#     guides(fill = guide_colorbar(barwidth = 30, barheight = 1,title = paste0("Standard deviation"))) +
#     scale_x_continuous(breaks=c(seq(1,12,by=1)),labels=month.short)   +
#     coord_flip() +
#     ylab("Month") + xlab('State') +
#     theme_bw() + theme(panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
#     plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
#     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
#     panel.border = element_rect(colour = "black"),strip.background = element_blank(),
#     legend.position = 'bottom',legend.justification='center',
#     legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))
# dev.off()
#
# # heat map square
# pdf(paste0(output.dir,'heatmap_square_plot_',year.start,'_',year.end,'_',dname,'_sd.pdf'),paper='a4r',height=0,width=0)
# ggplot(data=dat.sd)+
#     geom_tile(aes(y=month,x=full_name,fill=sd)) +
#     coord_equal() +
#     scale_fill_gradientn(colours=colorway,
#     breaks=seq(-0.5,4,0.5), limits = c(0,4),
#     guide = guide_legend(nrow = 1,title = paste0("Standard deviation of anomalies"))) +
#     guides(fill = guide_colorbar(barwidth = 30, barheight = 1,title = paste0("Standard deviation"))) +
#     scale_y_continuous(breaks=c(seq(1,12,by=1)),labels=month.short)   +
#     # coord_flip() +
#     ylab("Month") + xlab('State') +
#     theme_bw() + theme(panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
#     plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
#     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
#     panel.border = element_rect(colour = "black"),strip.background = element_blank(),
#     legend.position = 'bottom',legend.justification='center',
#     legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))
# dev.off()
#
# ############# POSITIVE ANOMALY #############
#
# # process statistics
# dat.anom.pos = ddply(subset(dat,variable>=0),.(full_name,month),summarise,anom.pos=mean(variable))
#
# # dots
# pdf(paste0(output.dir,'dots_plot_',year.start,'_',year.end,'_',dname,'_anom_pos.pdf'),paper='a4r',height=0,width=0)
# ggplot(data=dat.anom.pos) +
#     geom_point(aes(x=month,y=anom.pos)) +
#     scale_x_continuous(breaks=c(seq(1,12,by=1)),labels=month.short)   +
#     xlab("Month") + ylab('Average positive anomaly') +
#     theme_bw() + theme(panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
#     plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
#     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
#     panel.border = element_rect(colour = "black"),strip.background = element_blank(),
#     legend.position = 'bottom',legend.justification='center',
#     legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))
# dev.off()
#
# # heat map
# pdf(paste0(output.dir,'heatmap_plot_',year.start,'_',year.end,'_',dname,'_anom_pos.pdf'),paper='a4r',height=0,width=0)
# ggplot(data=dat.anom.pos)+
#     geom_tile(aes(x=month,y=full_name,fill=anom.pos)) +
#     coord_equal() +
#     scale_fill_gradientn(colours=colorway,
#     breaks=seq(-0.5,4,0.5), limits = c(0,4),
#     guide = guide_legend(nrow = 1,title = paste0("Average positive anomaly"))) +
#     guides(fill = guide_colorbar(barwidth = 30, barheight = 1,title = paste0("Average positive anomaly"))) +
#     scale_x_continuous(breaks=c(seq(1,12,by=1)),labels=month.short)   +
#     coord_flip() +
#     xlab("Month") + ylab('State') +
#     theme_bw() + theme(panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
#     plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
#     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
#     panel.border = element_rect(colour = "black"),strip.background = element_blank(),
#     legend.position = 'bottom',legend.justification='center',
#     legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))
# dev.off()
#
# # heat map square
# pdf(paste0(output.dir,'heatmap_square_plot_',year.start,'_',year.end,'_',dname,'_anom_pos.pdf'),paper='a4r',height=0,width=0)
# ggplot(data=dat.anom.pos)+
#     geom_tile(aes(y=month,x=full_name,fill=anom.pos)) +
#     coord_equal() +
#     scale_fill_gradientn(colours=colorway,
#     breaks=seq(-0.5,4,0.5), limits = c(0,4),
#     guide = guide_legend(nrow = 1,title = paste0("Average positive anomaly"))) +
#     guides(fill = guide_colorbar(barwidth = 30, barheight = 1,title = paste0("Average positive anomaly"))) +
#     scale_y_continuous(breaks=c(seq(1,12,by=1)),labels=month.short)   +
#     # coord_flip() +
#     ylab("Month") + xlab('State') +
#     theme_bw() + theme(panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
#     plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
#     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
#     panel.border = element_rect(colour = "black"),strip.background = element_blank(),
#     legend.position = 'bottom',legend.justification='center',
#     legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))
# dev.off()
#
# ############# NEGATIVE ANOMALY #############
#
# # process statistics
# dat.anom.neg = ddply(subset(dat,variable<0),.(full_name,month),summarise,anom.neg=mean(variable))
#
# # dots
# pdf(paste0(output.dir,'dots_plot_',year.start,'_',year.end,'_',dname,'_anom_neg.pdf'),paper='a4r',height=0,width=0)
# ggplot(data=dat.anom.neg) +
#     geom_point(aes(x=month,y=abs(anom.neg))) +
#     scale_x_continuous(breaks=c(seq(1,12,by=1)),labels=month.short)   +
#     xlab("Month") + ylab('Average negative anomaly') +
#     theme_bw() + theme(panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
#     plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
#     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
#     panel.border = element_rect(colour = "black"),strip.background = element_blank(),
#     legend.position = 'bottom',legend.justification='center',
#     legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))
# dev.off()
#
# # heat map
# pdf(paste0(output.dir,'heatmap_plot_',year.start,'_',year.end,'_',dname,'_anom_neg.pdf'),paper='a4r',height=0,width=0)
# ggplot(data=dat.anom.neg)+
#     geom_tile(aes(x=month,y=full_name,fill=abs(anom.neg))) +
#     coord_equal() +
#     scale_fill_gradientn(colours=colorway,
#     breaks=seq(-0.5,4,0.5), limits = c(0,4),
#     guide = guide_legend(nrow = 1,title = paste0("Average negative anomaly"))) +
#     guides(fill = guide_colorbar(barwidth = 30, barheight = 1,title = paste0("Average negative anomaly"))) +
#     scale_x_continuous(breaks=c(seq(1,12,by=1)),labels=month.short)   +
#     coord_flip() +
#     xlab("Month") + ylab('State') +
#     theme_bw() + theme(panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
#     plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
#     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
#     panel.border = element_rect(colour = "black"),strip.background = element_blank(),
#     legend.position = 'bottom',legend.justification='center',
#     legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))
# dev.off()
#
# # heat map square
# pdf(paste0(output.dir,'heatmap_square_plot_',year.start,'_',year.end,'_',dname,'_anom_neg.pdf'),paper='a4r',height=0,width=0)
# ggplot(data=dat.anom.neg)+
#     geom_tile(aes(y=month,x=full_name,fill=abs(anom.neg))) +
#     coord_equal() +
#     scale_fill_gradientn(colours=colorway,
#     breaks=seq(-0.5,4,0.5), limits = c(0,4),
#     guide = guide_legend(nrow = 1,title = paste0("Average negative anomaly"))) +
#     guides(fill = guide_colorbar(barwidth = 30, barheight = 1,title = paste0("Average negative anomaly"))) +
#     scale_y_continuous(breaks=c(seq(1,12,by=1)),labels=month.short)   +
#     # coord_flip() +
#     ylab("Month") + xlab('State') +
#     theme_bw() + theme(panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
#     plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
#     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
#     panel.border = element_rect(colour = "black"),strip.background = element_blank(),
#     legend.position = 'bottom',legend.justification='center',
#     legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))
# dev.off()
#
# ############# ANY ANOMALY #############
#
# # process statistics
# dat = subset(dat,year>1979&year<2010&state.fips!=32)
# dat.anom = ddply(subset(dat),.(full_name,month),summarise,anom=mean(variable))
#
# # dots
# pdf(paste0(output.dir,'dots_plot_',year.start,'_',year.end,'_',dname,'_anom.pdf'),paper='a4r',height=0,width=0)
# ggplot(data=dat.anom) +
#     geom_point(aes(x=month,y=abs(anom))) +
#     scale_x_continuous(breaks=c(seq(1,12,by=1)),labels=month.short)   +
#     xlab("Month") + ylab('Average anomaly') +
#     ylim(c(0,2)) +
#     theme_bw() + theme(panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
#     plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
#     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
#     panel.border = element_rect(colour = "black"),strip.background = element_blank(),
#     legend.position = 'bottom',legend.justification='center',
#     legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))
# dev.off()
#
# # heat map
# pdf(paste0(output.dir,'heatmap_plot_',year.start,'_',year.end,'_',dname,'_anom.pdf'),paper='a4r',height=0,width=0)
# ggplot(data=dat.anom)+
#     geom_tile(aes(x=month,y=full_name,fill=abs(anom))) +
#     coord_equal() +
#     # scale_fill_gradientn(colours=colorway,
#     # breaks=seq(0,5,0.5),
#     # guide = guide_legend(nrow = 1,title = paste0("Average anomaly"))) +
#     guides(fill = guide_colorbar(barwidth = 30, barheight = 1,title = paste0("Average anomaly"))) +
#     scale_x_continuous(breaks=c(seq(1,12,by=1)),labels=month.short)   +
#     coord_flip() +
#     xlab("Month") + ylab('State') +
#     theme_bw() + theme(panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
#     plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
#     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
#     panel.border = element_rect(colour = "black"),strip.background = element_blank(),
#     legend.position = 'bottom',legend.justification='center',
#     legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))
# dev.off()
#
# # heat map square
# pdf(paste0(output.dir,'heatmap_square_plot_',year.start,'_',year.end,'_',dname,'_anom.pdf'),paper='a4r',height=0,width=0)
# ggplot(data=dat.anom)+
#     geom_tile(aes(y=month,x=full_name,fill=abs(anom))) +
#     coord_equal() +
#     # scale_fill_gradientn(colours=colorway,
#     # breaks=seq(-0.5,5,0.5),
#     # guide = guide_legend(nrow = 1,title = paste0("Average anomaly"))) +
#     guides(fill = guide_colorbar(barwidth = 30, barheight = 1,title = paste0("Average anomaly"))) +
#     scale_y_continuous(breaks=c(seq(1,12,by=1)),labels=month.short)   +
#     # coord_flip() +
#     ylab("Month") + xlab('State') +
#     theme_bw() + theme(panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
#     plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
#     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
#     panel.border = element_rect(colour = "black"),strip.background = element_blank(),
#     legend.position = 'bottom',legend.justification='center',
#     legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))
# dev.off()
#
