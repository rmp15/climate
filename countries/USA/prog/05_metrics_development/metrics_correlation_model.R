# this script
# loads particular metrics of interest
# establishes r^2 values for each state-month
# find average for country for each metric based on state-month values
# plots on a matrix/heat map
# go to correct directory
setwd('~/git/climate/countries/USA/prog/00_bash')

rm(list=ls())

library(ggplot2)
library(RColorBrewer)
library(plyr)

# arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)
year.start <- as.numeric(args[1])
year.end <- as.numeric(args[2])
dname.1 <- as.character(args[3])
dname.2 <- as.character(args[4])

# declare variables
year.start = 1979 ; year.end = 2015 ;
dname.1 = 't2m' ; dname.2 = 't2m'

# create output directory
dir = paste0("../../output/metrics_correlation_matrix/",dname.1,"/")
ifelse(!dir.exists(dir), dir.create(dir,recursive=TRUE), FALSE)

# load climate metrics
source('../../data/objects/objects.R')

# load all metrics of interest
i=1 # label counter
for (metric in metrics.matrix) {
    dat.metric = readRDS(paste0('../../output/metrics_development/',dname.1,'/',
                            metric,'_',dname.1,'/state_weighted_summary_',
                            metric,'_',dname.1,'_',year.start,'_',year.end,'.rds'))
    dat.metric = subset(dat.metric,age==85&sex==2)
    dat.metric= with(dat.metric,dat.metric[order(sex,age,state.fips,month),])
    assign(paste0('dat.',i),dat.metric)
    i=i+1
}

# create an average of r^2 values for each state month combination
dat.rsq.all = data.frame(metric.1=character(0),metric.2=character(0),rsq.mean=numeric(0),rsq.mi=numeric(0),rsq.max=numeric(0))
for (j in seq(length(metrics.matrix))) {
    for (k in seq(length(metrics.matrix))) {
        print(c(j,k))
        assign('dat.a',data.frame(get(paste0('dat.',j))))  ; assign('dat.b',data.frame(get(paste0('dat.',k))))
        dat.corr = data.frame(month=dat.1$month,state.fips=dat.a$state.fips,metric.1=dat.a[,ncol(dat.a)],metric.2=dat.b[,ncol(dat.b)])
        # loop through state-months and find r^2 values
        dat.rsq = data.frame(month=numeric(0),state.fips=character(0),r.squared=numeric(0))
        for (month.temp in sort(unique(dat.corr$month))){
            for(state in sort(unique(dat.corr$state.fips))){
                dat.temp = subset(dat.corr,month==month.temp & state.fips==state)
                lm = lm(metric.2~metric.1,data=dat.temp)
                r.squared = round(summary(lm)$r.squared,2)
                #cor.value = cor(dat.temp$metric.1,dat.temp$metric.2)
                #print(c(month.temp,state.fips,r.squared))
                dat.add = data.frame(month=month.temp,state.fips=state,r.squared=r.squared)
                dat.add$metric.1 = metrics.matrix[j] ; dat.add$metric.2 = metrics.matrix[k]
                dat.rsq = rbind(dat.rsq,dat.add)
            }}
        # find average of squared values (and the max/min?)
        dat.add = ddply(dat.rsq,.(),summarize,rsq.mean=mean(r.squared),rsq.min=min(r.squared),rsq.max=max(r.squared))
        dat.add$metric.1 = metrics.matrix[j] ; dat.add$metric.2 = metrics.matrix[k]
        print(dat.add)
        dat.rsq.all = rbind(dat.rsq.all,dat.add)
}}
dat.rsq.all$'.id' = NULL

pdf(paste0(dir,'correlation_matrix_rsquared_',year.start,'_',year.end,'.pdf'),height=7,width=7)
print(ggplot(data=dat) +
geom_tile(aes(x=as.factor(metric.1),y=as.factor(metric.2),fill=rsq.mean)) +
geom_text(aes(x=as.factor(metric.1),y=as.factor(metric.2),label=paste0(round(rsq.mean,2),'\n(',round(rsq.min,2),',\n',round(rsq.max,2),')')),
size=2,color='white') +
scale_fill_gradientn(colours=colors.reg,
na.value = "grey98", limits = c(0, 1)) +
guides(fill = guide_colorbar(barwidth = 20, barheight = 1,title = 'R squared')) +
scale_x_discrete(labels=metrics.matrix.short) +
scale_y_discrete(labels=metrics.matrix.short) +
ggtitle('') +
scale_size(guide = 'none') +
xlab("") + ylab('') +
theme(
text = element_text(size = 15), axis.text = element_text(size=15),
panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
axis.text.x = element_text(angle=90),
plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
strip.background = element_blank(), axis.line = element_line(colour = "black"),
legend.position = 'bottom',legend.justification='center',
legend.background = element_rect(fill="gray90", size=.5, linetype="dotted")))
dev.off()

# create an average of r values for each state month combination
dat.rsq.all = data.frame(metric.1=character(0),metric.2=character(0),rsq.mean=numeric(0),rsq.mi=numeric(0),rsq.max=numeric(0))
for (j in seq(length(metrics.matrix))) {
    for (k in seq(length(metrics.matrix))) {
        print(c(j,k))
        assign('dat.a',data.frame(get(paste0('dat.',j))))  ; assign('dat.b',data.frame(get(paste0('dat.',k))))
        dat.corr = data.frame(month=dat.1$month,state.fips=dat.a$state.fips,metric.1=dat.a[,ncol(dat.a)],metric.2=dat.b[,ncol(dat.b)])
        # loop through state-months and find r^2 values
        dat.rsq = data.frame(month=numeric(0),state.fips=character(0),r.squared=numeric(0))
        for (month.temp in sort(unique(dat.corr$month))){
            for(state in sort(unique(dat.corr$state.fips))){
                dat.temp = subset(dat.corr,month==month.temp & state.fips==state)
                lm = lm(metric.2~metric.1,data=dat.temp)
                #r.squared = round(summary(lm)$r.squared,2)
                r.squared = cor(dat.temp$metric.1,dat.temp$metric.2)
                #print(c(month.temp,state.fips,r.squared))
                dat.add = data.frame(month=month.temp,state.fips=state,r.squared=r.squared)
                dat.add$metric.1 = metrics.matrix[j] ; dat.add$metric.2 = metrics.matrix[k]
                dat.rsq = rbind(dat.rsq,dat.add)
            }}
        # find average of squared values (and the max/min?)
        dat.add = ddply(dat.rsq,.(),summarize,rsq.mean=mean(r.squared),rsq.min=min(r.squared),rsq.max=max(r.squared))
        dat.add$metric.1 = metrics.matrix[j] ; dat.add$metric.2 = metrics.matrix[k]
        print(dat.add)
        dat.rsq.all = rbind(dat.rsq.all,dat.add)
}}
dat.rsq.all$'.id' = NULL

# reorder dataframe variables for plotting
dat = merge(dat.rsq.all,dat.dict,by.x=c('metric.1'),by.y=c('metric'))
dat$name.1 = dat$name ; dat$name = NULL
dat$name.1 = reorder(dat$name.1,dat$order)
dat$metric.1 = NULL ; dat$order = NULL

dat = merge(dat,dat.dict,by.x=c('metric.2'),by.y=c('metric'))
dat$name.2 = dat$name ; dat$name = NULL
dat$name.2 = reorder(dat$name.2,dat$order)
dat$metric.2 = NULL ; dat$order = NULL

# use the data frame of all the correlations to make a national heatmap, with numbers filled-in to express
pdf(paste0(dir,'correlation_matrix_r_',year.start,'_',year.end,'.pdf'),height=7,width=7)
print(ggplot(data=dat) +
geom_tile(aes(x=as.factor(name.1),y=as.factor(name.2),fill=rsq.mean)) +
geom_text(aes(x=as.factor(name.1),y=as.factor(name.2),label=paste0(round(rsq.mean,2),'\n(',round(rsq.min,2),',\n',round(rsq.max,2),')')),
size=3,color='white') +
scale_fill_gradientn(colours=colors.reg,
na.value = "grey98", limits = c(-1, 1)) +
guides(fill = guide_colorbar(barwidth = 20, barheight = 1,title = 'Correlation')) +
ggtitle('') +
scale_size(guide = 'none') +
xlab("") + ylab('') +
theme_bw() + theme( panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
panel.border = element_rect(colour = "black"),strip.background = element_blank(),
legend.position = 'bottom',legend.justification='center',
legend.background = element_rect(fill="gray90", size=.5, linetype="dotted")))
dev.off()