# this script
# loads particular metrics of interest
# finds particular values of R^2 values
# plots on a matrix/heat map

# go to correct directory
setwd('~/git/climate/countries/USA/prog/00_bash')

rm(list=ls())

library(ggplot2)
library(RColorBrewer)
library(plyr)
library(INLA)

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
dat.complete = data.frame()
for (metric in metrics.matrix) {
    dat.metric = readRDS(paste0('../../output/metrics_development/',dname.1,'/',
                            metric,'_',dname.1,'/state_weighted_summary_',
                            metric,'_',dname.1,'_',year.start,'_',year.end,'.rds'))
    dat.metric = subset(dat.metric,age==85&sex==2)
    dat.metric= with(dat.metric,dat.metric[order(sex,age,state.fips,month),])
    assign(paste0('dat.',i),dat.metric)
    if(i==1){
        dat.complete = dat.metric
    }
    if(i>1){
        dat.complete = merge(dat.complete,dat.metric)
    }
    i=i+1
}

dat.complete$month = as.integer(dat.complete$month)

# eliminate unnecessary columns
dat.complete$sex = NULL ; dat.complete$age = NULL ;
dat.complete$year = NULL ; dat.complete$leap = NULL

# extract unique table of state and months to generate state.month
dat.state.month = unique(dat.complete[,c('month', 'state.fips')])
dat.state.month$month = as.integer(dat.state.month$month)
dat.state.month$state.month = seq(nrow(dat.state.month))

# merge year.month table with population table to create year.month id
dat.complete = merge(dat.complete,dat.state.month, by=c('month','state.fips'),all.x=1)

#######################################
# LM
#######################################

# define and run model
table.names = names(dat.complete[3:10])
dat.plot = data.frame(metric.1=character(0),metric.2=character(0),r=numeric(0),rsq=numeric(0))
for (i in seq(length(table.names))){
    for (j in seq(length(table.names))) {
        lmod = lm(get(table.names[i]) ~ get(table.names[j]) + as.factor(month) + state.fips, dat.complete)
        print(c(table.names[i],table.names[j]))
        print(c(lmod$coefficients[2],summary(lmod)$adj.r.squared))
        dat.add = data.frame(metric.1=metrics.matrix[i],metric.2=metrics.matrix[j],r=lmod$coefficients[2], rsq=summary(lmod)$adj.r.squared)
        dat.plot = rbind(dat.plot,dat.add)
}}

lmod.1 = lm(t2m.meanc3 ~ t2m.sd + as.factor(month) + as.factor(state.fips), dat.complete)
lmod.2 = lm(t2m.sd  ~ t2m.meanc3 + as.factor(month) + as.factor(state.fips), dat.complete)


#######################################
# INLA
#######################################

# define a least-squares regression model in INLA
#fml = t2m.meanc3 ~ f(state.month,t2m.10percc3,model='iid')
#mod = inla(fml, family="gaussian", data=dat.complete)

# need to then reconstruct the values by combining the parameters...

#######################################
# PLOTTING
#######################################

# reorder dataframe variables for plotting
dat = merge(dat.plot,dat.dict,by.x=c('metric.1'),by.y=c('metric'))
dat$name.1 = dat$name ; dat$name = NULL
dat$name.1 = reorder(dat$name.1,dat$order)
dat$metric.1 = NULL ; dat$order = NULL

dat = merge(dat,dat.dict,by.x=c('metric.2'),by.y=c('metric'))
dat$name.2 = dat$name ; dat$name = NULL
dat$name.2 = reorder(dat$name.2,dat$order)
dat$metric.2 = NULL ; dat$order = NULL

# use the data frame of all the r-squared to make a national heatmap, with numbers filled-in to express
pdf(paste0(dir,'correlation_matrix_rsquared_',year.start,'_',year.end,'.pdf'),height=7,width=7)
print(ggplot(data=dat) +
geom_tile(aes(x=as.factor(name.1),y=as.factor(name.2),fill=rsq)) +
geom_text(aes(x=as.factor(name.1),y=as.factor(name.2),label=paste0(round(rsq,2))),
size=3,color='white') +
scale_fill_gradientn(colours=colors.rsq,
na.value = "grey98", limits = c(0, 1)) +
guides(fill = guide_colorbar(barwidth = 20, barheight = 1,title = 'R-squared')) +
ggtitle('') +
scale_size(guide = 'none') +
xlab('Dependent (output) variable') + ylab('Independent (input) variable') +
theme_bw() + theme( panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
panel.border = element_rect(colour = "black"),strip.background = element_blank(),
legend.position = 'bottom',legend.justification='center',
legend.background = element_rect(fill="gray90", size=.5, linetype="dotted")))
dev.off()
