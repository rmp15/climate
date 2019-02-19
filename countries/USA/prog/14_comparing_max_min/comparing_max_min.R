rm(list=ls())

library(ggplot2)
library(foreign)
library(plyr)

# arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)
year.start <- as.numeric(args[1])
year.end <- as.numeric(args[2])
metric <- as.character(args[3])
dname <- as.character(args[4])

file.loc = paste0("../../output/comparing_max_min/",dname,'/')
ifelse(!dir.exists(file.loc), dir.create(file.loc, recursive=TRUE), FALSE)

# load mean max and min
dat.mean = readRDS(paste0('../../output/metrics_development/',dname,'/',metric,'_',dname,'/state_weighted_summary_',metric,'_',dname,'_',year.start,'_',year.end,'.rds'))
dat.min = readRDS(paste0('../../output/metrics_development/',dname,'/',metric,'_',dname,'/state_weighted_summary_',metric,'_',dname,'_',year.start,'_',year.end,'_min.rds'))
dat.max = readRDS(paste0('../../output/metrics_development/',dname,'/',metric,'_',dname,'/state_weighted_summary_',metric,'_',dname,'_',year.start,'_',year.end,'_max.rds'))

# restrict to one age and sex
dat.mean = subset(dat.mean,sex==2&age==65)
dat.min = subset(dat.min,sex==2&age==65)
dat.max = subset(dat.max,sex==2&age==65)

# merge datasets
dat.merged = merge(dat.mean,dat.min, by=c('year','month','state.fips','sex','age'),all.x=TRUE)
dat.merged = merge(dat.merged,dat.max, by=c('year','month','state.fips','sex','age'))

# TEMPORARILY restrict to just 1979-2015 (weird problem with 2016)
dat.merged = subset(dat.merged,year%in%c(seq(1979,2015)))

# get rid of alaska and hawaii
dat.merged = subset(dat.merged,!(state.fips%in%c('02','15')))

# get rid of age and sex
dat.merged$age = dat.merged$sex = NULL

# calculate correlation per state, month, and month-state
dat.state = ddply(dat.merged,.(state.fips),corr(t2m.mean,t2m.max))
dat.month = ddply()
dat.state.month = ddply()
# dat.pois.summary <- ddply(dat.pois,.(sex,age,year), function(z)coef(summary(glm(deaths.pred ~ maxmonth + offset(log(pop.adj)),family=poisson,data=z))))

# plot overall correlation
pdf(paste0(file.loc,'/',dname,'_',metric,'_',year.start,'_',year.end,'.pdf'),height=0,width=0,paper='a4r')
# overall plots
ggplot(data=dat.merged) +
geom_point(aes(x=t2m.mean,y=t2m.min)) +
xlab('Mean temperature') +
ylab('Mininum tempearture') +
theme_bw() +
theme(panel.grid.major = element_blank(),text = element_text(size = 15),
axis.ticks.x=element_blank(),
panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
panel.border = element_rect(colour = "black"),strip.background = element_blank(),
legend.position = 'bottom',legend.justification='center',
legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))

ggplot(data=dat.merged) +
geom_point(aes(x=t2m.mean,y=t2m.max)) +
xlab('Mean temperature') +
ylab('Maximum tempearture') +
theme_bw() +
theme(panel.grid.major = element_blank(),text = element_text(size = 15),
axis.ticks.x=element_blank(),
panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
panel.border = element_rect(colour = "black"),strip.background = element_blank(),
legend.position = 'bottom',legend.justification='center',
legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))

ggplot(data=dat.merged) +
geom_point(aes(x=t2m.min,y=t2m.max)) +
xlab('Minimum temperature') +
ylab('Maximum tempearture') +
theme_bw() +
theme(panel.grid.major = element_blank(),text = element_text(size = 15),
axis.ticks.x=element_blank(),
panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
panel.border = element_rect(colour = "black"),strip.background = element_blank(),
legend.position = 'bottom',legend.justification='center',
legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))

dev.off()

# plot correlation by month
pdf(paste0(file.loc,'/',dname,'_',metric,'_month_',year.start,'_',year.end,'.pdf'),height=0,width=0,paper='a4r')
# overall plots
ggplot(data=dat.merged) +
geom_point(aes(x=t2m.mean,y=t2m.min)) +
xlab('Mean temperature') +
ylab('Mininum tempearture') +
facet_wrap(~month) +
theme_bw() +
theme(panel.grid.major = element_blank(),text = element_text(size = 15),
axis.ticks.x=element_blank(),
panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
panel.border = element_rect(colour = "black"),strip.background = element_blank(),
legend.position = 'bottom',legend.justification='center',
legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))

ggplot(data=dat.merged) +
geom_point(aes(x=t2m.mean,y=t2m.max)) +
xlab('Mean temperature') +
ylab('Maximum tempearture') +
facet_wrap(~month) +
theme_bw() +
theme(panel.grid.major = element_blank(),text = element_text(size = 15),
axis.ticks.x=element_blank(),
panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
panel.border = element_rect(colour = "black"),strip.background = element_blank(),
legend.position = 'bottom',legend.justification='center',
legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))

ggplot(data=dat.merged) +
geom_point(aes(x=t2m.min,y=t2m.max)) +
xlab('Minimum temperature') +
ylab('Maximum tempearture') +
facet_wrap(~month) +
theme_bw() +
theme(panel.grid.major = element_blank(),text = element_text(size = 15),
axis.ticks.x=element_blank(),
panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
panel.border = element_rect(colour = "black"),strip.background = element_blank(),
legend.position = 'bottom',legend.justification='center',
legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))

dev.off()

# plot correlation by state
pdf(paste0(file.loc,'/',dname,'_',metric,'_state_',year.start,'_',year.end,'.pdf'),height=0,width=0,paper='a4r')
# overall plots
ggplot(data=dat.merged) +
geom_point(aes(x=t2m.mean,y=t2m.min)) +
xlab('Mean temperature') +
ylab('Mininum tempearture') +
facet_wrap(~state.fips) +
theme_bw() +
theme(panel.grid.major = element_blank(),text = element_text(size = 15),
axis.ticks.x=element_blank(),
panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
panel.border = element_rect(colour = "black"),strip.background = element_blank(),
legend.position = 'bottom',legend.justification='center',
legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))

ggplot(data=dat.merged) +
geom_point(aes(x=t2m.mean,y=t2m.max)) +
xlab('Mean temperature') +
ylab('Maximum tempearture') +
facet_wrap(~state.fips) +
theme_bw() +
theme(panel.grid.major = element_blank(),text = element_text(size = 15),
axis.ticks.x=element_blank(),
panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
panel.border = element_rect(colour = "black"),strip.background = element_blank(),
legend.position = 'bottom',legend.justification='center',
legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))

ggplot(data=dat.merged) +
geom_point(aes(x=t2m.min,y=t2m.max)) +
xlab('Minimum temperature') +
ylab('Maximum tempearture') +
facet_wrap(~state.fips) +
theme_bw() +
theme(panel.grid.major = element_blank(),text = element_text(size = 15),
axis.ticks.x=element_blank(),
panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
panel.border = element_rect(colour = "black"),strip.background = element_blank(),
legend.position = 'bottom',legend.justification='center',
legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))

dev.off()
