rm(list=ls())

library(plyr)
library(ggplot2)

# arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)
year.start <- as.numeric(args[1])
year.end <- as.numeric(args[2])
dname <- as.character(args[3])
metric <- as.character(args[4])

# create directory
dir = paste0("../../output/metrics_statistics/",dname,"/",metric,"/")
ifelse(!dir.exists(dir), dir.create(dir, recursive=TRUE), FALSE)

# load file with climate data
dat <- readRDS(paste0('../../output/metrics_development/',dname,'/',metric,'_',dname,'/state_weighted_summary_',metric,'_',dname,'_',year.start,'_',year.end,'.rds'))
dat$state.fips <- as.numeric(as.character(dat$state.fips))

# load state names
state.lookup <- read.csv('~/git/mortality/USA/state/data/fips_lookup/name_fips_lookup.csv')

# merge state names
dat <- merge(dat,state.lookup,by.x='state.fips',by.y='fips')

# rename to generalise code
names(dat)[grep(dname,names(dat))] <- 'variable'
dat$variable.rounded <- round(dat$variable)

# trim years of interest
dat = subset(dat,age==85 & sex==2)

# add short names for months
month.short <- c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')
month.lookup = data.frame(month=c(1:12),month.short=month.short)
dat = merge(dat,month.lookup,by=('month'))
dat$month.short <- reorder(dat$month.short,dat$month)

# create directory
dir.sub = paste0(dir,year.start,"_",year.end,"/")
ifelse(!dir.exists(dir.sub), dir.create(dir.sub, recursive=TRUE), FALSE)

# plot values per state over time
pdf(paste0(dir.sub,'linegraph_over_time_',dname,'_',metric,'_',year.start,'_',year.end,'_state.pdf'),paper='a4r',height=0,width=0)
ggplot() + geom_line(data=dat,aes(x=year,y=variable,color=full_name)) + ylab(paste0(dname,'_',metric)) + facet_wrap(~month.short)
dev.off()

# max and min for pdf graph
max = max(dat$variable.rounded)
min = min(dat$variable.rounded)

# function to decide which period to cover
summary.function = function(year.start.arg,year.end.arg) {
    
    # create directory
    dir.sub = paste0(dir,year.start.arg,"_",year.end.arg,"/")
    ifelse(!dir.exists(dir.sub), dir.create(dir.sub, recursive=TRUE), FALSE)
    
    # create year range for inclusion in analysis
    years = c(year.start.arg:year.end.arg)
    
    # trim years of interest
    dat = subset(dat,age==85 & sex==2 & year %in% years)

# summarise how many for one age-sex group (they're all essentially equivalent here)
dat.summary <- count(subset(dat,age==85 & sex==2 & year %in% years),'variable.rounded')
dat.summary$percentage <- with(dat.summary, round(100*freq/nrow(subset(dat,age==85 & sex==2)),1))

dat.summary.state <- count(subset(dat,age==85 & sex==2 & year %in% years),c('full_name','variable.rounded'))
dat.summary.state$percentage <- with(dat.summary.state, round(100*freq/nrow(subset(dat,age==85 & sex==2 & full_name=='Alabama')),1))

# histogram of varaiable nationally
pdf(paste0(dir.sub,'histogram_',dname,'_',metric,'_',year.start.arg,'_',year.end.arg,'_national.pdf'),paper='a4r',height=0,width=0)
#hist(dat$variable,nclass=10,main=paste0('histogram_',dname,'_',metric,'_',year.start,'_',year.end))
print(ggplot() + geom_histogram(data=dat,aes(variable)) + ggtitle(paste0('histogram nationally ',dname,' ',metric,' ',year.start.arg,' ',year.end.arg)))
dev.off()

# histogram of varaiable subnationally
pdf(paste0(dir.sub,'histogram_',dname,'_',metric,'_',year.start.arg,'_',year.end.arg,'_subnational.pdf'),paper='a4r',height=0,width=0)
print(ggplot() + geom_histogram(data=dat,aes(variable)) + facet_wrap(~full_name) + ggtitle(paste0('histogram subnationally ',dname,' ',metric,' ',year.start.arg,' ',year.end.arg)))
dev.off()

# special case for up or downwaves
wave.test = grepl('waves', metric)
ymax=50
if(wave.test==TRUE){ymax=100}

# line graph of percentage of variable subnationally
pdf(paste0(dir.sub,'linegraph_',dname,'_',metric,'_',year.start.arg,'_',year.end.arg,'_subnational.pdf'),paper='a4r',height=0,width=0)
print(ggplot() + geom_line(data=dat.summary.state,aes(x=variable.rounded,y=percentage,color=full_name)) + xlim(c(min,max)) + ylim(c(0,ymax)) + ggtitle(paste0('proportion subnationally ',dname,' ',metric,' ',year.start.arg,' ',year.end.arg)))
dev.off()

# export data
saveRDS(dat.summary,paste0(dir.sub,"national_summary_",dname,"_",metric,"_",year.start.arg,"_",year.end.arg))
write.csv(dat.summary,paste0(dir.sub,"national_summary_",dname,"_",metric,"_",year.start.arg,"_",year.end.arg,'.csv'))

saveRDS(dat.summary.state,paste0(dir.sub,"subnational_summary_",dname,"_",metric,"_",year.start.arg,"_",year.end.arg))
write.csv(dat.summary.state,paste0(dir.sub,"subnational_summary_",dname,"_",metric,"_",year.start.arg,"_",year.end.arg,'.csv'))

}

# ENTIRE PERIOD
summary.function(year.start,year.end)

# FIRST DECADE
summary.function(1980,1989)

# SECOND DECADE
summary.function(1990,1999)

# THIRD DECADE
summary.function(2000,2009)



