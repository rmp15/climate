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
dir = paste0("../../output/metrics_statistics/",dname,"/",metric)
ifelse(!dir.exists(dir), dir.create(dir, recursive=TRUE), FALSE)

# load file with climate data
dat <- readRDS(paste0('../../output/metrics_development/',dname,'/',metric,'_',dname,'/state_weighted_summary_',metric,'_',dname,'_',year.start,'_',year.end,'.rds'))
dat$state.fips <- as.numeric(as.character(dat$state.fips))

# load state names
state.lookup <- read.csv('~/git/mortality/USA/state/data/fips_lookup/name_fips_lookup.csv')

# merge state names
dat <- merge(dat,state.lookup,by.x='state.fips',by.y='fips')

names(dat)[grep(dname,names(dat))] <- 'variable'

dat$variable.rounded <- round(dat$variable)

# summarise how many for one age-sex group (they're all essentially equivalent here)
dat.summary <- count(subset(dat,age==85 & sex==2),'variable.rounded')
dat.summary$percentage <- with(dat.summary, round(100*freq/nrow(subset(dat,age==85 & sex==2)),1))

dat.summary.state <- count(subset(dat,age==85 & sex==2),c('full_name','variable.rounded'))
dat.summary.state$percentage <- with(dat.summary.state, round(100*freq/nrow(subset(dat,age==85 & sex==2 & full_name=='Alabama')),1))

# histogram of varaiable nationally
pdf(paste0(dir,'/','histogram_',dname,'_',metric,'_',year.start,'_',year.end,'_national.pdf'),paper='a4r',height=0,width=0)
#hist(dat$variable,nclass=10,main=paste0('histogram_',dname,'_',metric,'_',year.start,'_',year.end))
ggplot() + geom_histogram(data=dat,aes(variable)) + ggtitle(paste0('histogram nationally ',dname,' ',metric,' ',year.start,' ',year.end))
dev.off()

# histogram of varaiable subnationally
pdf(paste0(dir,'/','histogram_',dname,'_',metric,'_',year.start,'_',year.end,'_subnational.pdf'),paper='a4r',height=0,width=0)
ggplot() + geom_histogram(data=dat,aes(variable)) + facet_wrap(~full_name) + ggtitle(paste0('histogram subnationally ',dname,' ',metric,' ',year.start,' ',year.end))
dev.off()

# line graph of percentage of variable subnationally
pdf(paste0(dir,'/','linegraph_',dname,'_',metric,'_',year.start,'_',year.end,'_subnational.pdf'),paper='a4r',height=0,width=0)
ggplot() + geom_line(data=dat.summary.state,aes(x=variable.rounded,y=percentage,color=full_name)) + ggtitle(paste0('proportion subnationally ',dname,' ',metric,' ',year.start,' ',year.end))
dev.off()

# export data
saveRDS(dat.summary,paste0("../../output/metrics_statistics/",dname,"/",metric,"/national_summary_",dname,"_",metric,"_",year.start,"_",year.end))
write.csv(dat.summary,paste0("../../output/metrics_statistics/",dname,"/",metric,"/national_summary_",dname,"_",metric,"_",year.start,"_",year.end,'.csv'))

saveRDS(dat.summary.state,paste0("../../output/metrics_statistics/",dname,"/",metric,"/subnational_summary_",dname,"_",metric,"_",year.start,"_",year.end))
write.csv(dat.summary.state,paste0("../../output/metrics_statistics/",dname,"/",metric,"/subnational_summary_",dname,"_",metric,"_",year.start,"_",year.end,'.csv'))

