rm(list=ls())

print('running population_weighted_mean.R')

library(ggplot2)
library(plyr)
library(foreign)
library(dplyr)

# create directory to place output files into
ifelse(!dir.exists("../../output/population_weighted_mean"), dir.create("../../output/population_weighted_mean"), FALSE)

# load county based population data
pop.county <- read.dta("~/data/mortality/US/state/processed/county/countyPopulationsnewyears.dta")

# fix ages to fit age groups in model data
pop.county$age[pop.county$age==10] <- 5
pop.county$age[pop.county$age==20] <- 15
pop.county$age[pop.county$age==30] <- 25
pop.county$age[pop.county$age==40] <- 35
pop.county$age[pop.county$age==50] <- 45
pop.county$age[pop.county$age==60] <- 55
pop.county$age[pop.county$age==70] <- 65
pop.county$age[pop.county$age==80] <- 75

# fix county codes for transient fips codes
# 08 013 to 08 014
# 12 025 to 12 086
# 30 113 to 30 031
# 46 113 to 46 102
# 51 560 to 51 005
# 51 780 to 51 083
# 51 515 to 51 019
pop.county$countyFips[pop.county$stateFips=='08' & pop.county$countyFips=='013'] <- '014'
pop.county$countyFips[pop.county$stateFips=='12' & pop.county$countyFips=='025'] <- '086'
pop.county$countyFips[pop.county$stateFips=='30' & pop.county$countyFips=='113'] <- '031'
pop.county$countyFips[pop.county$stateFips=='46' & pop.county$countyFips=='113'] <- '102'
pop.county$countyFips[pop.county$stateFips=='51' & pop.county$countyFips=='560'] <- '005'
pop.county$countyFips[pop.county$stateFips=='51' & pop.county$countyFips=='780'] <- '083'
pop.county$countyFips[pop.county$stateFips=='51' & pop.county$countyFips=='515'] <- '019'

# summarise over new age groups
pop.county <- summarise(dplyr::group_by(pop.county,sex,age,year,stateFips,countyFips),pop=sum(pop))

# remove '99' age, which is the summation of all ages in a county
# could use this if wanted to population weight across all age groups to create one temperature per state-month
# rather than per state month age combination
pop.county <- subset(pop.county, age!=99)

# create summary table to get state populations
pop.county.wm <- ddply(pop.county,.(sex,age,year,stateFips),summarize,pop.state=sum(pop))

# merge to get weighted population by county
pop.county.wm <- merge(pop.county,pop.county.wm,by=c('sex','age','year','stateFips'),all.x=1)
pop.county.wm$pop.weighted <- with(pop.county.wm,pop/pop.state)

# use same population for each month in the year by repeating each row 12 times
# may use inferred population at a later date
pop.county.wm.month <- do.call("rbind", replicate(12, pop.county.wm, simplify = FALSE))
pop.county.wm.month$month <- rep(1:12,each=dim(pop.county.wm)[1])
names(pop.county.wm.month)[4]<-'state.fips'
names(pop.county.wm.month)[5]<-'county.fips'
names(pop.county.wm.month)[6]<-'pop.county'
pop.county.wm.month$state.county.fips <- paste0(pop.county.wm.month$state.fips,pop.county.wm.month$county.fips)

#saveRDS(pop.county.wm.month,'../../output/population_weighted_mean/state_population_weightings.rds')
# create directory to place output files into
ifelse(!dir.exists("~/data/climate/population_weightings/"), dir.create("~/data/climate/population_weightings/"), FALSE)
saveRDS(pop.county.wm.month,'~/data/climate/population_weightings/state_population_weightings.rds')

print('done')
