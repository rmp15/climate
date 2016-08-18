rm(list=ls())

library(ggplot2)
library(plyr)
library(foreign)
library(dplyr)

# create directory to place output files into
ifelse(!dir.exists("../../output/population_weighted_mean"), dir.create("../../output/population_weighted_mean"), FALSE)

# load county based population data
pop.county <- read.dta('../../data/population/processed/countyPopulations.dta')

# fix ages to fit age groups in model data
pop.county$age[pop.county$age==10] <- 5
pop.county$age[pop.county$age==20] <- 15
pop.county$age[pop.county$age==30] <- 25
pop.county$age[pop.county$age==40] <- 35
pop.county$age[pop.county$age==50] <- 45
pop.county$age[pop.county$age==60] <- 55
pop.county$age[pop.county$age==70] <- 65
pop.county$age[pop.county$age==80] <- 75

# summarise over new age groups
pop.county <- summarise(group_by(pop.county,sex,age,year,stateFips,countyFips),sum(pop))
names(pop.county)[6] <- 'pop'

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

saveRDS(pop.county.wm.month,'../../output/population_weighted_mean/state_population_weightings.rds')

####################################################################################################

# isolate for a few years if desired
#years.selected <- unique(pop.county.wm.month$year)
#pop.county.wm.month <- subset(pop.county.wm.month,year %in% years.selected)

# load temperature data (create dummy for now)
#temp.dummy <- expand.grid(year=unique(pop.county.wm.month$year),month=c(1:12),stateFips=unique(pop.county.wm$stateFips),countyFips=unique(pop.county.wm$countyFips))
#set.seed(112331)
#temp.dummy$temp <- rnorm(mean=300,dim(temp.dummy)[1],10)
#temp.dummy$heatwaves <- ceiling(runif(max=10,min=0,n=dim(temp.dummy)[1]))

# merge temperature data with population data
# not every county will exist throughout the history of the pop data
#pop.county.temp <- merge(pop.county.wm.month,temp.dummy,by=c('year','month','stateFips','countyFips'),all.x=1)

# summarise temperature data by state to get weighted mean value for use in model
#climate.weighted <- ddply(pop.county.temp,.(sex,age,year,month,stateFips),summarize,temp.weighted=sum(pop.weighted*temp),heatwave.weighted=sum(pop.weighted*heatwaves))

#write.csv(climate.weighted,'../../output/population_weighted_mean/climate_weighted_test.csv',row.names=FALSE)

# plot to check how the temperature for each age group for the same month varies
#plot(climate.weighted$temp.weighted[climate.weighted$age==0],climate.weighted$temp.weighted[climate.weighted$age==85])
#plot(climate.weighted$heatwaves[climate.weighted$age==0],climate.weighted$heatwaves[climate.weighted$age==85]
