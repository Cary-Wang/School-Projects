library(plyr)
library(lubridate)
library(scales)
library(ggplot2)
library(ggmap)

setwd("C:/Users/Cary Wang/Documents/NYU/Analytics and the Digital Economy/HW1")

# 1a
# Import data,reformat inspection date, and set 5-year date range 
rodent_data = read.csv("Rodent_Inspection.csv")
rodent_data$INSPECTION_DATE = as.Date(rodent_data$INSPECTION_DATE, format='%m/%d/%Y')
rodent_data=rodent_data[which(rodent_data$INSPECTION_DATE>='2009-11-01' & rodent_data$INSPECTION_DATE<='2014-11-01'),]


# Use lubridate to parse out month and year and create a new month-year date field
rodent_data$INSPECTION_DATE_MONYR = paste(year(rodent_data$INSPECTION_DATE),'-',month(rodent_data$INSPECTION_DATE),sep='')
rodent_data$INSPECTION_DATE_MONYR = as.Date(paste(rodent_data$INSPECTION_DATE_MONYR,'-01',sep=''))

# Create dataframe of just active rat signs
rat_sightings = rodent_data[which(rodent_data$RESULT=='Active Rat Signs'),]

# Create two dataframes using plyr to find frequencies per month-year and borough 
all_inspections=count(rodent_data,c("INSPECTION_DATE_MONYR","BOROUGH"))
active_rat_instances = count(rat_sightings,c("INSPECTION_DATE_MONYR", "BOROUGH"))
all_inspections$activeRat=active_rat_instances$freq

# Plot line graph using ggplot2, grouping by borough
base = ggplot(all_inspections, aes(x=INSPECTION_DATE_MONYR,y=activeRat, color=BOROUGH))+geom_line()+labs(title="Rat Sightings over Time",x="Month-Year",y="Rat Sightings")
base + (scale_x_date(breaks=date_breaks("4 month"),labels=date_format("%m %y")))

# 1b
# Create "efficiency" field and plot
all_inspections$efficiency=active_rat_instances$freq/all_inspections$freq
base_efficiency = ggplot(all_inspections, aes(x=INSPECTION_DATE_MONYR,y=efficiency,color=BOROUGH))+geom_line()+labs(title="Rat Inspection Efficiency over Time",x="Month-Year",y="Inspection Efficiency")
base_efficiency + (scale_x_date(breaks=date_breaks("4 month"),labels=date_format("%m %y")))

#1c
# Use plyr to create a dataframe of the top 10 zipcodes throughout NYC  
active_rat_zip = count(rat_sightings,c("ZIP_CODE","BOROUGH"))
active_rat_zip = arrange(active_rat_zip,desc(active_rat_zip$freq))
head(active_rat_zip,10)

#1d
# Use ggmap to plot frequencies on Google Maps
map = get_map(location="Central Park",zoom=10)
map_points = ggmap(map)+geom_point(x=rodent_data$LONGITUDE,y=rodent_data$LATITUDE,size=.03,data=rodent_data,color='blue',alpha=.8)+labs(title="Rat Inspections throughout NYC")
map_points

#2a
# Create 3 data frames - one with 2 years data before Sandy, 2 years data after Sandy, and data during Sandy
rat_sightings_pre_sandy=rodent_data[which(rodent_data$INSPECTION_DATE_MONYR>='2010-01-01' & rodent_data$INSPECTION_DATE_MONYR <='2011-12-31' & rodent_data$RESULT=='Active Rat Signs'),]
active_rat_zip_pre = count(rat_sightings_pre_sandy,c("ZIP_CODE"))
active_rat_zip_pre = arrange(active_rat_zip_pre,desc(active_rat_zip_pre$freq))
head(active_rat_zip_pre,10)

rat_sightings_post_sandy=rodent_data[which(rodent_data$INSPECTION_DATE_MONYR>='2013-01-01' & rodent_data$INSPECTION_DATE_MONYR <='2014-12-31' & rodent_data$RESULT=='Active Rat Signs'),]
active_rat_zip_post = count(rat_sightings_post_sandy,c("ZIP_CODE"))
active_rat_zip_post = arrange(active_rat_zip_post,desc(active_rat_zip_post$freq))
head(active_rat_zip_post,10)

# Import Sandy data and limit to when descriptor equals 'Rat Sighting'
sandy_data=read.csv("sandyrelated.csv")
sandy_rats = sandy_data[which(sandy_data$Descriptor=='Rat Sighting'),]

# Create dataframe with view of frequency by zipcode
sandy_zip = count(sandy_rats, "Incident.Zip")
sandy_zip = arrange(sandy_zip,desc(sandy_zip$freq))
head(sandy_zip,10)

#3a
# Merge together data from all inspections and a rat-only dataframe
all=count(rodent_data,c("INSPECTION_DATE_MONYR","ZIP_CODE"))
only_rats = count(rat_sightings,c("INSPECTION_DATE_MONYR", "ZIP_CODE"))
merged_data = merge(x=all,y=only_rats, by=c("INSPECTION_DATE_MONYR","ZIP_CODE"),all.x=TRUE)
merged_data
# Set all NA variables to 0
merged_data$R[is.na(merged_data)] = 0

# Create efficiency field
merged_data$ActiveRatSightings = merged_data$freq.y/merged_data$freq.x

# Parse out months and years and clean up merged dataframe
merged_data$MONTH = month(merged_data$INSPECTION_DATE_MONYR)
merged_data$YEAR = year(merged_data$INSPECTION_DATE_MONYR)
merged_data=rename(merged_data,c("freq.x"="ALL_INSPECTIONS", "freq.y"="RAT_SIGHTINGS", "ZIP_CODE"="ZIPCODE"))
merged_data$INSPECTION_DATE_MONYR = NULL
head(merged_data,10)

# Import restaurant inspection data and format dates
restaurant_data = read.csv("dohmh.csv")
restaurant_data$INSPECTION.DATE = as.Date(restaurant_data$INSPECTION.DATE, format='%m/%d/%Y')

# Parse out months and years
restaurant_data$MONTH = month(restaurant_data$INSPECTION.DATE)
restaurant_data$YEAR = year(restaurant_data$INSPECTION.DATE)

# Create dummy variable with nested ifelse statements
restaurant_data$RAT_VIOLATION=ifelse("04K" == restaurant_data$VIOLATION.CODE,1, ifelse("04L" == restaurant_data$VIOLATION.CODE,1,ifelse("08A" == restaurant_data$VIOLATION.CODE,1,0)))

# Merge rodent and restaurant dataframes
restaurant_data = merge(x=restaurant_data, y=merged_data, by=c("MONTH","YEAR","ZIPCODE"))

# Change Month and Year into factors
restaurant_data$MONTH = as.factor(restaurant_data$MONTH)
restaurant_data$YEAR = as.factor(restaurant_data$YEAR)

# Run logistic regression

fit=glm(RAT_VIOLATION~ActiveRatSightings + MONTH + YEAR,data=restaurant_data,family=binomial(link="logit"))
summary(fit)



