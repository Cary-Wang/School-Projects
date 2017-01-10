# Cary Wang

library(ggmap)
library(dplyr)
library(shiny)
library(leaflet)
library(rvest)
library(stringr)

# ------------------------------------------------------------------------------------------
# PART 1: Creating the Dataframe
# Import 4 CSV datasets for the various crime locations
setwd("C://Users/Cary Wang/Documents/NYU/Analytics and the Digital Economy/Final Project")
nonCampus = read.csv("noncampuscrime121314.csv")
onCampus = read.csv('oncampuscrime121314.csv') 
publicProperty = read.csv('publicpropertycrime121314.csv')
resHall = read.csv('residencehallcrime121314.csv')

# Set onCampus dataset as the primary file that all others will be merged with. 
# Aggregate the data by overall university name rather than individual school name
# Specify the desired categorical and aggregated variables
onCampus_rapes = aggregate(cbind(onCampus$RAPE14,onCampus$FONDL14,onCampus$STATR14)~onCampus$INSTNM+onCampus$Sector_desc+onCampus$men_total+onCampus$women_total+onCampus$Total,FUN=sum)
onCampus_rapes = onCampus_rapes[order(-onCampus_rapes$V1),]
head(onCampus_rapes,20)

# Aggregate and merge residence hall data
res_rapes = aggregate(cbind(resHall$RAPE14,resHall$FONDL14,resHall$STATR14)~resHall$INSTNM, FUN=sum)
res_rapes = res_rapes[order(-res_rapes$V1),]
head(res_rapes,20)

# Aggregate and merge non-campus data
nonCampus_rapes = aggregate(cbind(nonCampus$RAPE14,nonCampus$FONDL14,nonCampus$STATR14)~nonCampus$INSTNM,FUN=sum)
nonCampus_rapes = nonCampus_rapes[order(-nonCampus_rapes$V1),]
head(nonCampus_rapes,20)

# Aggregate and merge public property data
publicProperty_rapes = aggregate(cbind(publicProperty$RAPE14, publicProperty$FONDL14,publicProperty$STATR14)~publicProperty$INSTNM,FUN=sum)
publicProperty_rapes = publicProperty_rapes[order(-publicProperty_rapes$V1),]
head(publicProperty_rapes,20)

# Create master data set, excluding residence hall because it is a subset of on-campus data
all_rapes = merge(onCampus_rapes,nonCampus_rapes,all.x=T, by.x="onCampus$INSTNM",by.y="nonCampus$INSTNM")
all_rapes = merge(all_rapes,publicProperty_rapes,all.x=T, by.x="onCampus$INSTNM",by.y="publicProperty$INSTNM")

# Rename columns to be more readable
colnames(all_rapes) = c("University","SchoolType","MenStud","WomenStud","TotalStud", "onCampus_Rapes","onCampus_Fond","onCampus_StatR", "nonCampus_Rapes","nonCampus_Fond","nonCampus_StatR","publicProperty_Rapes","publicProperty_Fond","publicProperty_StatR")

# Calculate sums of different types of sexual assault (rape, fondling, statutory rape) across each university
all_rapes$totalRapes = rowSums(all_rapes[,c("onCampus_Rapes","nonCampus_Rapes","publicProperty_Rapes")],na.rm = T)
all_rapes$totalFond = rowSums(all_rapes[,c("onCampus_Fond","nonCampus_Fond","publicProperty_Fond")],na.rm = T)
all_rapes$totalStatR = rowSums(all_rapes[,c("onCampus_StatR","nonCampus_StatR","publicProperty_StatR")],na.rm = T)
all_rapes$totalSexualAssault = rowSums(all_rapes[,c("totalRapes","totalFond","totalStatR")],na.rm = T)

# Calculate total number of reported sexual assaults in 2014
sum(all_rapes$totalSexualAssault) # 8,107 instances

# ------------------------------------------------------------------------------------------
# STEP 2: Mapping Sexual Assault
# Import university information dataset
univData = read.csv("postscndryunivsrvy2013dirinfo.csv")

# Merge desired location data into master dataset
all_rapes=merge(all_rapes,univData[,c("UNITID","INSTNM","ADDR","CITY","STABBR","ZIP","LONGITUD","LATITUDE")], all.x=T, by.x="University",by.y="INSTNM")

# Order by total sexual assaults descending
all_rapes = all_rapes[order(-all_rapes$totalSexualAssault),]

# Reorder dataframe to be more readable
all_rapes = all_rapes[,c(1:2, 19:25,3:18)]

# Calculate percentage of sexual assault variable
all_rapes$AssaultPerc = (all_rapes$totalSexualAssault/all_rapes$TotalStud)*100

# Calculate male/female ratio. Remove infinites values for all-male schools
all_rapes$MFRatio = (all_rapes$MenStud/all_rapes$WomenStud)
is.na(all_rapes$MFRatio) <- do.call(cbind,lapply(all_rapes$MFRatio, is.infinite))

# Find campuses with highest percentage of sexual assualt within student body. 
# Removed campuses with less than 1000 students
top_perc = all_rapes[which(all_rapes$TotalStud > 1000),]
top_perc = top_perc[order(-top_perc$AssaultPerc),]

# Write CSV files for mapping in Tableau
write.csv(all_rapes,file="AllRapes.csv")
write.csv(top_perc, file="TopPerc.csv")

# Find top 10 schools by sexual assault absolute and percentage
head(all_rapes,10)
head(top_perc,10)

# Create a map of top 100 universities of absolute sexual assualt instances
# Ended up using Tableau for map on site
assaultMap = get_map("US",zoom=4)
ggmap(assaultMap)
ggmap(assaultMap) + geom_point(data=all_rapes[1:100,],aes(LONGITUD,LATITUDE, color="red",size=all_rapes[1:100,]$totalSexualAssault))

# Create a map of top 100 universities of sexual assualt percentages
# Ended up using Tableau for map on site
percAssaultMap = get_map("US",zoom=4)
ggmap(percAssaultMap) + geom_point(data=top_perc[1:100,],aes(LONGITUD,LATITUDE,color="red",size=top_perc[1:100,]$AssaultPerc))

# ------------------------------------------------------------------------------------------
# STEP 3: Analyze Data About NYU
# Retreive data about NYU
# Find NYU ranking in total sexual assaults
which(all_rapes$University=="New York University") # Rank 83 in absolute assaults
which(top_perc$University=="New York University") # Rank 903 in assault percentage

# Create new dataset of only NYU data
onCampus_NYU = onCampus[which(onCampus$INSTNM=="New York University"),]
nonCampus_NYU = nonCampus[which(nonCampus$INSTNM=="New York University"),]
res_NYU = resHall[which(resHall$INSTNM=="New York University"),]
public_NYU = publicProperty[which(publicProperty$INSTNM=="New York University"),]
all_NYU = merge(onCampus_NYU,nonCampus_NYU[,c("INSTNM","BRANCH","RAPE14","FONDL14","STATR14")],all.x=T, by=c("INSTNM","BRANCH"))
all_NYU = merge(all_NYU,public_NYU[,c("INSTNM","BRANCH","RAPE14","FONDL14","STATR14")],all.x=T, by=c("INSTNM","BRANCH"))

# Get rid of unwanted columns
all_NYU[,c(13:32,35, 37:44)]=NULL

# Rename columns for enhanced readability
names(all_NYU)[13:21]= c("onCampus_RAPE14","onCampus_FONDL14","onCampus_STATR14","nonCampus_Rape","nonCampus_FONDL","nonCampus_STATR","public_RAPE","public_FONDL","public_STATR")

# Calculate sums of different types of sexual assault (rape, fondling, statutory rape) across each NYU campus
all_NYU$totalRapes = rowSums(all_NYU[,c("onCampus_RAPE14","nonCampus_Rape","public_RAPE")],na.rm = T)
all_NYU$totalFond = rowSums(all_NYU[,c("onCampus_FONDL14","nonCampus_FONDL","public_FONDL")],na.rm = T)
all_NYU$totalStatR = rowSums(all_NYU[,c("onCampus_STATR14","nonCampus_STATR","public_STATR")],na.rm = T)                                                                                                      
all_NYU$totalSexualAssault = rowSums(all_NYU[,c("totalRapes","totalFond","totalStatR")],na.rm=T)
all_NYU = all_NYU[order(-all_NYU$totalSexualAssault),]
# ------------------------------------------------------------------------------------------
# STEP 4: Run Regression on Admission Data
# Import college admission data and merge in SAT score data
admission_data = read.csv("adm2014.csv")
all_rapes=merge(all_rapes,admission_data[,c("UNITID","SATVR75","SATMT75","SATWR75")], all.x=T,by="UNITID")
all_rapes$SATCOMP75 = rowSums(all_rapes[,c("SATVR75","SATMT75","SATWR75")],na.rm = F)
head(all_rapes,50)

# Run regression on SAT score, total number of students, male-female ratio, and type of school
fit =lm(AssaultPerc ~ SATCOMP75+TotalStud+MFRatio+SchoolType,data=all_rapes, na.action = na.exclude)
summary(fit)
# ------------------------------------------------------------------------------------------
# STEP 5: Run Regression on Party School Rankings
# Scrape Princeton Review website for Top 20 Party Schools and rankings
party_schools = read_html("https://www.princetonreview.com/college-rankings?rankings=party-schools")
party_schools = html_nodes(party_schools,".margin-top-none a")
party_schools = html_text(party_schools)

# Split rank and school name into two columns 
party_schools = data.frame(do.call('rbind',strsplit(party_schools,"[.]")))
colnames(party_schools) = c("Rank","University")

# Clean up text to ensure proper joining by name
party_schools$University = trimws(party_schools$University)
party_schools$University[19] = "University of Colorado Boulder"
party_schools$University[11] = "University of California-Santa Barbara"
party_schools$University[9] = "Tulane University of Louisiana"

# Merge party school ranking into new dataframe of 20 elements
party_assaults = merge(all_rapes,party_schools,all.y=T, by=as.character("University"))
party_assaults$Rank = as.numeric(levels(party_assaults$Rank))[party_assaults$Rank]
plot(party_assaults$Rank,party_assaults$totalSexualAssault)

# Run regression on the 20 schools for SAT score, total number of students, school type, and rank on the list
fit2 =lm(totalSexualAssault ~ SATCOMP75+TotalStud+MFRatio+SchoolType+Rank,data=party_assaults)
summary(fit2)
