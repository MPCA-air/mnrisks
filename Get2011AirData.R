##########################################
##Set working directory and pull libraries
##########################################
library(dplyr)
library(tidyr)
library(RODBC)
library(stringr)
setwd("M:/KME Files/Model Monitor/2011_AllSources/Ambient Air Data")

data <- readRDS("X:/Programs/Air_Quality_Programs/Air Monitoring Data and Risks/6 Air Data/Monitoring Data/Air Toxics/AirToxics2002-2015.rds")
data <- filter(data, Year==2011)
source("MLE_Stats_EnvStats.R")
source("bootEnvStats.R")
set.seed(27)
tox_summary <- envStats.summary(data    = data,
                                SITEID           = "AQS_ID", 
                                Results          = "Concentration",
                                MDL              = "MDL", 
                                Year             = "Year",
                                Date             = "Date",
                                Pollutant        = "Pollutant",
                                Param_Code       = "Param_Code",
                                POC              = "POC",
                                Percent_Cutoff   = 10,
                                Minimum_Samples  = 45,
                                Minimum_Detects  = 6,
                                Boot_Repeats     = 3000,
                                seed             = 27)

#write.csv(tox_summary, "airtoxics_2011.csv")

##Air Toxics
airtoxics <- airtoxics[airtoxics$Year %in% "2011", c("SITEID", "Pollutant", "CAS", "Year", "Param_Code", "Mean", "StdDev")]  
#airtoxics <- mutate(airtoxics, groupid=paste0(MPCAID, Param_Code, "1"), poc=1, scheduled=60, count_valid=Count_Sampled, pct_complete=scheduled/count_valid, count_zero=NA, pct_zero=NA, count_detect=Count_Detect)
airtoxics <- mutate(airtoxics, pollutanttype="airtoxics", Year=NULL, aqsid=SITEID)
airtoxics$SITEID <- NULL
airtoxics <- unique(airtoxics)
write.csv(airtoxics, "airtoxics_2011.csv")
airtoxics <- read.csv("M:/KME Files/Model Monitor/2011_AllSources/Ambient Air Data/airtoxics_2011.csv", stringsAsFactors=FALSE)
airtoxics$X <- NULL

##Nitrogen Dioxide
no2 <- as.data.frame(read.table("X:/Programs/Air_Quality_Programs/Air Monitoring Data and Risks/6 Air Data/Monitoring Data/NO2/NO2 1990-2014.txt", sep="|", header=TRUE, skip=2, comment.char = "#"), stringsAsFactors=FALSE)
no2 <- no2[, 3:13]
colnames(no2) <- c("StateCode", "CountyCode", "SiteID", "Parameter", "POC", "SampleDuration", "Unit", "Method", "Date", "StartTime", "SampleValue")
no2 <- mutate(no2, SampleValue = SampleValue*1.88*1000)

##PM2.5
pm2588101 <- as.data.frame(read.table("X:/Programs/Air_Quality_Programs/Air Monitoring Data and Risks/6 Air Data/Monitoring Data/PM2.5/PM25_88101_1999_2014.txt", sep="|", header=TRUE, skip=2, comment.char = "#"), stringsAsFactors=FALSE)
pm2588101 <- pm2588101[, 3:13]
colnames(pm2588101) <- c("StateCode", "CountyCode", "SiteID", "Parameter", "POC", "SampleDuration", "Unit", "Method", "Date", "StartTime", "SampleValue")

pm2588502 <- as.data.frame(read.table("X:/Programs/Air_Quality_Programs/Air Monitoring Data and Risks/6 Air Data/Monitoring Data/PM2.5/PM25_88502_1999_2014.txt", sep="|", header=TRUE, skip=2, comment.char = "#"), stringsAsFactors=FALSE)
pm2588502 <- pm2588502[, 3:13]
colnames(pm2588502) <- c("StateCode", "CountyCode", "SiteID", "Parameter", "POC", "SampleDuration", "Unit", "Method", "Date", "StartTime", "SampleValue")

pm25 <- rbind(pm2588502, pm2588101)
pm25$Date <- as.character(pm25$Date)
pm25_2011 <- pm25[grepl("2011", pm25$Date)==TRUE,]

##PM10
pm10 <- as.data.frame(read.table("X:/Programs/Air_Quality_Programs/Air Monitoring Data and Risks/6 Air Data/Monitoring Data/PM10/PM10_1990_2014.txt", sep="|", header=TRUE, skip=2, comment.char = "#"), stringsAsFactors=FALSE)
pm10 <- pm10[, 3:13]
colnames(pm10) <- c("StateCode", "CountyCode", "SiteID", "Parameter", "POC", "SampleDuration", "Unit", "Method", "Date", "StartTime", "SampleValue")
pm10_2011 <- pm10[grepl("2011", pm10$Date)==TRUE,]

##SO2
SO2 <- as.data.frame(read.table("X:/Programs/Air_Quality_Programs/Air Monitoring Data and Risks/6 Air Data/Monitoring Data/SO2/SO2_1990-2014.txt", sep="|", header=TRUE, skip=2, comment.char = "#"), stringsAsFactors=FALSE)
SO2 <- SO2[, 3:13]
colnames(SO2) <- c("StateCode", "CountyCode", "SiteID", "Parameter", "POC", "SampleDuration", "Unit", "Method", "Date", "StartTime", "SampleValue")
SO2 <- mutate(SO2, SampleValue = ifelse(Unit==8, SampleValue*2.7, SampleValue*2.7*1000))

##CO
CO <- as.data.frame(read.table("X:/Programs/Air_Quality_Programs/Air Monitoring Data and Risks/6 Air Data/Monitoring Data/CO/HourlyCO 2004-2014.txt", sep="|", header=TRUE, skip=2, comment.char = "#"), stringsAsFactors=FALSE)
CO <- CO[, 3:13]
colnames(CO) <- c("StateCode", "CountyCode", "SiteID", "Parameter", "POC", "SampleDuration", "Unit", "Method", "Date", "StartTime", "SampleValue")
CO <- mutate(CO, SampleValue = ifelse(Unit==8, SampleValue*1.145, SampleValue*1.145*1000))

##Combine and Summarize
criteria <- rbind(pm25, pm10, SO2, no2, CO)

criteria$Parameter <- as.character(criteria$Parameter)
criteria$SiteID <- as.character(criteria$SiteID)
criteria$Date <- as.character(criteria$Date)
criteria$County <- as.character(criteria$County)
criteria$State <- as.character(criteria$State)
criteria$SampleValue <- as.numeric(criteria$SampleValue)

criteria <- mutate(criteria, Year = substr(Date, 1, 4))
criteria <- filter(criteria, Year=="2011")
criteria_summary <- group_by(criteria, Parameter, SiteID, Date, Year, County, State) %>%
  summarize(POCMean=mean(SampleValue, na.rm=T), POCMaximum=max(SampleValue, na.rm=T)) %>%
  ungroup()

criteria_summary <- group_by(criteria_summary, Parameter, SiteID, County, State) %>%
  summarize(Mean=mean(POCMean, na.rm=T), Maximum=max(POCMaximum, na.rm=T), StdDev = sd(POCMean, na.rm=T)) %>%
  ungroup()

criteria_summary <- mutate(criteria_summary, 
                           Maximum = ifelse(Parameter=="88502", NA, Maximum), 
                           sitepar=paste0(SiteID, Parameter), 
                           Rsiteparameter= paste0(Parameter, SiteID, County, State), 
                           aqsid=paste0(str_pad(State, 2, pad="0"), str_pad(County, 3, pad="0"), str_pad(SiteID, 4, pad="0")))

write.csv(criteria_summary, "criteria_summary_2011.csv")
criteria_summary <- read.csv("M:/KME Files/Model Monitor/2011_AllSources/Ambient Air Data/criteria_summary_2011.csv", stringsAsFactors = FALSE)
criteria_summary <- criteria_summary[, c("Parameter", "Mean", "StdDev", "aqsid")]
criteria_summary$Parameter <- as.character(criteria_summary$Parameter)
criteria_summary <- mutate(criteria_summary, 
          pollutanttype="criteria", 
          Pollutant=ifelse(Parameter=="42101", "CarbonMonoxide", ifelse(Parameter=="42401", "SulfurDioxide", ifelse(Parameter=="42602", "NitrogenDioxide", ifelse(Parameter=="81102", "PM10", "PM2.5")))), 
          CAS = ifelse(Parameter=="42101", "630-08-0", ifelse(Parameter=="42401", "7446-09-5", ifelse(Parameter=="42602", "E17134099", ifelse(Parameter=="81102", "E1647619", "E1647635")))))
colnames(criteria_summary) <- c("Param_Code", "Mean", "StdDev", "aqsid", "pollutanttype", "Pollutant", "CAS")

##Include AQSID and MPCAID
conn = odbcConnectExcel("M:/KME Files/Model Monitor/2011_AllSources/Ambient Air Data/site information list - all sites.xlsx") # open a connection to the Excel file
sites = sqlFetch(conn, "sites - locations", stringsAsFactors=FALSE) # read a sheet
close(conn) # close the connection to the file
sites <- sites[, c("aqsid", "last4aqsid", "MPCAID")]
sites <- mutate(sites, mpcaid = paste0(substr(aqsid, 1, 5), str_pad(MPCAID, 4, pad="0", side="left")))
sites$aqsid <- as.character(sites$aqsid)


allmeasurements_2011 <- rbind(airtoxics, criteria_summary)
allmeasurements_2011$aqsid <- as.character(allmeasurements_2011$aqsid)
allmeasurements_2011 <- left_join(allmeasurements_2011, sites)
allmeasurements_2011$MPCAID <- NULL
allmeasurements_2011$last4aqsid <- NULL
write.csv(allmeasurements_2011, "M:/KME Files/Model Monitor/2011_AllSources/Ambient Air Data/allmeasurements_2011.csv")
