
###This script joins MNRiskS Results that are within a 5km buffer of an Ambient Air Site and Measurements from that Site. The script then also pulls together MnRiskS results at the nearest receptor to an ambient air monitoring site and that measured data.


##Set Working Directory
setwd("M:/KME Files/Model Monitor/2011_AllSources/Statistics")
##Pull in Libraries
library(tidyr)
library(dplyr)
library(foreign)
library(stringr)

##Pull in Ambient Air Data

##Need to pull in combined criteria and air toxics files.
allmeasurements_2011 <- read.csv("M:/KME Files/Model Monitor/2011_AllSources/Ambient Air Data/allmeasurements_2011.csv", stringsAsFactors=FALSE)

#mdls <- readRDS("X:/Programs/Air_Quality_Programs/Air Monitoring Data and Risks/6 Air Data/Monitoring Data/Air Toxics/AirToxics2002-2015.rds")
#mdls_2011 <- filter(mdls, Year==2011)
#mdls_2011 <- mdls_2011[, c("Dlimit (ug/m3)", "ShortName", "CAS Num")]
#mdls_2011 <- unique(mdls_2011)
#colnames(mdls_2011) <- c("MDL", "Pollutant", "CAS")

#allmeasurements_2011_mdls <- left_join(allmeasurements_2011, mdls_2011)
#allmeasurements_2011_mdls <- mutate(allmeasurements_2011_mdls, Censored = ifelse(is.na(Mean), 0, 1)) %>% ungroup()
#allmeasurements_2011_mdls <- group_by(allmeasurements_2011_mdls, Pollutant, CAS, Param_Code) %>% mutate(Censored_Sum = sum(Censored)) %>% ungroup()
#allmeasurements_2011 <- mutate(allmeasurements_2011_mdls, Mean = ifelse(Censored_Sum>1 & is.na(Mean), MDL, Mean)) %>% ungroup()

##Pull out Parameter Code and Average by CAS. This takes a mean of PM2.5 across Parameter types.
allmeasurements_2011$Param_Code <- NULL
allmeasurements_2011$X <- NULL
allmeasurements_2011 <- group_by(allmeasurements_2011, Pollutant, CAS, pollutanttype, aqsid, mpcaid) %>% summarize(Mean=mean(Mean, na.rm=T), StdDev=mean(StdDev, na.rm=T)) %>% ungroup()
##, MDL=mean(MDL, na.rm=T
##, Censored=paste(Censored, collapse=",")
##Pull in MNRiskS Results
mnrisks_buffers <- read.csv("M:/KME Files/Model Monitor/2011_AllSources/MnRiskS Results/Buffers_MnRiskS_Results_2011.csv", stringsAsFactors=FALSE)
mnrisks_buffers <- mutate(mnrisks_buffers, mpcaid=paste0(str_pad(state, 2, pad="0"), str_pad(county, 3, pad="0"), str_pad(mpcaid, 4, pad="0"))) %>% ungroup()


mnrisks_closest <- read.csv("M:/KME Files/Model Monitor/2011_AllSources/MnRiskS Results/Closest_MnRiskS_Results_2011.csv", stringsAsFactors=FALSE)

##Pull in Receptors Closest to Ambient Air Monitors
receptorsclosest <- read.dbf("M:/KME Files/Model Monitor/2011_AllSources/Maps/Receptors_Final_Closestc.dbf", as.is=TRUE)
receptorsclosest <- mutate(receptorsclosest, mpcaid=paste0(str_pad(State_Code, 2, pad="0"), str_pad(County_Cod, 3, pad="0"), str_pad(MPCA_ID_LO, 4, pad="0"))) %>% ungroup()

#######################################################
##Pull in Receptors within 5km of an Ambient Air Site
#######################################################
receptorsinbuffers <- read.dbf("M:/KME Files/Model Monitor/2011_AllSources/Maps/receptorsinbuffers2011c.dbf", as.is=TRUE)
receptorsinbuffers <- mutate(receptorsinbuffers, mpcaid=paste0(str_pad(state, 2, pad="0"), str_pad(county, 3, pad="0"), str_pad(mpcaid, 4, pad="0")))

#######################################################
##Combine Measurements and Closest Receptors and Site IDS and MNRiskS Results
#######################################################
allmeasurements_2011$mpcaid <- as.character(allmeasurements_2011$mpcaid)
allmeasurements_2011$aqsid <- as.character(allmeasurements_2011$aqsid)
receptorsclosest$AQS_Site_I <- gsub("-", "", receptorsclosest$AQS_Site_I)
allmeasurements_receptors_closest_mpcaid <- left_join(allmeasurements_2011, receptorsclosest)
allmeasurements_receptors_closest_aqsid <- left_join(allmeasurements_2011, receptorsclosest, by=c("aqsid" = "AQS_Site_I"))

allmeasurements_receptors_closest_mpcaid <- mutate(allmeasurements_receptors_closest_mpcaid, SITEID=mpcaid, SITEIDTYPE="MPCAID") %>% ungroup()
allmeasurements_receptors_closest_mpcaid <- allmeasurements_receptors_closest_mpcaid[, c("Pollutant", "CAS", "pollutanttype", "Mean", "StdDev","Distance", "State_Code", "County_Cod", "Latitude", "Longitude", "long", "lat", "utm_x", "utm_y", "County", "Receptor", "SITEID", "SITEIDTYPE")]

allmeasurements_receptors_closest_aqsid <- mutate(allmeasurements_receptors_closest_aqsid, SITEID=aqsid, SITEIDTYPE="AQSID") %>% ungroup()
allmeasurements_receptors_closest_aqsid <- allmeasurements_receptors_closest_aqsid[, c("Pollutant", "CAS", "pollutanttype", "Mean", "StdDev","Distance", "State_Code", "County_Cod", "Latitude", "Longitude", "long", "lat", "utm_x", "utm_y", "County", "Receptor", "SITEID", "SITEIDTYPE")]

allmeasurements_receptors_closest <- rbind(allmeasurements_receptors_closest_mpcaid, allmeasurements_receptors_closest_aqsid)
#write.csv(allmeasurements_receptors_closest, "allmeasurements_receptors_closest.csv")
mnrisks_measurements_2011 <- merge(allmeasurements_receptors_closest, mnrisks_closest, by = c("Receptor", "CAS"))

allmeasurements_receptors_closest <- mnrisks_measurements_2011[, c("Receptor", "CAS", "Pollutant.x", "Mean", "StdDev", "pollutanttype", "Distance", "Annual", "SITEID", "SITEIDTYPE", "Pollutant.y")]
##"Censored", 
colnames(allmeasurements_receptors_closest) <- c("Receptor", "CAS", "substance", "obs", "obsstddev", "airpollutanttype", "Distance", "mod", "site", "SITEIDTYPE", "Pollutant_MNRiskS")
##"Censored", 
allmeasurements_receptors_closest <- unique(allmeasurements_receptors_closest)
allmeasurements_receptors_closest <- filter(allmeasurements_receptors_closest, !is.na(obs))
allmeasurements_receptors_closest <- spread(allmeasurements_receptors_closest, SITEIDTYPE, site)
allmeasurements_receptors_closest <- mutate(allmeasurements_receptors_closest, site = ifelse(is.na(MPCAID), AQSID, MPCAID)) %>% ungroup()
allmeasurements_receptors_closest$AQSID <- NULL
allmeasurements_receptors_closest <- unique(allmeasurements_receptors_closest)
write.csv(allmeasurements_receptors_closest, "M:/KME Files/Model Monitor/2011_AllSources/Statistics/Data Tables and Processing/allmeasurements_receptors_closest.csv")

###NEED TO REDO BUFFERS######################################################
#############################################################################
##Combine Measurements and Buffer Receptors and Site IDS and MNRiskS Results
#############################################################################
allmeasurements_2011$mpcaid <- as.character(allmeasurements_2011$mpcaid)
allmeasurements_2011$aqsid <- as.character(allmeasurements_2011$aqsid)
mnrisks_buffers$mpcaid <- as.character(mnrisks_buffers$mpcaid)
mnrisks_buffers$aqsid <- as.character(mnrisks_buffers$aqsid)

allmeasurements_receptors_buffer_mpcaid <- merge(allmeasurements_2011, mnrisks_buffers, by=c("mpcaid", "CAS"))
allmeasurements_receptors_buffer_aqsid <- merge(allmeasurements_2011, mnrisks_buffers, by=c("aqsid", "CAS"))

allmeasurements_receptors_buffer_mpcaid$aqsid.x <- NULL
allmeasurements_receptors_buffer_mpcaid$aqsid.y <- NULL
allmeasurements_receptors_buffer_mpcaid <- mutate(allmeasurements_receptors_buffer_mpcaid, SITEID=mpcaid, SITEIDTYPE="MPCAID") %>% ungroup()
allmeasurements_receptors_buffer_mpcaid$mpcaid <- NULL

allmeasurements_receptors_buffer_aqsid$mpcaid.x <- NULL
allmeasurements_receptors_buffer_aqsid$mpcaid.y <- NULL
allmeasurements_receptors_buffer_aqsid <- mutate(allmeasurements_receptors_buffer_aqsid, SITEID=aqsid, SITEIDTYPE="AQSID") %>% ungroup()
allmeasurements_receptors_buffer_aqsid$aqsid <- NULL

allmeasurements_receptors_buffer <- rbind(allmeasurements_receptors_buffer_mpcaid, allmeasurements_receptors_buffer_aqsid)
allmeasurements_receptors_buffer <- unique(allmeasurements_receptors_buffer)
allmeasurements_receptors_buffer <- allmeasurements_receptors_buffer[, c("CAS", "Pollutant.x", "Mean", "StdDev", "pollutanttype", "FID_5kmbuf", "BUFF_DIST", "summary", "mod", "SITEID", "SITEIDTYPE")]
##, "Censored"
colnames(allmeasurements_receptors_buffer) <- c("CAS", "substance", "obs", "obsstddev", "airpollutanttype", "FID_5kmbuf", "BUFF_DIST", "summary", "mod", "site", "SITEIDTYPE")
##, "Censored"
allmeasurements_receptors_buffer <- spread(allmeasurements_receptors_buffer, SITEIDTYPE, site)
allmeasurements_receptors_buffer <- mutate(allmeasurements_receptors_buffer, site = ifelse(is.na(MPCAID), AQSID, MPCAID)) %>% ungroup()
allmeasurements_receptors_buffer$AQSID <- NULL
allmeasurements_receptors_buffer <- unique(allmeasurements_receptors_buffer)
write.csv(allmeasurements_receptors_buffer, "M:/KME Files/Model Monitor/2011_AllSources/Statistics/Data Tables and Processing/allmeasurements_receptors_buffer.csv")

