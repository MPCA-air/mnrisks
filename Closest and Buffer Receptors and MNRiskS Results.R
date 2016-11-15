##Set working directory
setwd("M:/KME Files/Model Monitor/2011_AllSources/MnRiskS Results")

##Pull in libraries
library(tidyr)
library(dplyr)
library(stringr)
library(foreign)

##Read and sum PM2.5 results
finepart <- readRDS("M:/KME Files/Model Monitor/2011_AllSources/MnRiskS Results/gcp - statewide - all sources except fire events -  PM25 concentration primary&secondary.rds")

finepart <- gather(finepart, TimeAverage, AirConcentration, Hourly:Annual)
finepart <- finepart[, c("GEOID", "utm_x", "utm_y", "County", "Receptor", "CAS", "Pollutant", "TimeAverage", "AirConcentration")]
finepart_sum <- group_by(finepart, GEOID, utm_x, utm_y, County, Receptor, TimeAverage) %>% summarize(Pollutant = "Particulate Matter <2.5", CAS = "E1647635", AirConcentration = sum(AirConcentration, na.rm=T))

finepart_final <- spread(finepart_sum, TimeAverage, AirConcentration)
finepart_final <- mutate(finepart_final, Coords=paste0(utm_x, "_", utm_y))
finepart_final$utm_x=NULL
finepart_final$utm_y=NULL

##Read other criteria results and combine data sets
no2 <- readRDS("M:/KME Files/Model Monitor/2011_AllSources/MnRiskS Results/gcp - statewied - all sources - NO2 concentration2.rds")
so2 <- readRDS("M:/KME Files/Model Monitor/2011_AllSources/MnRiskS Results/gcp - statewied - all sources - SO2 concentration.rds")
othercriteriapollutants <- readRDS("M:/KME Files/Model Monitor/2011_AllSources/MnRiskS Results/kme - statewide - all sources - remaining criteria concentrations.rds")

criteria <- rbind(no2, so2, othercriteriapollutants)
criteria <- mutate(criteria, Coords=paste0(utm_x, "_", utm_y))
criteria <- criteria[, c("GEOID", "Coords", "County", "Receptor", "CAS", "Pollutant", "Hourly", "Annual")]

##Read and combine voc datasets
vocah <- readRDS("M:/KME Files/Model Monitor/2011_AllSources/MnRiskS Results/kme - statewide - all sources - vocsA_H concentrations.rds")
vociz <- readRDS("M:/KME Files/Model Monitor/2011_AllSources/MnRiskS Results/kme - statewide - all sources - vocsM_Z concentrations.rds")
vocs <- rbind(vocah, vociz)
vocs <- mutate(vocs, Coords=paste0(utm_x, "_", utm_y))
vocs <- vocs[, c("GEOID", "Coords", "County", "Receptor", "CAS", "Pollutant", "Hourly", "Annual")]

##Read in Carbonyls
carbonyls <- readRDS("M:/KME Files/Model Monitor/2011_AllSources/MnRiskS Results/kme - statewide - all sources - carbonyls concentrations.rds")
carbonyls <- mutate(carbonyls, Coords=paste0(utm_x, "_", utm_y))
carbonyls <- carbonyls[, c("GEOID", "Coords", "County", "Receptor", "CAS", "Pollutant", "Hourly", "Annual")]

##Read in Metals
metals <- readRDS("M:/KME Files/Model Monitor/2011_AllSources/MnRiskS Results/kme - statewide - all sources - metals - concentrations.rds")
metals <- mutate(metals, Coords=paste0(utm_x, "_", utm_y))
metals <- metals[, c("GEOID", "Coords", "County", "Receptor", "CAS", "Pollutant", "Hourly", "Annual")]
chromium <- filter(metals, Pollutant %in% c("CHROM6 CMP", "Chromium (III)"))
nochromium <- filter(metals, !Pollutant %in% c("CHROM6 CMP", "Chromium (III)"))
chromium <- group_by(chromium, GEOID, Coords, County, Receptor) %>% summarize(Hourly=sum(Hourly, na.rm=T), Annual=sum(Annual, na.rm=T))
chromium <- mutate(chromium, CAS="7440-47-3", Pollutant="Chromium")
metals <- rbind(chromium, nochromium)
##Pull together all pollutant concentrations
allconcentrations <- rbind(finepart_final, criteria, vocs, carbonyls, metals)

##Read in receptors in buffers
receptorsinbuffers <- read.dbf("M:/KME Files/Model Monitor/2011_AllSources/Maps/receptorsinbuffers2011c.dbf")

##Join by the Receptors in the Receptors in Buffers dataframe
allconcentrations_buffers <- left_join(receptorsinbuffers, allconcentrations)
allconcentrations_buffers_summary <- group_by(allconcentrations_buffers, FID_5kmbuf, mpcaid, aqsid, state, county, BUFF_DIST, CAS, Pollutant) %>% summarize(Maximum=max(Annual, na.rm=T), Minimum=min(Annual, na.rm=T), Mean=mean(Annual, na.rm=T))
allconcentrations_buffers_summary <- gather(allconcentrations_buffers_summary, summary, mod, Maximum:Mean)
#allconcentrations_buffers_summary <- allconcentrations_buffers_summary[, c("Receptor", "CAS", "Pollutant", "Annual")]
write.csv(allconcentrations_buffers_summary, "Buffers_MnRiskS_Results_2011.csv")

##Read in receptors closest to air monitoring sites
receptorsclosest <- read.dbf("M:/KME Files/Model Monitor/2011_AllSources/Maps/Receptors_Final_Closestb.dbf")

##Join by the Receptors in the Receptors in Buffers dataframe
allconcentrations_closest <- left_join(receptorsclosest, allconcentrations)
allconcentrations_closest <- allconcentrations_closest[, c("Receptor", "CAS", "Pollutant", "Annual")]
allconcentrations_closest <- unique(allconcentrations_closest)
write.csv(allconcentrations_closest, "Closest_MnRiskS_Results_2011.csv")
