##### Apportion processed emissions for

#Gas Stations
#Airports
#High Traffic
#CMV
#Trains

##Load Packages
library(dplyr)
library(stringr)
library(RODBC)
library(reshape2)
library(readr)
library(Rcpp)


setwd("M:/Emissions/2011 Emissions/2. Processed Emissions")

#**Compare total facility emissions

#==========================#
#Gas Stations
#==========================#

#Load Emissions
gas <- read_csv("Gas-Stations Emissions for Apportioning.csv")
names(gas)[1] <- "County"
gas$County <- gsub("[.]", "", gas$County)

#Load station fractions
gas_frx <- read_csv("M:/Emissions/2011 Emissions/3. Apportioned Emissions/Apportioning Fractions/Gas Station Sales v3.csv")

gas_frx$County <- toupper(gas_frx$County)

gas_frx <- group_by(gas_frx, County) %>% mutate(County_Tot = sum(Pct_County_Sales))

gas_frx <- mutate(ungroup(gas_frx), Pct_County_Sales = Pct_County_Sales / County_Tot)

#QA County total fractions
qa <- group_by(gas_frx, County) %>% summarize(County_Tot = sum(Pct_County_Sales))

unique(qa$County_Tot)

gas_frx$County_Tot <- NULL

#Check for County emissions with no gas stations
unique(gas$County)[!unique(gas$County) %in% unique(gas_frx$County)]

unique(gas_frx$County)[!unique(gas_frx$County) %in% unique(gas$County)]


#Attach sales fractions
gas_final <- left_join(gas_frx, gas, by = "County")

#Apportion emissions
gas_final <- mutate(gas_final, Emissions = Emissions * Pct_County_Sales)

#QC county emission totals
if(round(sum(gas$Emissions)) == round(sum(gas_final$Emissions))) {
  print("Emissions total is the same.")
}

gas_final <- gas_final[ , -c(12:15)]
gas_final <- gas_final[ , c(1:9,12:18,10:11)]

# Clean and Save results
write.csv(gas_final, "M:\\MNRiskS 2011 development\\Send to Lakes\\Emissions\\Gas Stations Apportioned for LAKES v3.csv", row.names = F)


#==========================#
#Airports
#==========================#

#Load point Emissions
planes <- read_csv("Airport Point Processed Emissions.csv")
names(planes)[1] <- "County"

#Calculate each airport's fraction of COUNTY emissions
planes <- group_by(planes, County) %>% mutate(County_Total = sum(Emissions, na.rm = T))

planes <- group_by(planes, SOURCE_ID) %>% mutate(County_Frx = sum(Emissions, na.rm = T) / County_Total[1])

a <- group_by(planes, County) %>% summarize(County_frx_Tot = sum(County_Frx[Pollutant == Pollutant[1]]))
unique(a$County_frx_Tot)

#Calculate each airport's fraction of STATE emissions
planes <- mutate(ungroup(planes), State_Total_Pb = sum(Emissions[Pollutant == "LEAD"], na.rm = T))

planes <- group_by(planes, SOURCE_ID) %>% mutate(State_Frx = sum(Emissions[Pollutant == "LEAD"], na.rm = T) / State_Total_Pb)

a <- filter(ungroup(planes), !duplicated(SOURCE_ID)) %>% summarize(State_frx_Tot = sum(State_Frx))
unique(a$State_frx_Tot)


#QC
a <- group_by(planes, County) %>% summarize(County_Tot = sum(County_Frx))


#Load non-point airport emissions
planes_np <- read_csv("Airports Emissions for Apportioning.csv")
names(planes_np)[1] <- "County"

planes_flight <- filter(planes_np, County == "Statewide")  
planes_np <- filter(planes_np, County!= "Statewide")  

#Check for County emissions with no airports
unique(planes_np$County)[!unique(planes_np$County) %in% planes$County]
unique(planes$County)[!unique(planes$County) %in% planes_np$County]


#Attach airport fractions
planes_np <- left_join(planes_np, planes[!duplicated(planes$SOURCE_ID), c(1,3,10)], by = "County")

planes_flight <- cbind(planes[planes$Pollutant == "LEAD", ], planes_flight$Emissions)  

#Apportion non-point emissions
planes_np <- mutate(planes, Emissions = Emissions * County_Frx)

planes_flight$Emissions = planes_flight$'planes_flight$Emissions' * planes_flight$State_Frx

a <- arrange(filter(planes_flight, Pollutant == "LEAD"), SOURCE_ID)

#QC
sum(planes_np[!duplicated(planes_np$SOURCE_ID), ]$County_Frx) == length(unique(planes_np$County))
sum(planes_flight$State_Frx)

#Combine point and non-point emissions
planes_all <- rbind(planes[ , 1:8], planes_np[ , c(1:8)])

planes_all <- rbind(planes_all, planes_flight[ , 1:8])

planes_all <- group_by(planes_all, SOURCE_ID, Pollutant) %>% mutate(Emissions = sum(Emissions, na.rm = T))

a <- arrange(filter(planes_all, Pollutant == "LEAD"), SOURCE_ID)

planes_all <- unique(planes_all)

#QC
sum(c(planes$Emissions, planes_np$Emissions, planes_flight$Emissions)) == sum(planes_all$Emissions)


#Save results
write.csv(planes_all, "M:/Emissions/2011 Emissions/3. Apportioned Emissions/Airports Apportioned for LAKES.csv", row.names = F)


#==========================#
#High Traffic
#==========================#

#Load Emissions
ht <- read_csv("High Traffic Processed Emissions.csv")
names(ht)[1] <- "County"


#ht$Source.Category <- "High Traffic Line Sources"

#Load high traffic fractions
ht_frx <- read_csv("M:/Emissions/2011 Emissions/3. Apportioned Emissions/Apportioning Fractions/High Traffic Fractions.csv")
#ht_ID <- read_csv("M:/Emissions/2011 Emissions/3. Apportioned Emissions/Apportioning Fractions/High Traffic IDs.csv")
#ht_ID <- mutate(ht_ID, Coords_TIS = paste(UTMX, UTMY, TIS_CODE))
#ht_frx$GEOID10 <- NULL
#ht_frx$UTMX <- NULL
#ht_frx$UTMY <- NULL
#ht_frx <- left_join(ht_frx, ht_ID[ , c(5:6,10)])
#ht_frx <- arrange(ht_frx, HT_ID)
#ht_frx$GEOID10 <- as.character(ht_frx$GEOID10)
#ht_frx$UTMX <- as.character(ht_frx$UTMX)
#ht_frx$UTMY <- as.character(ht_frx$UTMY)
#write_csv(ht_frx[ , c(12,1,2,13:15,3:11)], "M:/Emissions/2011 Emissions/3. Apportioned Emissions/Apportioning Fractions/High Traffic Fractions.csv")

names(ht_frx)[2] <- "County"
ht_frx$County <- toupper(ht_frx$County)

ht_frx <- group_by(ht_frx, County) %>% mutate(County_Tot_HT = sum(Point_VKT))
ht_frx <- group_by(ht_frx, County) %>% mutate(County_VKT_frx = Point_VKT / County_Tot_HT)

a <- group_by(ht_frx, County) %>% summarize(County_Check = sum(County_VKT_frx))
unique(a$County_Check)

#Check for County emissions with no high traffic
unique(ht$County)[!unique(ht$County) %in% unique(ht_frx$County)]
unique(ht_frx$County)[!unique(ht_frx$County) %in% unique(ht$County)]

#Attach fractions
ht_frx <- left_join(ht_frx, ht, by = "County")

#Final emissions
ht2 <- mutate(ht_frx, Emissions = Emissions * County_VKT_frx)

if(round(sum(ht$Emissions)) == round(sum(ht2$Emissions))){
  print("Emissions total is the same.")
  ht_frx <- mutate(ht_frx, Emissions = Emissions * County_VKT_frx)
}

#QC

#Save results
ht_frx <- ungroup(ht_frx)[ , names(ht_frx)[!names(ht_frx) %in% c("County_Total_VKT", "HighT_County_Frx", "Total_County_HighT_AADT")]]

write.csv(ht_frx, "M:/Emissions/2011 Emissions/3. Apportioned Emissions/High Traffic Apportioned for LAKES.csv", row.names = F)


#==========================#
#CMV
#==========================#

#Load point Emissions
ports <- read_csv("CMV Point Processed Emissions.csv")
ports_bk <- ports
names(ports)[1] <- "County"
ports$Port_FID <- str_split(ports$SOURCE_ID, "CMV")

ports$Row_ID <- 1:nrow(ports)

ports <- group_by(ports, Row_ID) %>% mutate(Port_FID = as.integer(Port_FID[[1]][2]))

#Load port locations
port_bgs <- read_csv("M:\\Emissions\\2011 Emissions\\3. Apportioned Emissions\\Apportioning Fractions\\CMV Port BGs.csv")
names(port_bgs)[2] <- "Port_FID"

#Join port blockgroups
ports <- left_join(ports, port_bgs[ , -1])

a <- filter(ports, Port_FID > 9000)

#Load block group fraction of underway section
river_frx <- read_csv("M:\\Emissions\\2011 Emissions\\3. Apportioned Emissions\\Apportioning Fractions\\CMV Underway BG fractions.csv")
names(river_frx)[3] <- "Port_FID"

river_frx$BG_frx <- river_frx$BG_Shape_Leng / river_frx$Shape_Length


#Join underway blockgroups
ports <- left_join(ports, river_frx[ , c(3,10,22)], by = "Port_FID")

a <- filter(ports, !is.na(GEOID10.x), !is.na(GEOID10.y))

ports$Row_ID <- 1:nrow(ports)

ports <- group_by(ports, Row_ID) %>% mutate(GEOID10 = max(c(GEOID10.x, GEOID10.y), na.rm =T))
ports$County <- substr(ports$GEOID10, 1, 5) 

ports$GEOID10.x <- NULL
ports$GEOID10.y <-NULL

a <- filter(ports, Port_FID > 9000)
b <- filter(ports, Port_FID < 9000)
c <- filter(ports, Port_FID == 2510)
nrow(filter(b, is.na(BG_frx)))
nrow(filter(ports, is.na(GEOID10)))


#QC
a <- group_by(ports[ports$Pollutant == ports$Pollutant[1], ], Port_FID) %>% summarize(Underway_Tot = sum(BG_frx))


#Calculate each BG's fraction of COUNTY emissions
#cmv <- group_by(cmv, County) %>% mutate(County_Total = sum(Emissions, na.rm = T))
#cmv <- group_by(cmv, GEOID10) %>% mutate(County_Frx = sum(Emissions, na.rm = T) / County_Total[1])
#a <- group_by(cmv, County) %>% summarize(County_frx_Tot = sum(County_Frx[Pollutant == Pollutant[1]]))
#unique(a$County_frx_Tot)

#QC
#a <- group_by(planes, County) %>% summarize(County_Tot = sum(County_Frx))


#Apportion port emissions
ports <- mutate(ungroup(ports), Emissions = ifelse(Port_FID < 9000, Emissions * BG_frx, Emissions))

a <- arrange(filter(ports, Pollutant == "LEAD"), SOURCE_ID)

#QC
round(sum(ports[ports$Pollutant == "LEAD", ]$BG_frx, na.rm = T)) == length(unique(filter(ports, Port_FID < 9000)$Port_FID))


#Combine point and underway emissions
ports <- group_by(ports, GEOID10, Pollutant) %>% mutate(Emissions = sum(Emissions, na.rm = T))

a <- arrange(filter(ports, Pollutant == "LEAD"), GEOID10)

ports$BG_frx <- NULL
ports$Port_FID <- NULL
ports$Row_ID <- NULL
ports$SOURCE_ID <- NULL
ports$County <- NULL
ports$FIPS <- NULL

ports <- unique(ports)

#Q
abs(sum(ports$Emissions) - sum(ports_bk$Emissions)) < sum(ports_bk$Emissions) * .000001


#Save results
write.csv(ports, "M:/Emissions/2011 Emissions/3. Apportioned Emissions/Block Group Emissions/CMV Apportioned to BGs for LAKES.csv", row.names = F)


#==========================#
#Trains
#==========================#

#Load point Emissions
stations <- read_csv("Trains Point Processed Emissions.csv")
stations_bk <- stations
names(stations)[1] <- "County"

stations$Station_FID <- str_split(stations$SOURCE_ID, "P")

stations$Row_ID <- 1:nrow(stations)
stations <- group_by(stations, Row_ID) %>% mutate(Station_FID = as.integer(Station_FID[[1]][2]))

#Load station locations
station_bgs <- read_csv("M:\\Emissions\\2011 Emissions\\3. Apportioned Emissions\\Apportioning Fractions\\Rail Station BGs.csv")
names(station_bgs)[3] <- "Station_FID"

#Join station blockgroups
stations <- left_join(stations, station_bgs[ , c(3,13)])

a <- filter(stations, is.na(GEOID10))

#Calculate each BG's fraction of COUNTY rail station emissions
stations <- group_by(stations, County) %>% mutate(County_Total = sum(Emissions, na.rm = T))
stations <- group_by(stations, GEOID10) %>% mutate(County_Frx = sum(Emissions, na.rm = T) / County_Total[1])

#QC
a <- group_by(stations, County) %>% summarize(County_frx_Tot = sum(County_Frx[Pollutant == Pollutant[1]]))
summary(a$County_frx_Tot)

sum(stations$Emissions) == sum(stations_bk$Emissions)

######===================#
## Rail lines
######===================#

#Load segment rail line emissions
railways <- read_csv("Rail Line Processed Emissions.csv")
railways_bk <- railways
railways$Rail_FID <- str_split(railways$SOURCE_ID, "RR")

railways$Row_ID <- 1:nrow(railways)
railways <- group_by(railways, Row_ID) %>% mutate(Rail_FID = as.integer(Rail_FID[[1]][2]))

a <- filter(railways, is.na(Rail_FID))

#Load rail line blockgroup fractions
conn <- odbcConnectExcel2007("M:/MNRiskS 2011 development/Rail/Railway by block group intersect.xlsx") ##Connect to the Excel File
railway_BGs <- sqlFetch(conn, "Railway by block group intersec") # read a table
names(railway_BGs)[3] <- "Rail_FID"
close(conn) # close the connection to the file

#QC
a <- group_by(railway_BGs, Rail_FID) %>% summarize(Seg_frx_Tot = sum(Seg_frx_length))
summary(a$Seg_frx_Tot)

#Attach blockgroup fractions
railways <- left_join(railways, railway_BGs[ , c(3,17,25,26,27)])
railways$COUNTY_NAME <- NULL
railways$FIPS <- substr(railways$GEOID10, 1, 5) 
railways$BG_frx <- railways$Segment_length_m / railways$Shape_length_m

a <- filter(railways, is.na(GEOID10))
a <- filter(railways, is.na(BG_frx))

#QC
a <- group_by(railways, Pollutant, Rail_FID) %>% summarize(Rail_Seg_Tot = sum(BG_frx))
summary(a$Rail_Seg_Tot)

#Apportion railway emissions
railways <- mutate(ungroup(railways), Emissions_portion = Emissions * BG_frx)

#QC
a <- arrange(filter(railways, Pollutant == "LEAD"), SOURCE_ID)

railways$Emissions <- railways$Emissions_portion
railways$Emissions_portion <- NULL

sum(railways$Emissions) == sum(railways_bk$Emissions)


#Calculate each BG's fraction of COUNTY rail *LINE* emissions
railways <- group_by(railways, FIPS) %>% mutate(County_Total = sum(Emissions, na.rm = T))
railways <- group_by(railways, GEOID10) %>% mutate(County_Frx = sum(Emissions, na.rm = T) / County_Total[1])

#QC
a <- filter(ungroup(railways), FIPS == 27001, Pollutant == "LEAD")
a <- group_by(railways, FIPS, Pollutant) %>% filter(!duplicated(GEOID10)) %>% summarize(County_frx_Tot = sum(County_Frx))
summary(a$County_frx_Tot)

#SAVE BG emission fractions
rail_frx <- filter(ungroup(railways), !duplicated(GEOID10))[ , c(1,6,10,16)]
write.csv(rail_frx, "M:/Emissions/2011 Emissions/3. Apportioned Emissions/Apportioning Fractions/Railway BG frxs for Counties - Wtd by emissions.csv", row.names = F)


#Load COUNTY rail line emissions
railways_np <- read_csv("Rail Emissions for Apportioning.csv")
railways_np_bk <- railways_np
railways_np$FIPS <- as.character(railways_np$FIPS)

#Join and apportion non-point rail line emissions
railways_np <- left_join(railways_np, rail_frx[ , -2], by = "FIPS")

railways_np <- mutate(ungroup(railways_np), Emissions = Emissions * County_Frx)

#QC
sum(railways_np$Emissions) == sum(railways_np_bk$Emissions)
a <- filter(railways_np, is.na(County_Frx))

#Join rail line, rail line non-point, and rail-station emissions
railways <- rbind(railways[ , c(1,3:7,10)], railways_np[ , c(2:6,8:9)])

abs(sum(railways_bk$Emissions) + sum(railways_np_bk$Emissions) - sum(railways$Emissions)) <= sum(railways$Emissions) * .000001

railways <- rbind(railways, stations[ , c(2,4:8,11)])

abs(sum(stations_bk$Emissions) + sum(railways_bk$Emissions) + sum(railways_np_bk$Emissions) - sum(railways$Emissions)) <= sum(railways$Emissions) * .000001

a <- arrange(filter(ungroup(railways), Pollutant == "NOX"), GEOID10)


#Aggregate emissions by blockgroup #GEOID10
railways <- group_by(ungroup(railways), GEOID10, Pollutant) %>% mutate(Emissions = sum(Emissions, na.rm = T))

b <- arrange(filter(railways, Pollutant == "NOX"), GEOID10)

railways$Category.SCC <- "NR_RAIL"
railways$Source.Category <- "Rail and switchyards"
railways$MNRiskS.category.name <- "nonroad - rail" 

railways <- unique(railways)


#QC
abs((sum(railways_bk$Emissions) + sum(railways_np_bk$Emissions) + sum(stations_bk$Emissions)) - sum(railways$Emissions)) <  sum(railways$Emissions) * .00000000001

#Save results
write.csv(railways, "M:/Emissions/2011 Emissions/3. Apportioned Emissions/Block Group Emissions/Trains Apportioned to BGs for LAKES.csv", row.names = F)
