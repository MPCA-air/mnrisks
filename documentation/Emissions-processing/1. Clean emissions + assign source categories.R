##### Pull Together Lists of Pollutants, Sources, and Units
##### for Non-Point Emissions

##Load Packages
library(dplyr)
library(stringr)
library(RODBC)
library(reshape2)
library(readr)
library(Rcpp)

#Prevent SCCs turning into scientific notation
options(scipen = 999)

#======================#
# Get surrogate SCC groups
#======================#
conn <- odbcConnectExcel2007("M:\\MNRiskS 2011 development\\Surrogates\\2011_Source categories & methods.xlsx") # open a connection to the Excel file

surr_SCCs <- data.frame(sqlFetch(conn, "2011_selected categories", as.is = T))[1:550, ]
close(conn) # close the connection to the file

surr_SCCs <- filter(surr_SCCs, !is.na(Category.SCC))
surr_SCCs$SCC <- str_trim(surr_SCCs$SCC)
surr_SCCs$SCC.Codes <- str_trim(surr_SCCs$SCC.Codes)
surr_SCCs$MNRiskS.category.name <- str_trim(surr_SCCs$MNRiskS.category.name)
surr_SCCs$Category.SCC <- str_trim(surr_SCCs$Category.SCC)
surr_SCCs$Source.Category <- str_trim(surr_SCCs$Source.Category)

#==================================================================#
## Pull all emission files into one
#==================================================================#
column_names <- c("COUNTY_NAME", "SOURCE_ID", "EMISSION_UNIT_ID", "PROCESS_ID", "SHORT_DESC", "SOURCE_TYPE", "SCC_CODE", "MATERIAL_CODE", "EMISSION_AMT", "EMISSION_UNIT_CODE", "FUGITIVE_EMISSION_AMT", "PRIORITY_NO")

setwd("M:\\Emissions\\2011 Emissions\\1. CEDR Emissions 8_10_15")

Emis_All <- data.frame()

for(file in list.files()[!grepl("~", list.files()) & grepl("[.]tab", list.files())]){
  
  print(file)
  
  ##Using RODBC
  #conn <- odbcConnectExcel2007(file)  
  #Emis <- sqlFetch(conn, gsub("['$]","", sqlTables(conn)$TABLE_NAME[[1]]), rownames = F, as.is = T)
  #odbcCloseAll()
  
  if(file == 'Facility Point 2013.tab'){ Emis <- read_tsv(file, col_types = "ccccccccccccccccccccccccccccccccc")
  } else Emis <- read_tsv(file, col_types = "ccccccccccccccccccccccccccccccccc")
  
  Emis$SCC_CODE    <- Emis$PROCESS_ID
  Emis$SHORT_DESC  <- Emis$SOURCE_ID  
  Emis$SHORT_NAME  <- Emis$SOURCE_ID  
  Emis$SOURCE_TYPE <- gsub("[.]tab", "", file)
  
  if(file == 'Facility Point 2013.tab'){
    Emis$SOURCE_ID <- paste0(Emis$SOURCE_ID, "_2013")
    
    Emis <- filter(Emis, COUNTY_NAME != "PORTABLE SOURCES",
                   MATERIAL_CODE %in% c("PM25-FIL", "PM10-FIL", "PM10-PRI", "PM", "PM-FIL", "PM-CON", "CO", "VOC", "SO2", "NOX", "LEAD"))
  }
  
  Emis_All <- rbind(Emis[ , column_names], Emis_All)
  rm(Emis)
}


#Attach unique ID to emissions
Emis_All <- mutate(Emis_All, ID = paste0(COUNTY_NAME, SOURCE_ID, EMISSION_UNIT_ID, SCC_CODE, PROCESS_ID, MATERIAL_CODE))

Emis_All$RELEASE_POINT_ID <- NA
Emis_All$FLOW_PCT <- NA

#Check for duplicate emissions
a <- filter(Emis_All,is.na(EMISSION_AMT)) 
a <- filter(Emis_All, duplicated(ID)) 

unique(a$SOURCE_TYPE)

b <- filter(a, SOURCE_TYPE == "Mobile")
b <- filter(a, SOURCE_TYPE == "Facility Point 2011")
b <- filter(a, SOURCE_TYPE == "Facility Point 2013")

c <- filter(Emis_All, ID == b$ID[1])
#c <- filter(Emis_All, ID %in% b$ID)

c <- arrange(c, ID)
rm(b)

#========================================================#
##Select top priority emissions for duplicate point source emissions
#========================================================#
unique(Emis_All$PRIORITY_NO)

Emis_All <- group_by(Emis_All, ID) %>% mutate(Top_Priority = min(as.numeric(PRIORITY_NO), na.rm=T))

Emis_All <- group_by(Emis_All, ID) %>% filter(as.numeric(PRIORITY_NO) == Top_Priority)

Emis_All <- ungroup(Emis_All)

Emis_All$Top_Priority <- NULL

Emis_All$PRIORITY_NO <- NULL

#========================================================#
##Remove duplicate rows
#========================================================#  
a <- Emis_All[duplicated(Emis_All$ID), ]

Emis_All <- Emis_All[!duplicated(Emis_All$ID), ]

#Drop dry cleaner's total VOCs
Emis_All <- filter(Emis_All, SCC_CODE != '2420000000')

#Replace NA's
Emis_All <- mutate(Emis_All, FUGITIVE_EMISSION_AMT = replace(FUGITIVE_EMISSION_AMT, is.na(FUGITIVE_EMISSION_AMT), 0))
Emis_All <- mutate(Emis_All, FUGITIVE_EMISSION_AMT = NA)

Emis_All <- mutate(Emis_All, EMISSION_AMT = replace(EMISSION_AMT, is.na(EMISSION_AMT), 0))

#========================================================#
##Format data
#========================================================#
##Create a FIPS Code column
Emis_All <- mutate(Emis_All, FIPS = substr(SOURCE_ID, 1, 5))

##Simplify column names
names(Emis_All)[names(Emis_All) == "MATERIAL_CODE"] <- "Pollutant"
names(Emis_All)[names(Emis_All) == "SCC_CODE"] <- "SCC"

Emis_All$EMISSION_AMT <- as.numeric(Emis_All$EMISSION_AMT)

#Set county as "Statewide" for in-flight emissions
Emis_All[Emis_All$SCC == 2275087000, ]$COUNTY_NAME <- "Statewide"

#Create backup of emissions for QC at end of processing
Emis_All_bkup <- Emis_All

#=============================================================#
##Combine the non-road and CMV process IDs with matching SCCs
#=============================================================# 
mobile_cmv <- filter(Emis_All, SOURCE_TYPE %in% c("Mobile", "CMV Point"))

mobile_cmv$SCC <- gsub("[A-z]", "", mobile_cmv$PROCESS_ID) 

#Update ID
mobile_cmv <- mutate(ungroup(mobile_cmv), ID = paste0(COUNTY_NAME, SOURCE_ID, EMISSION_UNIT_ID, SCC, Pollutant))

#Sum duplicate nonRoad SCCs
mobile_cmv <- group_by(mobile_cmv, ID) %>% mutate(EMISSION_AMT = sum(EMISSION_AMT, na.rm = T))

mobile_cmv <- mobile_cmv[!duplicated(mobile_cmv$ID), ]

Emis_All <- rbind(filter(Emis_All, !SOURCE_TYPE %in% c("Mobile", "CMV Point")), mobile_cmv)

rm(mobile_cmv)

#========================================================#
##Attach point source SCCs
#========================================================# 

#For rail-yards
Emis_All[Emis_All$SOURCE_TYPE == "Trains Point", ]$SCC <- "28500201"


#For facilities
for(year in c(2011, 2013)){
  
  columns <- "ccccccccccccccccccccccccccccccccc"
  
  stack_SCCs <- read_csv(paste0("M:\\Emissions\\2011 Emissions\\Point Sources\\SCCs for each stack ", year, ".csv"), col_types = columns)
  
  stack_SCCs <- mutate(stack_SCCs, ID = paste(SOURCE_ID, PROCESS_ID, EMISSION_UNIT_ID, RELEASE_POINT_ID, SCC_CODE))
  
  a <- stack_SCCs[duplicated(stack_SCCs$ID), ]
  
  stack_SCCs <- stack_SCCs[!duplicated(stack_SCCs$ID), c("SOURCE_ID", "SCC_CODE", "EMISSION_UNIT_ID", "PROCESS_ID", "RELEASE_POINT_ID", "FLOW_PCT")]
  
  stack_SCCs <- mutate(ungroup(stack_SCCs), ID = paste(round(as.numeric(SOURCE_ID)), PROCESS_ID, EMISSION_UNIT_ID))
  
  #a <- stack_SCCs[duplicated(stack_SCCs$ID), ]
  
  #Filter emissions to only facility point sources
  point <- filter(Emis_All, SOURCE_TYPE == paste("Facility Point", year))
  
  point$RELEASE_POINT_ID <- NULL
  point$SCC_CODE <- NULL
  point$FLOW_PCT <- NULL
  
  point <- mutate(ungroup(point), ID = paste(gsub("_2013", "", SOURCE_ID), PROCESS_ID, EMISSION_UNIT_ID))
  
  point <- left_join(ungroup(point), stack_SCCs[ , c("ID", "SCC_CODE", "RELEASE_POINT_ID", "FLOW_PCT")], by = 'ID', all.y = F)
  
  point$SCC <- round(as.numeric(point$SCC_CODE))
  
  point$SCC_CODE <- NULL
  
  a <- filter(point, is.na(SCC))
  b <- filter(point, is.na(FLOW_PCT))
  c <- filter(point, is.na(EMISSION_AMT))
  
  #Check for Fug. Emissions > Emissions Total
  fuge_high <- filter(point, as.numeric(FUGITIVE_EMISSION_AMT) > as.numeric(EMISSION_AMT))
  c <- filter(point, FUGITIVE_EMISSION_AMT >= EMISSION_AMT, FUGITIVE_EMISSION_AMT > 0)
  
  #Split emissions by release point
  point <- mutate(point, EMISSION_AMT = EMISSION_AMT * as.numeric(FLOW_PCT) / 100)
  
  #Re-attach point sources
  Emis_All <- rbind(filter(Emis_All, SOURCE_TYPE != paste("Facility Point", year)), point)
  
  Emis_All <- mutate(ungroup(Emis_All), ID = paste(COUNTY_NAME, SOURCE_ID, EMISSION_UNIT_ID, RELEASE_POINT_ID, SCC, PROCESS_ID, Pollutant))
  
}

a <- filter(Emis_All, duplicated(ID)) 
a <- filter(a, SOURCE_TYPE == "Facility Point 2011")

#For Airports
airport_SCCs <- read_csv("M:\\Emissions\\2011 Emissions\\Airports\\SCCs for Airport processes.csv", col_types = "cccccccccccccccccccccccccccccc")

airport_SCCs <- mutate(airport_SCCs, ID = paste(SOURCE_ID, PROCESS_ID, EMISSION_UNIT_ID, SCC_CODE))

airport_SCCs <- airport_SCCs[!duplicated(airport_SCCs$ID), c("SOURCE_ID", "SCC_CODE" , "EMISSION_UNIT_ID", "PROCESS_ID", "SHORT_DESC")]

airport_SCCs <- mutate(airport_SCCs, ID = paste(SOURCE_ID, PROCESS_ID, EMISSION_UNIT_ID))

a <- airport_SCCs[duplicated(airport_SCCs$ID), ]


#Filter emissions to airports
planes <- filter(ungroup(Emis_All), SOURCE_TYPE == "Airport Point")

planes$SHORT_DESC <- NULL

planes <- mutate(planes, ID = paste(SOURCE_ID, PROCESS_ID, EMISSION_UNIT_ID))

planes <- left_join(planes, airport_SCCs[ , c("ID", "SCC_CODE", "SHORT_DESC")])

planes$SCC <- round(as.numeric(planes$SCC_CODE))

planes$SCC_CODE <- NULL


#Re-attach airport sources
Emis_All <- rbind(filter(Emis_All, SOURCE_TYPE != "Airport Point"), planes)

Emis_All <- mutate(ungroup(Emis_All), ID = paste(COUNTY_NAME, SOURCE_ID, EMISSION_UNIT_ID, RELEASE_POINT_ID, SCC, PROCESS_ID, Pollutant))

a <- filter(Emis_All, duplicated(ID)) 

#------------------------#
#For allocated Point
#------------------------#

#Filter emissions to allocateed point
ap <- filter(ungroup(Emis_All), SOURCE_TYPE == "Allocated Point")

ap <- mutate(ap, ID = paste(SOURCE_ID, PROCESS_ID, EMISSION_UNIT_ID))

# Loadd SCC codes
ap_SCCs <- read_csv("M:\\Emissions\\2011 Emissions\\Allocated Point\\SCCs for Allocatedpoint.csv", col_types = "cccccccccccccccccccccccccccccc")

ap_SCCs <- mutate(ap_SCCs, ID = paste(SOURCE_ID, PROCESS_ID, EMISSION_UNIT_ID, SCC_CODE))

ap_SCCs <- ap_SCCs[!duplicated(ap_SCCs$ID), c("SOURCE_ID", "SCC_CODE" , "EMISSION_UNIT_ID", "PROCESS_ID", "SHORT_DESC")]

ap_SCCs <- mutate(ap_SCCs, ID = paste(SOURCE_ID, PROCESS_ID, EMISSION_UNIT_ID))

a <- ap_SCCs[duplicated(ap_SCCs$ID), ]

# Join SCC codes
ap <- left_join(ap, ap_SCCs[ , c("ID", "SCC_CODE")])

ap$SCC <- round(as.numeric(ap$SCC_CODE))

ap$SCC_CODE <- NULL

# Load allocated point release points
ap_rps <- read.delim("M:/Emissions/2011 Emissions/Allocated point/allocatedpoint_process release points.xls", stringsAsFactors=F)

# Join release point IDs
ap$RELEASE_POINT_ID <- NULL
ap$FLOW_PCT <- NULL
ap <- left_join(ap, ap_rps[ , c("SOURCE_ID", "EMISSION_UNIT_ID", "RELEASE_POINT_ID", "FLOW_PCT")])

ap[ap$SOURCE_ID == '2716909002' & ap$EMISSION_UNIT_ID == 'EU001', ]$RELEASE_POINT_ID <- 'SI001'
ap[ap$SOURCE_ID == '2716909002' & ap$EMISSION_UNIT_ID == 'EU001', ]$FLOW_PCT <- 100

#Re-attach allocated point sources
Emis_All <- rbind(filter(Emis_All, SOURCE_TYPE != "Allocated Point"), ap)

Emis_All <- mutate(ungroup(Emis_All), ID = paste(COUNTY_NAME, SOURCE_ID, EMISSION_UNIT_ID, RELEASE_POINT_ID, SCC, PROCESS_ID, Pollutant))

a <- filter(Emis_All, duplicated(ID)) 


#========================================================#
##Remove rows without an SCC code or with zero emissions
#========================================================#
Emis_All <- filter(Emis_All, !is.na(SCC), !is.na(Pollutant), !is.na(EMISSION_AMT), EMISSION_AMT > 0)

#Identify missing SCCs
missing <- Emis_All[!Emis_All$SCC %in% unique(as.character(surr_SCCs$SCC.Codes)), ]
missing <- missing[!duplicated(missing$SCC), ]
unique(missing$SOURCE_TYPE)

#Identify EXTRA sccs
xtra <- surr_SCCs[!(as.character(surr_SCCs$SCC.Codes)) %in% unique(Emis_All$SCC), ]
xtra <- xtra[!duplicated(xtra$SCC), ]

#=============================#
##Remove unwanted COPCs and pollutant groups
#=============================#
#Load pollutant processing table
conn <- odbcConnectExcel2007("M:/MNRiskS 2011 development/Methods/Pollutant_Processing.xlsx") # open a connection to the Excel file

drop_COPCs <- data.frame(sqlFetch(conn, "Drop"))

close(conn)

drop_COPCs <- filter(drop_COPCs, !is.na(Pollutant))

Emis_All <- filter(Emis_All, !Pollutant %in% drop_COPCs$Pollutant)

#===============================
## Unit Conversion
#===============================
Emis_All$Emissions <- 
  ifelse(Emis_All$EMISSION_UNIT_CODE == "TON",  
         Emis_All$EMISSION_AMT * 2000, 
         ifelse(Emis_All$EMISSION_UNIT_CODE == "KG",
                Emis_All$EMISSION_AMT/0.453, 
                Emis_All$EMISSION_AMT))

Emis_All$EMISSION_AMT <- NULL

names(Emis_All)[names(Emis_All)== "EMISSION_UNIT_CODE"] <- "Units"

Emis_All$Units <- "lbs per Year"

Emis_All$Emissions <- as.numeric(Emis_All$Emissions)
#============================================================================================
## There is no PM25 or PM10 condensable. To elimiate duplication from FILterable and PRImary,
## take the maximum emission for each SCC-County group
#============================================================================================

#PM25
onlyPM25 <- filter(ungroup(Emis_All), Pollutant %in% c("PM25-FIL", "PM25-PRI", "PM-CON"))

onlyPM25 <- mutate(onlyPM25, ID = paste(COUNTY_NAME, SOURCE_ID, EMISSION_UNIT_ID, SCC, PROCESS_ID, RELEASE_POINT_ID))

onlyPM25 <- group_by(onlyPM25, ID) %>% 
  mutate(Emissions = ifelse(sum(Emissions[Pollutant == "PM25-PRI"], na.rm = T) > 0, 
                            Emissions[Pollutant == "PM25-PRI"],
                            sum(c(Emissions[Pollutant == "PM25-FIL"], Emissions[Pollutant == "PM-CON"]), na.rm = T)))

onlyPM25$Pollutant <- "PM2.5"

onlyPM25 <- unique(onlyPM25)

Emis_All <- rbind(filter(Emis_All, !Pollutant %in% c("PM25-FIL", "PM25-PRI")), onlyPM25)



#PM10
onlyPM10 <- filter(ungroup(Emis_All), Pollutant %in% c("PM10-FIL", "PM10-PRI", "PM-CON"))

onlyPM10 <- mutate(onlyPM10, ID = paste(COUNTY_NAME, SOURCE_ID, EMISSION_UNIT_ID, SCC, PROCESS_ID, RELEASE_POINT_ID))

onlyPM10 <- group_by(onlyPM10, ID) %>% 
  mutate(Emissions = ifelse(sum(Emissions[Pollutant == "PM10-PRI"], na.rm = T) > 0, 
                            Emissions[Pollutant == "PM10-PRI"],
                            sum(c(Emissions[Pollutant == "PM10-FIL"], Emissions[Pollutant == "PM-CON"]), na.rm = T)))

onlyPM10$Pollutant <- "PM10"

onlyPM10 <- unique(onlyPM10)

Emis_All <- rbind(filter(Emis_All, !Pollutant %in% c("PM10-FIL", "PM10-PRI")), onlyPM10)


#Check when PM10 = PM2.5
pm10pm25 <- left_join(onlyPM10, onlyPM25, by = "ID")

pm10pm25_2 <- right_join(onlyPM10, onlyPM25, by = "ID")

pm10pm25 <- unique(rbind(pm10pm25, pm10pm25_2))

#pm10pm25 <- merge(onlyPM10, onlyPM25, by = "ID")

a <- filter(pm10pm25, is.na(Emissions.y))

pm10pm25 <- mutate(pm10pm25, PM25_PM10_ratio = as.numeric(Emissions.y) / as.numeric(Emissions.x))
pm10pm25equal <- filter(pm10pm25, PM25_PM10_ratio >= 1)

#unique(pm10pm25equal$SHORT_DESC.x)
#unique(pm10pm25equal$SOURCE_TYPE.x)

#a <- filter(pm10pm25equal, SCC.x %in% justDiesel)

max(pm10pm25$PM25_PM10_ratio, na.rm = T)

filter(pm10pm25, PM25_PM10_ratio > 10)
#For sources with equal pm10 & pm2.5, adjust ratio to match 95 percentile
#of sources with matching SCC

#Get 90 %-ile for each SCC
x90_pm25 <- group_by(filter(pm10pm25, PM25_PM10_ratio < 1), SCC.x) %>% 
  summarize(minR = min(PM25_PM10_ratio, na.rm = T),
            medR = median(PM25_PM10_ratio, na.rm = T),
            maxR = max(PM25_PM10_ratio, na.rm = T),
            x90R = quantile(PM25_PM10_ratio, 0.90, na.rm = T))

x90_pm25 <- arrange(x90_pm25, -x90R)

pm10pm25_adj <- left_join(pm10pm25, x90_pm25[ , c("x90R", "SCC.x")])

pm10pm25_adj$x90R <- ifelse(is.na(pm10pm25_adj$x90R), 1, pm10pm25_adj$x90R)

pm10pm25_adj <- mutate(ungroup(pm10pm25_adj), "Emissions.x" = ifelse(as.numeric(PM25_PM10_ratio) >= 1 | is.na(PM25_PM10_ratio), as.numeric(Emissions.x) * as.numeric(x90R), as.numeric(Emissions.y)))

pm10pm25_adj <- mutate(ungroup(pm10pm25_adj), "FUGITIVE_EMISSION_AMT.x" = ifelse(as.numeric(PM25_PM10_ratio) >= 1 | is.na(PM25_PM10_ratio), as.numeric(FUGITIVE_EMISSION_AMT.x) * as.numeric(x90R), as.numeric(FUGITIVE_EMISSION_AMT.y)))

a <- filter(pm10pm25_adj, PM25_PM10_ratio >= 1)

pm10pm25_adj <- filter(pm10pm25_adj, !is.na(PM25_PM10_ratio))

pm10pm25_adj <- pm10pm25_adj[ , 1:15]

pm10pm25_adj$Pollutant.x <- "PM2.5"


names(pm10pm25_adj) <- names(Emis_All)


Emis_All <- rbind(filter(Emis_All, !Pollutant %in% c("PM2.5")), pm10pm25_adj[ , 1:15])


#PM
onlyPM <- filter(ungroup(Emis_All), Pollutant %in% c("PM-FIL", "PM-CON", "PM", "PM-PRI", "PM10"))

onlyPM <- mutate(onlyPM, ID = paste(COUNTY_NAME, SOURCE_ID, EMISSION_UNIT_ID, SCC, PROCESS_ID, RELEASE_POINT_ID))

onlyPM <- group_by(onlyPM, ID) %>% 
  mutate(Emissions = ifelse(sum(Emissions[Pollutant %in% c("PM-PRI", "PM")], na.rm = T) > 0, 
                            max(Emissions[Pollutant  %in% c("PM-PRI", "PM", "PM10")], na.rm = T),
                            max(c(Emissions[Pollutant == "PM10"], sum(c(Emissions[Pollutant == "PM-FIL"], Emissions[Pollutant == "PM-CON"]), na.rm = T)), na.rm = T)))

onlyPM$Pollutant <- "PM"

onlyPM <- unique(onlyPM)

onlyPM$PM_CON <- NULL

Emis_All <- rbind(filter(Emis_All, !Pollutant %in% c("PM-FIL", "PM-CON", "PM", "PM-PRI")), onlyPM)


#==========================================================================================
##Combine pollutant groups that have a single health value
#Dichlorobenzes, Cresols, Xylenes...
#==========================================================================================
#Load pollutant processing table
conn <- odbcConnectExcel2007("M:/MNRiskS 2011 development/Methods/Pollutant_Processing.xlsx") # open a connection to the Excel file

group_COPCs <- data.frame(sqlFetch(conn, "Combine", stringsAsFactors = F))

names(group_COPCs) <- gsub("[.]", "_", names(group_COPCs))

close(conn)

grouped <- inner_join(ungroup(Emis_All), group_COPCs[ , c(1,3:4)])

grouped <- mutate(grouped, ID = paste(COUNTY_NAME, SOURCE_ID, EMISSION_UNIT_ID, RELEASE_POINT_ID, SCC, PROCESS_ID))

grouped <- group_by(grouped, ID, Group) %>% 
  mutate(Emissions = sum(Emissions * Conversion_Factor, na.rm = T), 
         FUGITIVE_EMISSION_AMT = sum(as.numeric(FUGITIVE_EMISSION_AMT) * Conversion_Factor, na.rm = T))

grouped$Pollutant <- grouped$Group

grouped$Group <- NULL
grouped$Conversion_Factor <- NULL

grouped <- unique(grouped)
#52509 rows

grouped <- mutate(grouped, ID = paste(COUNTY_NAME, SOURCE_ID, EMISSION_UNIT_ID, RELEASE_POINT_ID, SCC, PROCESS_ID, Pollutant))

a <- filter(grouped, duplicated(ID))

a <- filter(grouped, ID == a$ID[1])

Emis_All <- rbind(Emis_All[!Emis_All$Pollutant %in% group_COPCs$Pollutant, ], grouped[ , 1:15])



#========================================================#
# Speciate chromium
#========================================================#
chrom_sccs <- read_csv("M:\\MNRiskS 2011 development\\COPC\\Chromium speciation\\chromium speciation via Regulatory_ MACT_ SCC.csv", col_types = "cccccccccccc")

names(chrom_sccs)[c(10,12)] <- c("Hex_Frx", "Tri_Frx")

chrom_sccs <- filter(chrom_sccs, SCC != "")

dup_SCC <- unique(filter(chrom_sccs, duplicated(SCC))$SCC)

a <- filter(chrom_sccs, SCC %in% dup_SCC)

chrom_sccs <- chrom_sccs[!duplicated(chrom_sccs$SCC), ]

chrom_facs <- unique(filter(Emis_All, Pollutant == "CHROMIUM")$SOURCE_ID)

chrom_facs <- filter(Emis_All, grepl("CHROM", Pollutant), SOURCE_ID %in% chrom_facs)

chrom_facs <- filter(chrom_facs, SCC %in% str_trim(chrom_sccs$SCC))


#Check if facilty emissions contain unspeciated chromium and speciated chrom III & chrom VI
chrom_redund <- group_by(chrom_facs, ID) %>% mutate(count = n())

a <- unique(filter(chrom_redund, count == 1))

chrom_redund3 <- filter(chrom_redund, count > 2)
##########

#Find 95% chromIII/chromVI ratio
Emis_All <- mutate(Emis_All, ID = paste(COUNTY_NAME, SOURCE_ID, EMISSION_UNIT_ID, RELEASE_POINT_ID, SCC, PROCESS_ID))

chrom_III <- filter(Emis_All, grepl("CHROMIUM III", Pollutant))

chrom_VI <- filter(Emis_All, grepl("CHROMIUM VI", Pollutant))

chrom_ratios <- left_join(chrom_III, chrom_VI, by='ID')

chrom_ratios <- mutate(chrom_ratios, chrom_sum = Emissions.y + Emissions.x, chromIII_ratio = (Emissions.x / chrom_sum))

#Get 90 %-ile for each SCC
x90_chrom <- group_by(filter(chrom_ratios, chromIII_ratio < 1), SCC.x) %>% 
  summarize(minR = min(chromIII_ratio, na.rm = T),
            medR = median(chromIII_ratio, na.rm = T),
            maxR = max(chromIII_ratio, na.rm = T),
            x90_ChromIII_ratio = quantile(chromIII_ratio, 0.9, na.rm = T),
            SCC = SCC.x[1])

x90_chrom <- arrange(x90_chrom, -x90_ChromIII_ratio)



chrom_only <- Emis_All[(Emis_All$Pollutant == "CHROMIUM"), ]

chrom_only <- left_join(chrom_only, chrom_sccs[ , c(5,10,12)])

chrom_only <- left_join(chrom_only, x90_chrom[ , c(5,6)])

chrom_only <- mutate(chrom_only, 
                     Hex_Frx = ifelse(is.na(x90_ChromIII_ratio), Hex_Frx, (1 - x90_ChromIII_ratio)),
                     Tri_Frx = ifelse(is.na(x90_ChromIII_ratio), Tri_Frx, x90_ChromIII_ratio))

add_chromIII <- mutate(chrom_only, Pollutant = "CHROMIUM III", Emissions = Emissions * as.numeric(Tri_Frx))

add_chromVI <- mutate(chrom_only, Pollutant = "CHROMIUM VI", Emissions = Emissions * as.numeric(Hex_Frx))


Emis_All <- rbind(filter(Emis_All, !Pollutant %in% c("CHROMIUM")), add_chromIII[ , 1:15])

Emis_All <- rbind(Emis_All, add_chromVI[ , 1:15])

#========================================================#
# Add Diesel PM2.5 if source has a Diesel based SCC code
#
# First checked to see if I had all possible diesel related SCCs from the Tier 3 Descriptions
# After checking below, I will need to include these SCCs: 2285000000, 2505020092, 2270002066, 2270005000, 2270004076 as well as those from the Tier 3 Descriptions
# checkdieseltab <- filter(dieselSCCtab, Tier.3.Description==c("Distallate", "Distillate Oil", "Diesel", "Hddv", "Lddv", "Lddt", "Residual Oil", "Light Commercial", "Residual", "Process", "Other", "Motorcycles", "Lddv", "Lddt", "Hddv", "Recreational", "Construction", "Industrial", "Lawn & Garden", "Farm", "Light Commercial", "Logging", "Airport Service", "Diesel", "Residual Oil", "Recreational Marine Vessels", "Railway Maintenance", "Other Asphalt", "Cutback Asphalt", "Commercial/Institutional", "Residential", "Transfer", "Basic Oxygen Furnace", "Combined", "Fluid Catalytic Cracking Units", "Petroleum Refinery Fugitives", "Miscellaneous", "Efr / Seal Crude", "Ifr / Seal Gasoline", "Ifr / Seal Crude"))
#========================================================#

#======================#
## Get diesel SCC codes
#======================#
##Connect to the ACCESS database
diesel_conn <- odbcConnectAccess2007("M:/DLK files/EPA_SCC.mdb")
##Pull the SCC table
dieselSCCs <- sqlFetch(diesel_conn, "Source Classification Code")
odbcCloseAll()

##Format and filter to diesel only
names(dieselSCCs)[1] <- "SCC"
names(dieselSCCs)[c(3,17,18)] <- c("Short.Name", "Tier2", "FUEL")

dieselSCCs$SCC  <- as.character(dieselSCCs$SCC)
dieselSCCs$FUEL <- as.character(dieselSCCs$FUEL)

justdieselFUEL <- filter(dieselSCCs, grepl("Diesel", Short.Name) | grepl("diesel", Short.Name) | grepl("Crude", Short.Name) | FUEL %in% c("Distallate", "Distillate", "Distillate Oil", "Diesel", "Hddv", "Lddv", "Lddt", "Residual Oil", "Light Commercial", "Residual", "Oil") | Tier2 == "Diesels" | Tier2 == "Oil" | (grepl("Crude", FUEL) & !grepl("Gasoline", Short.Name)), Tier2 != "Non-Road Gasoline")
justdieselSCC  <- filter(dieselSCCs, SCC %in% c("2285000000", "2505020092", "2270002066", "2270005000", "2270004076"))

justDiesel <- unique(c(justdieselFUEL$SCC, justdieselSCC$SCC, filter(surr_SCCs, as.logical(as.numeric(Diesel.Source)))$SCC))

#Filter to PM2.5 for diesel sources
np_Diesel <- Emis_All[(Emis_All$Pollutant == "PM2.5") & (Emis_All$SCC %in% justDiesel), ]

np_Diesel$Pollutant <- "PM2.5 DIESEL"

#Relabel & Multiply by fraction of PM2.5 as Diesel
np_Diesel <- left_join(np_Diesel, surr_SCCs[ , c("SCC.Codes", "Diesel.PM.Frx")], by = c("SCC" = "SCC.Codes"))

np_Diesel$"Diesel.PM.Frx" <- ifelse(is.na(np_Diesel$"Diesel.PM.Frx"), 1, np_Diesel$"Diesel.PM.Frx")

np_Diesel <- mutate(np_Diesel, Emissions = Emissions * as.numeric(Diesel.PM.Frx))

#Subtract 'Diesel PM2.5' from source's total PM2.5 (Not doing it this way.)
#np_PM25_Diesel <- cbind(np_PM25, np_Diesel$Emissions)
#np_PM25_Diesel$Emissions <- np_PM25_Diesel$Emissions - np_PM25_Diesel[ ,14]

#Join
#Emis_All <- Emis_All[!((Emis_All$Pollutant=="PM2.5") & (Emis_All$SCC %in% justDiesel)), ]
Emis_All <- rbind(Emis_All, np_Diesel[ , 1:15])

#Remove zero and NA emissions
a <- filter(Emis_All, Emissions <= 0)
a <- filter(Emis_All, is.na(Emissions))

Emis_All <- filter(Emis_All, Emissions > 0, !is.na(Emissions))

#=========================================================#
# NOx to NO2 Conversion:
# Kristie's decision - use Tier 2 assumptions for 
# non-point sources, which is a default of 75%
##=======================================================#
no2only <- filter(Emis_All, Pollutant %in% "NOX")

no2only <- left_join(no2only, surr_SCCs[ , c("SCC.Codes", "NO2.Frx")], by = c("SCC" = "SCC.Codes"))

no2only$"NO2.Frx" <- ifelse(is.na(no2only$"NO2.Frx"), 0.75, no2only$"NO2.Frx")

no2only <- mutate(no2only, Emissions = Emissions * NO2.Frx)

no2only$Pollutant <- "NO2"

Emis_All <- rbind(Emis_All, no2only[ , 1:15])


##===================================================
###--  Calculate High Traffic Fraction  --##
##===================================================
Emis_All <- left_join(Emis_All, surr_SCCs[ , c("SCC.Codes", "Source.Category", "Category.SCC", "MNRiskS.category.name")], by = c("SCC" = "SCC.Codes"), all.y = F)

onRoad <- filter(Emis_All, Source.Category == "On-Road")

#Load and Combine off-network fractions
setwd("M:\\Emissions\\2011 Emissions\\Non point\\onroad sources\\High Traffic\\")

offNet <- read_csv("all_MOVES_off_NET_frxs.csv")

names(offNet)[c(1, 6)] <- c("Pollutant", "FIPS")

offNet$Hennepin <- (offNet$FIPS == 27053)

offNet <- mutate(offNet, uniqueID = paste(Pollutant, Hennepin, Category.SCC))

offNet$CAS <- str_trim(offNet$CAS)

#Subtract off-network emissions (start exhaust) from high-traffic
highTraff <- onRoad

unique(highTraff$Pollutant)[!unique(highTraff$Pollutant) %in% offNet$Pollutant]
unique(offNet$Pollutant)[!unique(offNet$Pollutant) %in% highTraff$Pollutant]

highTraff$Hennepin <- (highTraff$FIPS == 27053)

highTraff <- mutate(highTraff, uniqueID = paste(Pollutant, Hennepin, Category.SCC))

highTraff <- left_join(highTraff, offNet[offNet$uniqueID %in% highTraff$uniqueID, c("uniqueID", "Off_Network_Frx", "CAS")])

highTraff <- mutate(highTraff, Emissions = Emissions * (1 - Off_Network_Frx))


#Load county high traffic vkt fractions
setwd("M:\\MNRiskS 2011 development\\Surrogates\\Traffic\\Processed Results")
ht_frx <- read_csv("County_high_traffic_VKT_fractions.csv")

ht_frx$FIPS <- as.character(ht_frx$COUNTY_FIPS)

highTraff <- left_join(highTraff, ht_frx[ , -1])


#Split off high-traffic fraction
highTraff <- mutate(highTraff, Emissions = ifelse(Category.SCC == 'OR_GAS', Emissions * HighTraffic_Gas_Fraction,  Emissions * HighTraffic_Diesel_Fraction))

a <- filter(highTraff, is.na(Emissions))

highTraff <- filter(highTraff, Emissions > 0)


#Save high traffic
highTraff2 <- group_by(highTraff, COUNTY_NAME, FIPS, Units, Pollutant, CAS, Category.SCC, Source.Category) %>% summarise(Emissions = sum(Emissions, na.rm = T))

write_csv(highTraff2, "M:\\Emissions\\2011 Emissions\\2. Processed Emissions\\High Traffic Processed Emissions.csv")


#Load block group high traffic VKT fractions
#vktFrx <- read_csv("Non_high_traffic_VKT_fractions.csv")

#names(vktFrx)[1] <- "FIPS"

#vktFrx$FIPS <- as.character(vktFrx$FIPS)

#highTraff <- left_join(highTraff, vktFrx)

#highTraff <- mutate(highTraff, Emissions = Emissions * frx_County_VKT_high_traffic)


##===================================================================
##Adjust OnRoad emissions for high traffic
##===================================================================
onRoad <- mutate(onRoad, uniqueFID = paste(FIPS, Pollutant, SCC))

highTraff <- mutate(highTraff, uniqueFID = paste(FIPS, Pollutant, SCC))

onRoad <- left_join(onRoad, highTraff[ ,c("Emissions", "uniqueFID")], by = "uniqueFID")

onRoad <- group_by(onRoad, uniqueFID) %>% mutate(Emissions = Emissions.x - replace(Emissions.y, is.na(Emissions.y), 0))

onRoad$Emissions.y <- NULL
onRoad$Emissions.x <- NULL


##===================================================================
## Get CAS numbers
##===================================================================
cas <- read_csv("M:\\MNRiskS 2011 development\\COPC\\Synonyms\\CEDR_pollutants & CAS.csv") 

names(cas)[1] <- "CAS"
names(cas)[2] <- "Pollutant"

cas$Pollutant <- str_trim(cas$Pollutant)
cas$CAS <- str_trim(cas$CAS)

#Save onRoad
onRoad <- group_by(onRoad, COUNTY_NAME, FIPS, Units, Pollutant, Category.SCC, Source.Category) %>% summarise(Emissions = sum(Emissions, na.rm=T))
nrow(onRoad)

onRoad <- merge(onRoad, cas[ , c("CAS", "Pollutant")], all.x=T)
nrow(onRoad)



write_csv(onRoad, "M:\\Emissions\\2011 Emissions\\2. Processed Emissions\\onRoad Apportioned Emissions.csv")

#Attach CAS to remaining source types
Emis_All2 <- Emis_All

Emis_All <- Emis_All2

Emis_All <- filter(Emis_All, is.na(Source.Category) | Source.Category != "On-Road")

nrow(Emis_All)

Emis_All <- merge(Emis_All, cas[ , c("CAS", "Pollutant")], all.x = T)
nrow(Emis_All)

##===================================================================
## Check if CAS #'s are all present in MNRISK's COPC table
##===================================================================
sort(unique(Emis_All$Pollutant))

#pollutant_list <- read_csv("C:/Users/dkvale/Desktop/pollutant_list.csv")

conn <- odbcConnectExcel2007("M:\\MNRiskS 2011 development\\COPC\\2011 COPC table.xlsx")

pollutant_list <- data.frame(sqlFetch(conn, "2011 COPC Table_references", as.is = T))[1:514, ]
close(conn) # close the connection to the file

names(pollutant_list)[grep("CAS", names(pollutant_list))] <- "CAS"
names(pollutant_list)[1] <- 'COPC Name'

pollutant_list$CAS <- str_trim(pollutant_list$CAS)

cas <- pollutant_list[!is.na(pollutant_list$CAS), ]

unique(cas$CAS)

copc_bad <- unique(filter(Emis_All, !CAS %in% str_trim(cas$CAS))$Pollutant)
xtra_copc <- unique(cas[!str_trim(cas$CAS) %in% Emis_All$CAS, ]$'COPC Name')

copc_bad_ht <- unique(filter(highTraff, !CAS %in% str_trim(cas$CAS))$Pollutant)
xtra_copc_ht <- unique(cas[!str_trim(cas$CAS) %in% highTraff$CAS, ]$'COPC Name')

copc_bad_ms <- unique(filter(onRoad, !CAS %in% str_trim(cas$CAS))$Pollutant)
xtra_copc_ms <- unique(cas[!str_trim(cas$CAS) %in% onRoad$CAS, ]$'COPC Name')

a <- filter(Emis_All, is.na(CAS))
b <- filter(Emis_All, CAS == "")

##===================================================================
## SAVE POINT RESULTS
##===================================================================
setwd("M:/Emissions/2011 Emissions/2. Processed Emissions")

unique(Emis_All$Source.Category)

#Check for unassigned SCCs
a <- filter(Emis_All, is.na(Source.Category))
unique(a$SOURCE_TYPE)

#Combine similiar point groups
Emis_All$SOURCE_TYPE <- ifelse(grepl("Facility", Emis_All$SOURCE_TYPE), "Facility Point", Emis_All$SOURCE_TYPE)

Emis_All$SOURCE_TYPE <- ifelse(grepl("CMV", Emis_All$Source.Category) & Emis_All$SOURCE_TYPE != "Facility Point", "CMV Point", Emis_All$SOURCE_TYPE)

Emis_All$SOURCE_TYPE <- ifelse(Emis_All$Source.Category == "Rail" & Emis_All$SCC != "2401085000" & !Emis_All$SOURCE_TYPE %in% c("Mobile", "Facility Point", "Trains Point"), "Rail Line", Emis_All$SOURCE_TYPE)

#*******************

Emis_All$Pollutant <- ifelse(grepl("_2013", Emis_All$SOURCE_ID), 
                             paste0(Emis_All$Pollutant, "_2013_point"), 
                             Emis_All$Pollutant)

Emis_All$CAS <- ifelse(grepl("_2013", Emis_All$SOURCE_ID), 
                       paste0(Emis_All$CAS, "-2013"), 
                       Emis_All$CAS)

Emis_All$SOURCE_ID <- ifelse(grepl("_2013", Emis_All$SOURCE_ID), gsub("_2013", "", Emis_All$SOURCE_ID), Emis_All$SOURCE_ID)

unique(Emis_All$Pollutant)

#Assign Source.Category for point sources
point <- filter(Emis_All, SOURCE_TYPE %in% c("Rail Line", "Allocated Point", "Event Point", "Airport Point", "Trains Point", "CMV Point", "Facility Point"))

point$Source.Category <- ifelse(point$COUNTY_NAME == "PORTABLE SOURCES", "Portable Sources", point$SOURCE_TYPE) 

#Clean up columns
point$FUGITIVE_EMISSION_AMT <- NULL
point$SHORT_DESC <- NULL
point$SOURCE_TYPE <- NULL
point$Category.SCC <- NULL
point$MNRiskS.category.name <- NULL

copc_bad_pt <- unique(filter(point, !CAS %in% str_trim(cas$CAS))$Pollutant)
xtra_copc_pt <- unique(cas[!str_trim(cas$CAS) %in% point$CAS, ]$'COPC Name')


#Loop and save emissions for each point source group
for(group in unique(point$Source.Category)){
  data <- filter(point, Source.Category == group)
  
  if(!group %in% c("Portable Sources", "Facility Point", "Allocated Point")) data <- group_by(data, COUNTY_NAME, FIPS, SOURCE_ID, Pollutant, Units, CAS, Source.Category) %>% summarise(Emissions = sum(Emissions, na.rm = T))
  
  if(group == "Portable Sources") data <- group_by(data, COUNTY_NAME, FIPS, Pollutant, Units, CAS, Source.Category) %>% summarise(Emissions = sum(Emissions, na.rm = T))
  
  write.csv(data, file = paste0(group, " Processed Emissions.csv"), row.names = F)
}


##===================================================================
## SAVE NON-POINT RESULTS
##===================================================================
Emis_All <- filter(Emis_All, !SOURCE_TYPE %in% c("Rail Line", "Allocated Point", "Event Point", "Airport Point", "Trains Point", "CMV Point", "Facility Point"))

#Check for missing SCCs
a <- filter(Emis_All, is.na(Source.Category))
unique(a$SOURCE_TYPE)

unique(Emis_All$Source.Category)

#Aggregate non-point surrogate emissions to county level
Emis_All_SCCs <- Emis_All
#saveRDS(Emis_All_SCCs, "M:/MNRiskS 2011 Results/Stage-2 Gas station (x2)/Emis_All_SCCs.Rds")


# For CASSIE's non-point work
#write.csv(Emis_All_SCCs, file = "Non-point non-grouped 2011 Emissions for Cassie.csv", row.names = F)

Emis_All <- group_by(Emis_All, COUNTY_NAME, FIPS, Pollutant, CAS, Units, Source.Category, Category.SCC) %>% summarise(Emissions = sum(Emissions, na.rm = T))

#Loop and save emissions for each surrogate SCC group
for(group in unique(Emis_All$Source.Category)){
  data <- filter(Emis_All, Source.Category == group)
  write.csv(data, file = paste0(group, " Emissions for Apportioning.csv"), row.names = F)
}



##===================================================================
## QC PROCESSED RESULTS
##===================================================================

##Facilities
EI_point_tot <- read_csv("M:\\Emissions\\2011 Emissions\\Point Sources\\Chun-Yi - Emissions_grouped_ 7-30-15.csv")
names(EI_point_tot)[11:12] <- c("Pollutant", "Emissions")
EI_point_tot$SOURCE_ID <- as.character(EI_point_tot$SOURCE_ID)
EI_point_tot[EI_point_tot$Pollutant == "PM10-PRI", "Pollutant"] <- "PM10"
EI_point_tot[EI_point_tot$Pollutant == "PM-PRI", "Pollutant"] <- "PM"
EI_point_tot[EI_point_tot$Pollutant == "PM25-PRI", "Pollutant"] <- "PM2.5"
EI_point_tot[EI_point_tot$Pollutant == "DIOXN2378EQ-W/05", "Pollutant"] <- "DIOXN 2378EQ"

point_tot <- group_by(filter(point, !grepl("2013", Pollutant), grepl("Facility", Source.Category)), SOURCE_ID, Pollutant, CAS) %>% summarize(Emissions = sum(Emissions))

EI_point_tot <- group_by(EI_point_tot, SOURCE_ID, Pollutant) %>% summarize(Emissions = sum(as.numeric(Emissions)))

compare_point <- left_join(point_tot, EI_point_tot, by = c("SOURCE_ID", "Pollutant"))

compare_point <- filter(compare_point, !Pollutant %in% c("VOCs,Unspeciated", "NO2", "PM2.5 DIESEL", "CRESOL MX IS", "ALDEHYDES"))

compare_point <- filter(compare_point, is.na(Emissions.y) | abs(Emissions.x - Emissions.y) > .01 * Emissions.y)

unique(compare_point$Pollutant)

compare_point <- arrange(compare_point, SOURCE_ID, Pollutant)

#Non-FACILITY

Emis_All <- Emis_All_bkup

Emis_All <- filter(Emis_All, !Pollutant %in% drop_COPCs$Pollutant)

#===============================
## Unit Conversion
#===============================
Emis_All$Emissions <- 
  ifelse(Emis_All$EMISSION_UNIT_CODE == "TON",  
         Emis_All$EMISSION_AMT * 2000, 
         ifelse(Emis_All$EMISSION_UNIT_CODE == "KG",
                Emis_All$EMISSION_AMT/0.453, 
                Emis_All$EMISSION_AMT))

Emis_All$EMISSION_AMT <- NULL


##OnRoad
a <- str_trim(filter(surr_SCCs, Source.Category == "On-Road")$SCC)

EI_onroad <- group_by(filter(Emis_All, SCC %in% a), COUNTY_NAME, Pollutant) %>% summarize(Emissions = sum(Emissions))

highTraff_tot <- group_by(highTraff, COUNTY_NAME, Pollutant, CAS) %>% summarize(Emissions = sum(Emissions))

onroad_tot <- group_by(onRoad, COUNTY_NAME, Pollutant, CAS) %>% summarize(Emissions = sum(Emissions))
onroad_tot <- group_by(rbind(onroad_tot, highTraff_tot), COUNTY_NAME, Pollutant) %>% summarize(Emissions = sum(Emissions))

compare_onroad <- left_join(onroad_tot, EI_onroad, by = c("COUNTY_NAME", "Pollutant"))

compare_onroad <- filter(compare_onroad, !Pollutant %in% c("VOCs,Unspeciated", "NO2", "PM2.5 DIESEL", "CRESOL MX IS", "ALDEHYDES"))

compare_onroad <- filter(compare_onroad, is.na(Emissions.y) | abs(Emissions.x - Emissions.y) > .01 * Emissions.y)

unique(compare_onroad$Pollutant)

compare_onroad <- arrange(compare_onroad, COUNTY_NAME, Pollutant)


##Airports
EI_onroad <- group_by(filter(Emis_All, SOURCE_TYPE == "Airport Point"), COUNTY_NAME, Pollutant) %>% summarize(Emissions = sum(Emissions))

onroad_tot <- group_by(filter(point, Source.Category == "Airport Point"), COUNTY_NAME, Pollutant, CAS) %>% summarize(Emissions = sum(Emissions))

compare_onroad <- left_join(onroad_tot, EI_onroad, by = c("COUNTY_NAME", "Pollutant"))

compare_onroad <- filter(compare_onroad, !Pollutant %in% c("VOCs,Unspeciated", "NO2", "PM2.5 DIESEL", "CRESOL MX IS", "ALDEHYDES"))

compare_onroad <- filter(compare_onroad, is.na(Emissions.y) | abs(Emissions.x - Emissions.y) > .01 * Emissions.y)

unique(compare_onroad$Pollutant)

compare_onroad <- arrange(compare_onroad, COUNTY_NAME, Pollutant)


##AIRPORTS NON-POINT
a <- str_trim(filter(surr_SCCs, Source.Category == "Airports")$SCC)

EI_onroad <- group_by(filter(Emis_All, SCC %in% a), COUNTY_NAME, Pollutant) %>% summarize(Emissions = sum(Emissions))


airp <- read_csv("M:\\Emissions\\2011 Emissions\\2. Processed Emissions\\Airports Emissions for Apportioning.csv")

onroad_tot <- group_by(airp, COUNTY_NAME, Pollutant, CAS) %>% summarize(Emissions = sum(Emissions))

compare_onroad <- left_join(onroad_tot, EI_onroad, by = c("COUNTY_NAME", "Pollutant"))

compare_onroad <- filter(compare_onroad, !Pollutant %in% c("VOCs,Unspeciated", "NO2", "PM2.5 DIESEL", "CRESOL MX IS", "ALDEHYDES"))

compare_onroad <- filter(compare_onroad, is.na(Emissions.y) | abs(Emissions.x - Emissions.y) > .01 * Emissions.y)

unique(compare_onroad$Pollutant)

compare_onroad <- arrange(compare_onroad, COUNTY_NAME, Pollutant)


##Portables
EI_onroad <- group_by(filter(Emis_All, COUNTY_NAME == "PORTABLE SOURCES"), COUNTY_NAME, Pollutant) %>% summarize(Emissions = sum(Emissions))

onroad_tot <- group_by(filter(point, Source.Category == "Portable Sources"), COUNTY_NAME, Pollutant, CAS) %>% summarize(Emissions = sum(Emissions))

compare_onroad <- left_join(onroad_tot, EI_onroad, by = c("COUNTY_NAME", "Pollutant"))

compare_onroad <- filter(compare_onroad, !Pollutant %in% c("VOCs,Unspeciated", "NO2", "PM2.5 DIESEL", "CRESOL MX IS", "ALDEHYDES"))

compare_onroad <- filter(compare_onroad, is.na(Emissions.y) | abs(Emissions.x - Emissions.y) > .01 * Emissions.y)

unique(compare_onroad$Pollutant)

compare_onroad <- arrange(compare_onroad, COUNTY_NAME, Pollutant)

