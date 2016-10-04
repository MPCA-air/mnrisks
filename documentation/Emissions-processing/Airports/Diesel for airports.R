##Load Packages
library (dplyr)
library(stringr)
library(RODBC)
library(reshape2)
library(readr)


#======================
##Get diesel SCC codes
#======================
setwd("M:/DLK files")
##Give R the name of the database file
db="M:/DLK files/EPA_SCC.mdb"
##Connect to the database
dieselSCC=odbcConnectAccess2007(db)
##Pull the desired table from the database
dieselSCCtab=sqlFetch(dieselSCC, "Source Classification Code")
##Tell R new table is a dataframe
dieselSCCtab=data.frame(dieselSCCtab)
##Close connection with database
odbcClose(dieselSCC)

#==================================
##Get Point Source Emissions Table
#==================================
setwd("M:/Emissions/2011 Emissions/Point Sources/2011_point")
##Give R the name of the database file
dbpt="M:/Emissions/2011 Emissions/Point Sources/2011_point/2011_Point - Copy.accdb"
##Connect to the database
points=odbcConnectAccess2007(dbpt)
##Pull the desired table from the database
pointstab=sqlFetch(points, "Processed emissions")
##Tell R new table is a dataframe
pointstab=data.frame(pointstab)
##Close connection with database
odbcClose(points)

#=========================================================
##Convert PM2.5 to Diesel if its a Diesel based SCC code
#=========================================================
#First checked to see if I had all possible diesel related SCCs from the Tier 3 Descriptions
#After checking below, I will need to include these SCCs: 2285000000, 2505020092, 2270002066, 2270005000, 2270004076 as well as those from the Tier 3 Descriptions
#checkdieseltab <- filter(dieselSCCtab, Tier.3.Description==c("Distallate", "Distillate Oil", "Diesel", "Hddv", "Lddv", "Lddt", "Residual Oil", "Light Commercial", "Residual", "Process", "Other", "Motorcycles", "Lddv", "Lddt", "Hddv", "Recreational", "Construction", "Industrial", "Lawn & Garden", "Farm", "Light Commercial", "Logging", "Airport Service", "Diesel", "Residual Oil", "Recreational Marine Vessels", "Railway Maintenance", "Other Asphalt", "Cutback Asphalt", "Commercial/Institutional", "Residential", "Transfer", "Basic Oxygen Furnace", "Combined", "Fluid Catalytic Cracking Units", "Petroleum Refinery Fugitives", "Miscellaneous", "Efr / Seal Crude", "Ifr / Seal Gasoline", "Ifr / Seal Crude"))
#As well as the "Distillate Oil (Diesel) from the SCC.Level.Three field of the SCC table.
#========================================================
setwd("M:/Emissions/2011 Emissions/Point Sources")
#write.csv(checkdieseltab, file="Check Diesel SCCs.csv") #Monika, use this to check that I got evertyhing by systematically filtering the categories above, and only selecting "Points" in the Categories.
names(dieselSCCtab)[names(dieselSCCtab)== "Source.Classification.Code"] <- "SCC"
names(dieselSCCtab)[names(dieselSCCtab)== "Tier.3.Description"] <- "FUEL"
names(dieselSCCtab)[names(dieselSCCtab)== "SCC.Level.Three"] <- "SCCThree"
dieselSCCtab$SCC <- as.character(dieselSCCtab$SCC)
dieselSCCtab$FUEL <- as.character(dieselSCCtab$FUEL)
dieselSCCtab$SCCThree <- as.character(dieselSCCtab$SCCThree)
pointdiesel <- merge(dieselSCCtab, pointstab, by.x="SCC", by.y="SCC_CODE")


justdieselFUEL <- filter(pointdiesel, FUEL %in% c("Distallate", "Distillate Oil", "Diesel", "Hddv", "Lddv", "Lddt", "Residual Oil", "Light Commercial", "Residual"))
justdieselSCC <- filter(pointdiesel, SCC %in% c("2285000000", "2505020092", "2270002066", "2270005000", "2270004076"))
justdieselSCCThree <- filter(pointdiesel, SCCThree %in% c("Distillate Oil (Diesel)"))
justdiesel <- rbind(justdieselFUEL, justdieselSCC, justdieselSCCThree)
justdiesel <- unique(justdiesel)
justdieselPM25 <- filter(justdiesel, Group.Name %in% "PM2.5,PRIMRY")
justdieselPM25$Group.Name <- str_replace_all(justdieselPM25$Group.Name, "PM2.5,PRIMRY", "dieselPM25")
write.csv(justdieselPM25, file="Diesel PM2.5 Point Sources.csv")



