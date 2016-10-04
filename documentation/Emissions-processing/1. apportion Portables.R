##### Apportion processed emissions for


#Portables
#Apportioned by a combination of construction in the METRO area and medium and high development in the outstate area. Portables are reported as state totals.


##Load Packages
library(dplyr)
library(stringr)
library(RODBC)
library(reshape2)
library(readr)
library(Rcpp)
library(readxl)

##=========================================================##
##Open the final surrogates table for county information
##=========================================================##

setwd("M:/MNRiskS 2011 development/Surrogates")
conn = odbcConnectExcel2007("2011_surrogates.xlsx") ##Connect to the Excel File
surrogates = sqlFetch(conn, "census_2010_blockgroups_usboc") # read a table
close(conn) # close the connection to the file

##============================================##
##Open Construction Fraction Calculation Table
##============================================##
setwd("M:/MNRiskS 2011 development/Surrogates/Construction")
construction = read_excel("Construction Block Group Fractions.xls", sheet=1)
                          
#Calculate Total State Sums of Construction Events and unique county sums and create county fractions for construction events.
countyconstruction <- construction[, c("COUNTYFP10", "CountySums")]
countyconstruction <- unique(countyconstruction)
stateconstruction <- sum(countyconstruction$CountySums)
countyconstruction <- mutate(countyconstruction, frx_county_constr=CountySums/stateconstruction)

##=====================================##
##Open the land use surrogates table
##=====================================##

setwd("M:/MNRiskS 2011 development/Land Use")
conn = odbcConnectExcel2007("land use categories_2011.xlsx") ##Connect to the Excel File
landuse = sqlFetch(conn, "Data by BG") # read a table
close(conn) # close the connection to the file

#Calculate Total State Sums of Medium and High Density Development Areas in state and unique county sums and create county fractions.
countylanduse <- landuse[, c("county", "Developed, Medium Intensity", "Developed, High Intensity")]
colnames(countylanduse) <- c("county", "Medium", "High")
countylanduse <- group_by(countylanduse, county) %>% summarize(Medium=sum(Medium), High=sum(High))
statelanduseHI <- sum(countylanduse$High)
statelanduseMED <- sum(countylanduse$Medium)
countylanduse <- group_by(countylanduse, county) %>% mutate(frx_county_landuse_MEDHI=sum(Medium, High)/sum(statelanduseMED, statelanduseMED))

#Pull counties, construction, and medium and high density landuse together
countyinfo <- surrogates[, c("STATEFP10", "COUNTYFP10", "METRO", "COUNTY_FIPS")]
  
countyinfo <- unique(countyinfo)
frxportables <- cbind(countyinfo, countylanduse, countyconstruction)
METRO <- subset(frxportables, METRO=="METRO")
METRO <- group_by(METRO, county) %>% mutate(frxcountyportables=frx_county_constr)
OUTSTATE <- subset(frxportables, METRO=="OUTSTATE")
OUTSTATE <- group_by(OUTSTATE, county) %>% mutate(frxcountyportables=frx_county_landuse_MEDHI)
frxportables <- rbind(METRO, OUTSTATE)

setwd("M:/Emissions/2011 Emissions/3. Apportioned Emissions/Apportioning Fractions")
write_csv(frxportables, "Portables Fractions for Counties.csv")


##Examples for pulling in xls files: gas.station <- read.delim("M:/Emissions/2011 Emissions/Point Sources/Gasstations/gas station.xls", stringsAsFactors=FALSE)