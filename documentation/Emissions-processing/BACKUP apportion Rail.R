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

setwd("M:/MNRiskS 2011 development/Rail")
conn = odbcConnectExcel2007("Railway by block group intersect.xlsx") ##Connect to the Excel File
railways = sqlFetch(conn, "Railway by block group intersec") # read a table
close(conn) # close the connection to the file

##==============================================================##
##Calculate block gruop fractions within counties 
##by summing within block gruops and dividing by total county sums
##==============================================================##
                          
#First, sum segments within block groups
railsegments <- railways[, c("STATEFP10", "COUNTYFP10", "GEOID10", "Segment_length_m")]

blkgrpsegments <- group_by(railsegments, STATEFP10, COUNTYFP10, GEOID10) %>% summarize(blkgrpsegsums=sum(Segment_length_m))

countysegments <- group_by(railsegments, STATEFP10, COUNTYFP10) %>% summarize(countysegsums=sum(Segment_length_m))

railwaysegments <- merge(blkgrpsegments, countysegments, by=c("STATEFP10", "COUNTYFP10"))

railwaysegments <- mutate(railwaysegments, frxrailways=blkgrpsegsums/countysegsums)

checkforsums <- group_by(railwaysegments, COUNTYFP10) %>% summarize(sumfrxbycounty=sum(frxrailways))
summary(checkforsums)

setwd("M:/Emissions/2011 Emissions/3. Apportioned Emissions/Apportioning Fractions")
write.csv(railwaysegments, "Railway Fractions for Counties.csv", row.names = F)
