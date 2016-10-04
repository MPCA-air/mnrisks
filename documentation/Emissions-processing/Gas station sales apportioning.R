
#-- Load packages
library(readxl)
library(scales)
library(dplyr)
library(stringr)

#-- Load gas station sales data
sales <- read_excel("M:/Emissions/2011 Emissions/Point Sources/Gasstations/Gas stations.xlsx", "Final Gas stations")

names(sales)[c(1,5,14)] <- c("Name", "Zip", "Sales")

sales$Zip <- as.numeric(sales$Zip)
  
#-- Remove county totals
sales <- filter(sales, !is.na(City))

#-- Create unique ID
sales <- mutate(sales ,  ID = paste(Zip, Address, City, Name))
sales  <- sales[!duplicated(sales $ID), -c(21:27)]

#-- Load station coordinates
coords <- read_excel("M:\\Emissions\\2011 Emissions\\Point Sources\\Gasstations\\Gas Stations Reference USA list with coordinates from Google Maps_gcp.xlsx",
                     "final edited")

coords <- filter(coords, is.na(dup) | str_trim(dup) != "remove")

coords <- coords[ , -c(4:5,12,13)]

names(coords)[4] <- "Name"


#-- Create unique ID
coords <- mutate(coords,  ID_location = paste(Zip, Address, City))
coords <- mutate(coords,  ID = paste(Zip, Address, City, Name))


#-- Test for Greg's corrections
original <- read_excel("M:\\Emissions\\2011 Emissions\\Point Sources\\Gasstations\\Gas Stations Reference USA list with coordinates from Google Maps_gcp.xlsx")
original <- original[ , -c(4:5,12,13)]
original <- filter(original, is.na(dup) | str_trim(dup) != "remove")
names(original)[c(1,2,4)] <- c("LATITUDE_og","LONGITUDE_og","Name")

#-- Create ID and remove duplicate (Richfield 66)
original <- mutate(original,  ID = paste(Zip, Address, City, Name))
original <- original[!duplicated(original$ID), ]

#-- Join tables and search for coordinate differences
coords <- left_join(coords, original[ , c("ID","LATITUDE_og","LONGITUDE_og")], by = "ID")

#-- Mark as "fixed" if different
coords <- mutate(coords, dup = ifelse(is.na(dup) & (LATITUDE != LATITUDE_og | LONGITUDE != LONGITUDE_og), "fixed", dup))

#-- Remove added columns
coords <- coords[ , 1:13]
rm(original)


#-- Check for duplicates
dups <- coords[duplicated(coords$ID_location), ]
dups <- filter(coords, ID_location %in% dups$ID_location)

dups_sales <- filter(sales, ID %in% dups$ID)

coords <- coords[!duplicated(coords$ID), ]


#-- Replace coordinates with RMAD's tank data
tanks <- read.csv("M:\\Emissions\\2011 Emissions\\Point Sources\\Gasstations\\Service_Stations_Registered_Tanks_CASEY.csv",
                  stringsAsFactors = F)

tanks <- mutate(tanks,  ID_location = paste(zipcode, address, City))

coords <- left_join(coords, tanks[ , c("ID_location","latitude","longitude")], by = "ID_location")

#-- QA coords
coords[coords$ID == "56763 102 Lake St NE Warroad Riverside Bait & Tackle" , "dup"] <- "fixed"
coords[coords$ID == "56284 118 Dupont Ave Renville Cenex" , 1:3] <- list(44.792418, -95.213634, "fixed")
coords[coords$ID == "55302 3210 County Road 3 NW Annandale Tesoro" , 1:3] <- list(45.199706, -94.184864, "fixed")
coords[coords$ID == "55018 39033 70th Ave Dennison" , 1:3] <- list(44.406717, -93.040700, "fixed")


#-- Replace coordinates that weren't "fixed"
coords[!is.na(coords$latitude) && is.na(coords$dup), 1:2] <- coords[!is.na(coords$latitude) && is.na(coords$dup), c("latitude","longitude")] 

#-- Attach sales data
coords <- left_join(coords, sales[ , c("ID", "Sales")], by = c("ID"))

#-- Add stations with zero sales
#coords <- rbind(coords, filter(sales, Sales == 0)[ , names(coords)[-c(1:2,9:11)]])

#-- Select highest sales from duplicates, delete others
#coords <- group_by(coords, ID_location) %>% arrange(-Sales) %>% filter(row_number() == 1) 

#-- SAVE COORDINATES
coords <- coords[ , -c(3,14:15)]
names(coords)[12] <- "ID_name"
coords$ID <- 1:nrow(coords)
coords <- coords[ , c(14, 1:13)]

write.csv(coords, "M:\\MNRiskS 2011 development\\Send to Lakes\\Source Params\\Gas Station coordinates v3.csv", row.names = F)


##-- SALES APPORTIONING
#-- Cap sales at the 95 percentile :: 10 Million
quantile(sales$Sales, .95)

sales_cap <- 9E6

coords$Sales <- ifelse(coords$Sales > sales_cap, sales_cap, coords$Sales)

sales <- coords

#-- Scaled sales calculations
sales$Sales <- ifelse(is.na(sales$Sales), 0, sales$Sales)

sales$log_sales <- log(sales$Sales + 50000)

sales$sqrt_sales <- (sales$Sales + 50000)**.5

sales$Scaled_log_sales <- rescale(sales$log_sales, to = c(1, 10))

sales$Scaled_sqrt_sales <- rescale(sales$sqrt_sales, to = c(1, 10))


#-- County fractions
sales <- group_by(sales, County) %>% 
  mutate(county_sales_sqrt = sum(sqrt_sales),
         county_sales_fraction_sqrt = sqrt_sales / county_sales_sqrt)

sales <- group_by(sales, County) %>% 
         mutate(county_sales_scaled = sum(Scaled_sqrt_sales),
                county_sales_fraction_scaled = Scaled_sqrt_sales / county_sales_scaled)

sales <- group_by(sales, County) %>% 
         mutate(county_sales = sum(Sales),
                county_sales_fraction = Sales / county_sales)


#-- QA check the increasing fractions
qa <- filter(sales, county_sales_fraction_scaled  > county_sales_fraction)


#-- SAVE SALES FRACTIONS
sales <- sales[ , -c(15:21, 23:24)]
names(sales)[15] <- "Pct_County_Sales"

write.csv(sales, "M:\\Emissions\\2011 Emissions\\3. Apportioned Emissions\\Apportioning Fractions\\Gas Station Sales v3.csv",
          row.names = F)

