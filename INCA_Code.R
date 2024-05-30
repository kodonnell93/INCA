###### INCA SUNS
# We aim to take a closer look at the methods typically used to account for human 
# benefits of NBFs. In the most simplistic terms, a buffer is taken around the 
# project and the number of people found within that buffer are benefited.
# However, different sizes of buffers, different methods of setting the buffer,
# and different metrics of counting humans benefited have been used to define 
# social benefit from NBF.

# 3 different buffers (300m, 500m, and 1,000m) were produced for each NBS project
# from the TNC SUNS portfolio. The intersection between each of these buffers and
# 2 feet of SLR and the intersection between each of these buffers and FEMA's 
# special flood hazard areas (SFHA) were created. This results in 9 areas of 
# potential human benefit. The sum of people in each of these areas was taken 
# using 1) night time landscan imagery (30m resolution), 2) census block 
# populations intersected with the 9 areas, 3) total buildings within the 
# buffers using the parcel tax database, and 4) the total number of residential 
# serving parcels. Residential serving parcels included, residential parcels,
# public parks, schools, etc.

#Library's used for this project:
library(ggplot2)
library(tidyr)
library(tidyverse)
library(RColorBrewer)
library(dplyr)
library(ggpubr)
library(readxl)
library(purrr)
library(dgof)

# First we read in a csv that has estimates of the full SUNS area, not split by project. 
Diss_INCA <- read.csv("FullAreaEstimates.csv") #dissolved inca area
County_INCA <- subset(Diss_INCA, Diss_INCA$Start == "County") #subsetting the people estimate counts for the 3 count reagion
SUNS_Diss <- subset(Diss_INCA, Diss_INCA$Start != "County") #subsetting the people estimate counts for the dissolved inca area

# Then we read in the two excel files that have people estimates for each individual project (overlapping buffers are accounted for and each project has it's own full buffer)
path_footprint <- print("Footprint_all_TableToExcel.xlsx") # path to the people estimates for the buffers around a SUNS project footprint
path_point <- print("Point_all_TableToExcel.xlsx") # path to the people estimates for the buffers around a SUNS project centroid point. Some projects were only given as points. 
#The raw excel files will not be included in any public database to protect the location of some proposed projects. 
#If excel files are not included, skipp to line 129 to read in the full INCA database with identifiable information removed. 

#Read in the footprint excel
ls_footprint <- path_footprint %>%
  excel_sheets() %>%
  set_names() %>%
  map(read_excel, path = path_footprint)

#Read in the point excel
ls_point <- path_point %>%
  excel_sheets() %>%
  set_names() %>%
  map(read_excel, path = path_point)


## selecting the data we need from each dataframe in the lists created above (ls_footprint & ls_point)
# all the variables we want to pull out for analysis and plotting is found below
# If more variables are requested, check ls_footprint or ls_point for the variable names and add to the one_of() function 
df_footprint <- lapply(ls_footprint, function(x) 
  select(x, one_of("SUNSID", "Acres", "BUFF_DIST", 
                   "Landscan_pop",      # 1) landscan sum
                   "Census_pop20",      # 2) Census pop
                   "ParcelAll_bldgcnt", # 3) Building count
                   "ParcelRes_count",   # 4) Residential serving parcels
                   "ParcelAll_count"))) # number of parcels in area not used as an INCA method, but used to check the residential serving parcels numbers

df_point <- lapply(ls_point, function(x) 
  select(x, one_of("SUNSID", "BUFF_DIST", 
                   "Landscan_pop",      # 1) landscan sum
                   "Census_pop20",      # 2) Census pop
                   "ParcelAll_bldgcnt", # 3) Building count
                   "ParcelRes_count",   # 4) Residential serving parcels
                   "ParcelAll_count"))) 


#Remove Plan, Study, or linear parks from the SUNS data set of projects. We will only be testing the projects that are NBS. 
#the PS list was given by C.S. 
PS <- c("LYN04", "PAN08", "PAN18", "BAY02", "BAY10", "BAY05", "PAN20", "PAN06", "PAN02", "PSJ01", "GUL03") # list of SUNSID's that are a plan, study or linear park
dfProject_footprint <- lapply(df_footprint, function(x) subset(x, !(SUNSID %in% PS)))
dfProject_point <- lapply(df_point, function(x) subset(x, !(SUNSID %in% PS)))

#replace blank cells with 0 - cells with a blank means there was no "people" in that area for that method
dfProject_footprint <- lapply(dfProject_footprint, function(x) replace(x, is.na(x), 0))
dfProject_point <- lapply(dfProject_point, function(x) replace(x, is.na(x), 0))

# create two dataframes with the name of the list items as a column for both footprint and point 
# the list names are "[buffer][area]_SumWithin4"
INCA_rawF <- dfProject_footprint %>% bind_rows(.id = "CNames")
INCA_rawF$CNames <- gsub("_SumWithin4","",INCA_rawF$CNames) # creates a new column, associating the list name with each row

INCA_rawP <- dfProject_point %>% bind_rows(.id = "CNames")
INCA_rawP$CNames <- gsub("_SumWithin4","",INCA_rawP$CNames)

# create long format of INCA for Footprint and Point
INCA_longF <- pivot_longer(INCA_rawF, cols = c('Landscan_pop', 'Census_pop20', 'ParcelAll_bldgcnt', 'ParcelRes_count', 'ParcelAll_count'), names_to = "Method", values_to = "People", values_drop_na = TRUE)
INCA_longF$Start <- "Footprint"

INCA_longP <- pivot_longer(INCA_rawP, cols = c('Landscan_pop', 'Census_pop20', 'ParcelAll_bldgcnt', 'ParcelRes_count', 'ParcelAll_count'), names_to = "Method", values_to = "People", values_drop_na = TRUE)
INCA_longP$Acres <- NA # must have the same column names, acres for points is the size of the buffer
INCA_longP$Start <- "Point"

INCA <- rbind(INCA_longF, INCA_longP) # combines the points and footprints data into one df
#INCA <- INCA_longF # only the footprint data
#INCA <- INCA_longP # only the point data

# setting the categorical variables as factors and setting the factor levels. 
SUNS_Diss$Method <- factor(SUNS_Diss$Method, levels = c('Landscan', 'Census', 'BldgCnt', 'ResParcel', 'Parcel'),
                           labels = c("Landscan Sum", "Census Population", "Parcel Building Count", "Residential Parcels", "All Parcels"))

INCA$Method <- factor(INCA$Method, levels = c('Landscan_pop', 'Census_pop20', 'ParcelAll_bldgcnt', 'ParcelRes_count', 'ParcelAll_count'),
                      labels = c("Landscan Sum", "Census Population", "Parcel Building Count", "Residential Parcels", "All Parcels"))


SUNS_Diss$Buffer <- factor(SUNS_Diss$Buffer, levels = c("0.3", "0.5", "1"), 
                           labels = c("Buffer 0.3km", "Buffer 0.5km", "Buffer 1km"))

INCA$Buffer <- factor(INCA$BUFF_DIST, levels = c(300, 500, 1000), 
                      labels = c("Buffer 0.3km", "Buffer 0.5km", "Buffer 1km"))


INCA$Area <- ifelse(grepl("SLR2", INCA$CNames), "SLR2ft",
                    ifelse(grepl("SFHA", INCA$CNames), "SFHA",
                           ifelse(grepl("km", INCA$CNames), "Full", "TBD")))
INCA$Area <- factor(INCA$Area, levels = c("Full", "SFHA", "SLR2ft"),
                    labels = c("Full Buffer", "FEMA SFHA", "2ft. of SLR"))


#write.csv(INCA, file = "INCA_FULL.csv")

INCA <- read.csv("INCA_FULL.csv")

# we now have the dataframes of SUNS_Diss and INCA
# SUNS_Diss = the total quantity of people in the 3 areas (full buffer, Intersection with SFHA, and Intersection with 2ft. of SLR) for the 3 buffers (0.3km, 0.5km, and 1km). 
#   Each unique combination of methods is dissolved into one shape to count the total amount of people benefited by SUNS.
# INCA = each SUNS project has a quantity of people benefited for each method.
#   The buffers around each project is iterated over the summarize within function 
#   so overlapping buffers are counted separately and each project has the full 
#   quantity of people within the full buffer around the project.


