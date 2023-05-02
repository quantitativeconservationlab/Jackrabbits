
# Background --------------------------------------------------------------


## This script was developed by Leticia Camacho to research/evaluate 
## the jackrabbit spotlight survey 2022 data and determine optimal 
## spotlight survey methodology. 

## This script is the first in a group of ~three optimization scripts to achieve
## the above goal. 



# Data Description ---------------------------------------------

##This script was developed to clean and visualize black-tailed jackrabbit 
##count data collected using spotlight surveys conducted at dusk (10pm-2am) 
#and dawn(2am-6am) in the Morley Nelson Birds of Prey NCA. 
#Spotlight surveys were conducted by two teams of two trained technicians each
#(1 lead and 1 undergrad. pairs) during 8 day period in June and August 2022. 
#There are 4 total sites surveyed in August2022. All 4 sites were surveyed each night
#either by the dawn or dusk crew (road/weather conditions permitting). 
#Survey crews coordinated to randomize the order,start time, and start locations
#of each night and site that was surveyed (planning was done before hand @ 
#beginning of season).



# Aug2022 Survey Details: -------------------------------------------------

#The first 2 nights were an exception to the above statement about the individual teams. 
#The first two night of surveys were done as a team of 4 - with all technician in the 
#same truck, visiting all four sites together to standardize training measures
#and increase confidence in Identification skills in the field.
#along each site specific route the technicians drove no faster that 10mph 
#while surveying each site route 
#incidental rabbit observations, while technicians were driving no faster than 
#35mph, were also recorded on the GPS
#




# June2022 Survey Details : -----------------------------------------------------

## We relied on tablets rather than GPS units to gather GPS points of rabbits 
## observed along the routes. This led to a lack of data for this survey period
## - specifically the exact time that the GPS point was taken during the surveys
##    




# Github Code Source: -----------------------------------------------------


#Using the quantitativeconservationlab / Jackrabbits GitHub Repository 
#to store scripts. URL:https://github.com/quantitativeconservationlab/Jackrabbits.git




# Setup -------------------------------------------------------------------

## Set up your work space and load relevant packages -----------

## Clean your work space to reset your R environment. 
rm( list = ls() )

## Load packages relevant to this script:  -------------

library( tidyverse ) #package for easy data manipulation
library(lubridate)





# Setting Working Directory -----------------------------------------------


# Set working directory. This is the path to your Rstudio folder for this 
# project. If you are in your correct Rstudio project then it should be:

getwd()#"C:/Users/leticiacamacho/Documents/BTJR_MSProject/Rcode/Spotlights_Hab_2022/Jackrabbits"

# if so then:
workdir <- getwd() 
#creating working directory as an object so you can call it easier 
#     - without having to type it out or re-run each time.







# Load and Create Data: ---------------------------------------------------

# Set data paths:  -----------
 
#Creating pathway to call the BTJR data from the common drive
datapath <- "Z:/Common/Jackrabbits/Data/Spotlights/" 




# Importing Data: ---------------------------------------------------------

## Importing Aug22 Survey Data:  -----------
A_SiteInfo<- read.csv(paste0(datapath, "Aug22/BigRabdf_extended.csv"))
A_Rab<-read.csv(paste0(datapath, "Aug22/BTJR.obs_AUG_Clean.csv"))

#Aug22 is already in the correct format that we need - from previous scripts
#Need to get June22 data to match these 


## Importing June22 Survey Data:  -----------
J_Site<-read.csv(paste0(datapath, "June22/SiteInfo.csv"))
J_Rab <- read.csv( file = paste0(datapath, "June22/Rab.Loc_Simplified.Edit.csv"),
                   strip.white = TRUE )

# Cleaning June22 Data:  --------------------------------------------------

# Creating df from site with columns of interest:  -----------
J_df.Site<- J_Site %>%
  dplyr::select(Survey_ID, Date, Crew_name,Night_number, Start_time, End_time,
                 Site, Start_temp.F., Start_wind.km.h.)

# Will use start wind and temp because there are 2 NAs in the end wind/temps
# for Aug. spotlight surveys.
#There are 2 NAs in June22 site info - start temp and wind 
# - I will use the end temp and wind for these data to fill in the start temps/wind

#AUG22 Notes:
#using spotlight time start and end instead of using the individual site route
#start and end times because this encompasses the entire time that the techs
#were surveying (incidentals and site routes together) per night.\
# ***      - We do not have this data for June22 ***



## Removing NAs from start temp and wind :  -----------
(J_df.Site$Start_temp.F.[6]<-"74")
(J_df.Site$Start_wind.km.h.[6]<-11.0)


## Cleaning date format in df:  -----------
J_df.Site$Date<-lubridate:: dmy(J_df.Site$Date)


######################################################################
# DO NOT HAVE SPOTLIGHT START AND END TIMES FOR JUNE22
# IN AUGBTJR_DATASHEETDATACLEANING.R - CREATED STARTDATE AND ENDDATE
# COLUMNS TO COMBINE WITH SPOTLIGHT START/END TIMES TO CREATE 
# STARTMST.TIME AND ENDMST.TIME COLUMNS 

# NEED SUGGESTION WHAT TO DO HERE WITH OUT SPOTLIGHT START/END TIMES FOR JUNE


# DO I NEED TO DO THE SAME THING FOR JUNE22 - END/START DATE ?
# WOULD I USE SITE LEVEL START AND END TIMES? OR DO WE ESTIMATE WHAT WOULD BE A 
# SPOTLIGHT START/END TIME?



#######################################################################





#Making a yday column :  -----------

#Extract day of yr (out of 365):
J_df.Site$DayofYr <- lubridate::yday(J_df.Site$Date)



# # Creating a Route ID column (N or S routes):  ----------- 
J_df.Site$RouteID <- NA

for (r in 1:dim(J_df.Site)[1]){
  J_df.Site$RouteID <- ifelse(J_df.Site$Site == "Bigfoot_butte" |
                                J_df.Site$Site == "Simco" , "S.Route","N.Route")

}
# | is used as an OR argument in the ifelse statements 
# you would use & if you wanted an AND argument in the ifelse statement 




# Rab. Locations Cleaning: ------------------------------------------------

#June22:
#Selecting columns of interest:
J_Rab <- J_Rab %>%
  dplyr::select(RabID, Crew_name, Night_number,Rab.Obv, 
                Name, Date, lat, lon, RouteID )







# Saving Data: ------------------------------------------------------------

## Save csv's:   -----------

# Save cleaned csv:
write.csv( x = J_df.Site, 
           file = paste0( datapath,"June22/Site_June22.csv" ) )

write.csv( J_Rab, paste0( datapath, "June22/Rab.Loc_Simplified.Edit.csv") )



## Save work space:   -----------
# saving all data to the path
save.image( "Opt_Site.RData" )




# loading the work space:   -----------

#load("Opt_Site.RData")
