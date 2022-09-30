
# BTJR Route Datasheet Data Cleaning  -------------------------------------

###############################################################################



# Data Background/Description ---------------------------------------------

##This script was developed to clean and visualize black-tailed jackrabbit 
##count data collected using spotlight surveys conducted at dusk (10pm-2am) 
#and dawn(2am-6am) in the Morley Nelson Birds of Prey NCA. 
#Spotlight surveys were conducted by two teams of two trained technicians each
#(1 lead and 1 undergrad. pairs) during 8 day period in August 2022. 
#There are 4 total sites surveyed in August2022. All 4 sites were surveyed each night
#either by the dawn or dusk crew (road/weather conditions permitting). 
#Survey crews coordinated to randomize the order,start time, and start locations
#of each night and site that was surveyed (planning was done before hand @ beginning of season).
#The first 2 nights were an exception to the above statement about the individual teams. 
#The first two night of surveys were done as a team of 4 - with all technician in the 
#same truck, visiting all four sites together to standardize training measures
#and increase confidence in Identification skills in the field.
#along each site specific route the technicians drove no faster that 10mph 
#while surveying each site route 
#incidental rabbit observations, while technicians were driving no faster than 
#35mph, were also recorded on the GPS
#

## Github Code Source:  -----------

#Using the quantitativeconservationlab / Jackrabbits GitHub Repository 
#to store scripts. URL:https://github.com/quantitativeconservationlab/Jackrabbits.git


# Pseudo Code --------------------------------------------------------------

#(Focus on statements, mathematical operations, conditions, iterations, exceptions)

# START: this script will be used to combine the datasheet data from AUG 
#       spotlight surveys with the cleaned BTJR df created in the 300mBTJR aug 
#       script. Goal is to produce a cleaned predictor df to compare to the 
#       BTJR locations created in 300mgrid data prep script. I also want to 
#       visualize some of the predictors in this script from the BTJRdf and 
#       the site specific datasheet data from AUG


# INPUT:
#Common Z drive - jackrabbits - BTJR_Aug22_spotlight.surveys - Site_Aug2022
#this is the site specific data taken in the field and recorded on datasheets
#Common Z drive - jackrabbits - BTJR_Aug22_spotlight.surveys - Records_Aug2022
#this is the records of animals observed while during the surveys along the site
#specific routes 
#BTJR.obs_AUG_clean is a cleaned df/csv that was created in the BTJRDataPrep_300mGrid
#script that has the point locations of all the rabbits observed by both dawn
#and dusk crews over the entire Aug surey period


#CONFIGURE/MANIPULATE:
#create a cleaned predictor df and visualize all possible predictors that 
#are influencing BTJR detection/abundance to prep for abundance anaylsis 




# Setup -------------------------------------------------------------------

## Set up your work space and load relevant packages -----------

## Clean your work space to reset your R environment. 
rm( list = ls() )




## Load packages relevant to this script:  -------------

library( tidyverse ) #package for easy data manipulation
#install.packages("tidyverse") 
library(ggplot2)








# Setting Working Directory -----------------------------------------------


# Set working directory. This is the path to your Rstudio folder for this 
# project. If you are in your correct Rstudio project then it should be:
getwd()#"C:/Users/leticiacamacho/Documents/BTJR_MSProject/Rcode/Spotlights_Hab_2022/Jackrabbits"

# if so then:
workdir <- getwd() 
#creating working directory as an object so you can call it easier 
#     - without having to type it out or re-run each time.





# Load and Create Data ----------------------------------------------------

## Set data paths:  -----------

#Creating pathway to call the habitat NLCD data from the common drive
habpath <- "Z:/Common/QCLData/Habitat/" 
#Creating pathway to call the BTJR data from the common drive
datapath <- "Z:/Common/Jackrabbits/BTJR_Aug22_Spotlight.Surveys/" 





# Importing Data: ---------------------------------------------------------

## Import Rabbit Data:  -----------


site<- read.csv(paste0(datapath, "Site_Aug2022.csv"))

records<-read.csv(paste0(datapath, "Records_Aug2022.csv"))

Jackrabbits<-read.csv(paste0(datapath, "BTJR.obs_AUG_Clean.csv"))


#view:
View(site)


# Cleaning Data: ----------------------------------------------------------

## Creating "BigBTJR.df" from site.df with columns of interest:  -----------
Big.BTJR.df<- site %>%
  dplyr::select(Survey_ID, Date, Crew_name, Site, Start_temp.F., 
                Start_wind.km.h., Spotlight_Start.Time, 
                Spotlight_End.Time)
# Will use start wind and temp because there are 2 NAs in the end wind/temps
# for Aug. spotlight surveys.
#using spotlight time start and end instead of using the individual site route
#start and end times because this encompasses the entire time that the techs
#were surveying (incidentals and site routes together) per night.


## Removing NAs from end times :  -----------
Big.BTJR.df$Spotlight_End.Time[15]<-"1:46";Big.BTJR.df$Spotlight_End.Time
Big.BTJR.df$Spotlight_End.Time[16]<-"1:46";Big.BTJR.df$Spotlight_End.Time
Big.BTJR.df$Spotlight_End.Time[17]<-"1:58";Big.BTJR.df$Spotlight_End.Time
Big.BTJR.df$Spotlight_End.Time[18]<-"1:58";Big.BTJR.df$Spotlight_End.Time
#found the average time it took for this crew to survey the same routes on 
#other nights through the survey period (which was 3hr14mins) and added them
#to the start time to get these end times that were missing. 


## Create an end date column 
Big.BTJR.df$EndDate<-c("August 2,2022","August 2,2022","August 2,2022",
                       "August 2,2022","August 3,2022","August 3,2022",
                       "August 3,2022","August 3,2022","August 4,2022",
                       "August 4,2022","August 5,2022", "August 5,2022",
                       "August 8,2022","August 8,2022", "August 9,2022",
                       "August 9,2022","August 13,2022","August 13,2022", 
                       "August 4,2022","August 4,2022","August 5,2022",
                       "August 5,2022", "August 8,2022", "August 8,2022", 
                       "August 9,2022", "August 9,2022", "August 11,2022",
                       "August 11,2022",  "August 12,2022", "August 12,2022")

#have to do this to configure the correct end time below
#otherwise some of the dates would be off for dusk crew who worked from 10pm
#to 2am the next day 



## Cleaning date format in Big BTJR df:  -----------
Big.BTJR.df$Date <-lubridate::mdy(Big.BTJR.df$Date)
Big.BTJR.df$EndDate<-lubridate::mdy(Big.BTJR.df$EndDate)

#Combining date with start time to one column :  -----------
Big.BTJR.df$Start_MST.time <- paste(Big.BTJR.df$Date, Big.BTJR.df$Spotlight_Start.Time, 
                              sep = " ")

#Combining date with end time to one column :  -----------
Big.BTJR.df$End_MST.time <- paste(Big.BTJR.df$EndDate, Big.BTJR.df$Spotlight_End.Time, 
                              sep = " ")

#Making a yday column :  -----------

#Extract day of yr (out of 365):
Big.BTJR.df$DayofYr <- lubridate::yday(Big.BTJR.df$)

###########################################################################################
# NEED TO FIGURE OUT DATES IN START AND END MST.TIMES - MAKE SURE THEY ARE ALL CORRECT 
# THEY ARE GETTING MESSED UP BECASUE OF THE TIME CHANGE OVER FROM AM TO PM DURRING THE SURVEYS 
#TRIED TO USE THE DATE LISTED IN ORIGINAL DATE TO COMBINE WITH THE START AND END TIME BUT NOW THIS 
#HAS MADE A MESS OF THE CORRECT TIMES AND CORRISPONDING DAYS 


# FIX THIS FIRST THING SUNDAY MORNING 
# THEN CREATE RABBIT DF WITH CLEANED DATA AND NEW ROUTE N AND ROUTE S 
#COLUMNS TO BE FILLED WITH THE FOR LOOP THAT JC SHOWED ME TODAY (FRIDAY)



###########################################################################################


# Creating a Route ID column (N or S routes):  ----------- 
Big.BTJR.df$RouteID <- NA

for (r in 1:dim(Big.BTJR.df)[1]){
  Big.BTJR.df$RouteID <- ifelse(Big.BTJR.df$Site == "Bigfoot_butte" |
                                Big.BTJR.df$Site == "Simco" , "S.Route","N.Route")
                                   
}
# | is used as an OR argument in the ifelse statements 
# you would use & if you wanted an AND argument in the ifelse statement 






## Removing un-needed columns :  -----------
Big.BTJR.df <- Big.BTJR.df %>%
  dplyr::select(RouteID, SurveyDate=Date, Start_MST.time, End_MST.time, 
                Start_temp.F., Start_wind.km.h.) 







View(Big.BTJR.df)

























































# Saving Data -------------------------------------------------------------
## Save csv's:   -----------

# Save cleaned csv:
write.csv( x = , file =  )





## Save work space:   -----------
# saving all data to the path
save.image("xxxx.RData")




# loading the workspace
load("xxxx.RData")






