# Background --------------------------------------------------------------
# This script in the 2nd of 3 spotlight methodology optimization scripts 
# This script's purpose:
#        - Clean and format the rabbit locations/gps points data (Aug & June 2022)
#        - Develope spatial objects from the rab loc data to be used in next steps (Spatial analysis)
#        









# Setup -------------------------------------------------------------------

## Set up your work space and load relevant packages -----------

## Clean your work space to reset your R environment. 
rm( list = ls() )


# loading the work space:   -----------
# This workspace was saved from previous "SpotlightOptimization_SiteInfo.R" script
# workspace contains correctly formatted Aug and June 2022 Site csv's and rab loc
# - Rab loc need to be formatted : = purpose of this cleaning rab loc script 

#load("Opt_Site.RData")



## Load packages relevant to this script:  -------------

## Some packages may not been installed - install them if needed
## Some packagees may not be needed for different parts of this script 
## Load the packages needed - when needed


#install.packages("oce")
# library(oce)# used for moon phase information
library( tidyverse ) #package for easy data manipulation
# set option to see all columns and more than 10 rows
options( dplyr.width = Inf, dplyr.print_min = 100 )
#install.packages("tidyverse") 
# library(ggplot2)
# library(lubridate)
# library(tidyr)
# library(dplyr)



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

Arab <- read.csv( paste0( datapath, "Aug22/BTJR.obs_AUG_Clean.csv") )
Jrab <- read.csv( file = paste0(datapath, "June22/Rab.Loc_Simplified.Edit.csv"))

Asite <- read.csv(paste0(datapath, "Aug22/BigRabdf_extended.csv") )
Jsite <- read.csv(paste0(datapath, "June22/Site_June22.csv") )

# View rab loc data pulled from "Opt_Site.RData" work space:  -----------

# Aug 2022 RabLoc data:  -----------
view(Arab);str(Arab)
# --> noticing that the geometry column has been split into 2 columns 
#   - will need to join these together so it can be read by sf package/spatial 


# June 2022 RabLoc data:  -----------
view(Jrab);str(Jrab)
head( Jrab)

#cleaning by removing the X column
Jrab <- Jrab %>%  dplyr::select( -X ) 

# Aug 2022 site data:  -----------
view(Asite); str(Asite)

# June 2022 site data:  -----------
view(Jsite);str(Jsite)

#also checking unique sites
unique(Jsite$Site)
#note that there are two naming formats for bigfoot
#fix
Jsite$Site[ which( Jsite$Site == "bigfoot_butte")] <- "Bigfoot_butte"

##############################################################################
######### NEED TO DISCUSS W/JC: MISSING JUNE DATA FROM SpotlightOpt._SiteInfo############
######## fix that in that 1st script and then reload it and resave workspace so it can be pulled up here 

################################################################################





# Manipulating RabLoc data: -----------------------------------------------

#Fixing geometry col. in Aug rabloc data:  -----------
Arab$geometry<- paste(Arab$geometry, Arab$X, sep = ",")
View(Arab)
head(Arab)
# geting rid of the unneeded column:  
Arab<- Arab[-11]


#creating Survey Night column in jackrabbit df:  -----------
#creating for loop to assign survey night: 
#First create an empty column to be filled:
Arab$SurveyNight<-NA

#Create for loop:

for (r in 1:dim(Arab)[1]){
  Arab$SurveyNight <-
    ifelse(Arab$DayOfYr == "213", "1",
    ifelse(Arab$DayOfYr == "214" & Arab$Hour < 20, "1",
    ifelse(Arab$DayOfYr == "214" & Arab$Hour > 20, "2", 
    ifelse(Arab$DayOfYr == "215" & Arab$Hour < 20, "2",
    ifelse(Arab$DayOfYr == "215" & Arab$Hour > 20, "3",
    ifelse(Arab$DayOfYr == "216" & Arab$Hour < 20, "3",
    ifelse(Arab$DayOfYr == "216" & Arab$Hour > 20, "4", 
    ifelse(Arab$DayOfYr == "217" & Arab$Hour < 20, "4",
    ifelse(Arab$DayOfYr == "219" & Arab$Hour > 20, "5",
    ifelse(Arab$DayOfYr == "220" & Arab$Hour < 20, "5",
    ifelse(Arab$DayOfYr == "220" & Arab$Hour > 20, "6", 
    ifelse(Arab$DayOfYr == "221" & Arab$Hour < 20, "6",
    ifelse(Arab$DayOfYr == "222" & Arab$Hour > 20, "7",
    ifelse(Arab$DayOfYr == "223" & Arab$Hour < 20, "7",
    ifelse(Arab$DayOfYr == "223" & Arab$Hour > 20, "8", 
    ifelse(Arab$DayOfYr == "224" & Arab$Hour < 20, "8",
    "NA"))))))))))))))))
}

#checking worked correctly:
View(Arab)
# --> noticing that the name col. in Arab is not capitalized but it is in Jrab




  

  
#configuring June rabloc data to match Arab:  -----------
view(Jrab)
# --> Noticing that Jrab is lacking:
#     - Hour, MST.time 
#     - DayOfYr, geometry (will add these cols.below)
#
# Because June we used the tablets we are missing the exact time of night that 
# each gps points were taken --> so we can not (at this time) configure hour, 
# and thus cant config. MST.time either right now


### turn times into lubridate times and dates
Jsite <- Jsite %>% 
  mutate( Route_start = lubridate::ymd_hm( paste( Date, Start_time, sep = " " ),
                                           tz = "MST" ), 
          Route_end = lubridate::ymd_hm( paste( Date, End_time, sep = " " ),
                                           tz = "MST" ) )
#check
head( Jsite)

#fix missing value for Simco on Night number 7....assume it's 1 hour later 
#based on how long it took on other nights
NAid <- which( is.na( Jsite$Route_end ))
Jsite$Route_end[NAid] <- Jsite$Route_start[NAid] + hours(1) 
# extract rough start and end times for each route from site level info
# for each crew 

Jsite <- Jsite %>%  arrange( Night_number ) %>% 
  group_by( Night_number, Crew_name ) %>% 
  arrange( DayofYr, Start_time ) %>% 
  mutate( 
    Route_starttime = ifelse( first(Site) == "Bigfoot_butte", 
                  as.character( first(Route_start) - hours(1) - minutes(15) ),
          ifelse( first(Site) == "Simco", 
                  as.character( first(Route_start) - minutes(15) ),
                  as.character(first(Route_start) - minutes(30) ) ) ), 
    Route_endtime = ifelse( last( Site ) == "Simco", 
                    as.character( last(Route_end) +  minutes(15) ),
          ifelse( last(Site) == "Bigfoot_butte", 
                  as.character( last(Route_end) + hours(1) + minutes(15) ),
                   as.character( last(Route_end) + minutes(30) ) ) )  )

#check
head(Jsite)


#Now I create a routebased dataframe for June that only contains the relevant 
# columns that we need at the site level 
JRoutes <- Jsite %>% 
  #group by crew and night
  group_by( Crew_name, Night_number ) %>% 
  #get mean temperature and wind 
  summarise( RouteID = first(RouteID),
    temp.F = mean( Start_temp.F., na.rm = TRUE),
          wind.km.h = mean( Start_wind.km.h., na.rm = TRUE),
          Start_time = first(Route_starttime),
          End_time = last(Route_endtime),
          StartJday = first(DayofYr),
          EndJday = last(DayofYr) ) 

#check 
JRoutes

#Now add end and start times to Jrab
tail(Jrab);dim(Jrab)
#check data
unique(Jrab$RouteID)
#note that there are missing values
# I remove missing values here instead of later # sorry Leti
Jrab <- Jrab[ which(!is.na(Jrab$Night_number ) ), ] 


#############################################################################
############### WILL BE ASKING JC WHAT TO DO ABOUT THIS FEB.24 ###########
#########################################################################

# I added both Crew_name and RouteID to the Jrab df so we can assess what to do next - with missing data for june 






#Lubridate Date Manipulation:
Jrab$Date<-lubridate::mdy(Jrab$Date)
view(Jrab)


#Making a yday column :  -----------

#Extract day of yr (out of 365):
Jrab$DayOfYr <- lubridate::yday(Jrab$Date)

# 
# #remove all rows with a missing value in any column:  -----------
# view(Jrab)
# 
# row.has.na <- apply(Jrab, 1, function(x){any(is.na(x))})
# sum(row.has.na)
# Jrab.filtered <- Jrab[!row.has.na,]
# 
# Jrab<-Jrab.filtered
# head(Jrab); dim(Jrab)

#### calculate sampling effort for each month #######
head( Jrab )
head( Jsite )

# Saving Data: ------------------------------------------------------------

## Save csv's:   -----------

# Save cleaned csv:
write.csv( x = Arab, file = paste0(datapath, "Aug22/Arab.csv" ) )
write.csv( x = Jrab, file = paste0(datapath, "June22/Jrab.csv" ) )
write.csv( x = JRoutes, file = paste0(datapath, "June22/Jroutes.csv" ) )



## Save work space:   -----------
# saving all data to the path
save.image("Opt_RabLoc")


########################### end of script ####################################