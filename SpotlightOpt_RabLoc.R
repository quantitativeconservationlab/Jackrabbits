# Background --------------------------------------------------------------
# This script in the 2nd of 3 spotlight methodology optimization scripts 
# This script's purpose:
#        - Clean and format the rabbit locations/gps points data (Aug & June 2022)
#        - Develop spatial objects from the rab loc data to be used in next steps (Spatial analysis)
#        









# Setup -------------------------------------------------------------------

## Set up your work space and load relevant packages -----------

## Clean your work space to reset your R environment. 
rm( list = ls() )




## Load packages relevant to this script:  -------------

library( tidyverse ) #package for easy data manipulation
# set option to see all columns and more than 10 rows
options( dplyr.width = Inf, dplyr.print_min = 100 )
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


#Calling Rab Loc data:  
Arab <- read.csv( paste0( datapath, "Aug22/BTJR.obs_AUG_Clean.csv") )
Jrab <- read.csv( file = paste0(datapath, "June22/Rab.Loc_Simplified.Edit.csv"))

#calling Site Data:
Asite <- read.csv(paste0(datapath, "Aug22/BigRabdf_extended.csv") )
Jsite <- read.csv(paste0(datapath, "June22/Site_June22.csv") )


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

#Create for loop to assign survey night dependant on yday and hour:

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
#NOTICING MISTAKE : *** SOMEHOW JRAB LAT/LON GOT COL NAMES SWITCHED
#NEED TO FIX:
names(Jrab)[names(Jrab)=="lat"] <-'long'
names(Jrab)[names(Jrab)=="lon"] <-'lat'

View(Jrab)#Worked 
#now change long to lon to match Arab df:
names(Jrab)[names(Jrab)=="long"]<-'lon'
#Worked, ready to proceed 

head(Jrab)
# --> Noticing that Jrab is lacking:
#     - Hour, MST.time 
#     - DayOfYr, geometry (will add these cols. in raster script )
#
# Because June we used the tablets we are missing the exact time of night that 
# each gps points were taken --> so we Have to (at this time) configure hour and 
# MST.time with the following code:


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

#Now I create a routebased dataframe for august that only contains the relevant 
# columns that we need at the site level 
ARoutes <- Asite %>% 
  #group by crew and night
  group_by( Crew_name, Night_number ) %>% 
  #get mean temperature and wind 
  summarise( RouteID = first(RouteID),
             temp.F = mean( Start_temp.F., na.rm = TRUE),
             wind.km.h = mean( Start_wind.km.h., na.rm = TRUE),
             Start_time = first(Start_MST.time),
             End_time = last(End_MST.time),
             StartYday = first(DayofYr),
             EndYday = last(DayofYr) ) 

#check 
ARoutes


#RabLoc data altering:

#Now add end and start times to Jrab
tail(Jrab);dim(Jrab)
#check data
unique(Jrab$RouteID)
#note that there are missing values
Jrab <- Jrab[ which(!is.na(Jrab$Night_number ) ), ] 


#Lubridate Date Manipulation:
Jrab$Date<-lubridate::mdy(Jrab$Date)
view(Jrab)


#Making a yday column :  -----------

#Extract day of yr (out of 365):
Jrab$DayOfYr <- lubridate::yday(Jrab$Date)


#altering column name of Arab to match naming in Jrab,A&JRoutes :
names(Arab)[names(Arab)=="SurveyNight"] <-'Night_number'




# Duration Calulations: ---------------------------------------------------


#create an empty column for duration calculation to be put in for each site level df:
JRoutes$Duration<- "NA"
ARoutes$Duration<- "NA"

#Configuring Duration.Hrs column from start and end times:
JRoutes$Duration<-abs(difftime(JRoutes$Start_time, 
                                    JRoutes$End_time, units = "mins"))


#alter date/time format before Duration calc.s:
ARoutes$Start_time <-lubridate::mdy_hm(ARoutes$Start_time)

ARoutes$Start_time <- with_tz (lubridate::ymd_hms( ARoutes$Start_time),
                               tzone= "US/Mountain" )

ARoutes$End_time <-lubridate::mdy_hm(ARoutes$End_time)

ARoutes$End_time <- with_tz (lubridate::ymd_hms( ARoutes$End_time),
                               tzone= "US/Mountain" )

#Calc. Aug duration col:
ARoutes$Duration<-abs(difftime(ARoutes$Start_time, 
                               ARoutes$End_time, units = "mins"))










# Saving Data: ------------------------------------------------------------

## Save csv's:   -----------

# Save cleaned csv:
write.csv( x = Arab, file = paste0(datapath, "Aug22/Arab.csv" ) )
write.csv( x = Jrab, file = paste0(datapath, "June22/Jrab.csv" ) )
write.csv( x = JRoutes, file = paste0(datapath, "June22/Jroutes.csv" ) )
write.csv( x = ARoutes, file = paste0(datapath, "Aug22/Aroutes.csv" ) )




## Save work space:   -----------
# saving all data to the path
save.image("Opt_RabLoc")
#load("Opt_RabLoc")


########################### end of script ####################################