# Background --------------------------------------------------------------
# This script in the 2nd of 3 spotlight methodology optimization scripts 
# This script's purpose:
#        - Clean and format the rabbit locations/gps points data (Aug & June 2022)
#        - Get Aug22 and June22 rab loc data matching 
#        - Get Aug22 and June22 site information/route data matching 
#        - Have both rabloc and site/route data have same matching columns:
#                 1. Rab.Obv, 
#                 2. Crew_name, 
#                 3. RouteID, 
#                 4. Night_number



##########################################################################

# IN PROGRESS:

# Need to alter august Aroute df 
#   - split up day 1 & 2 so that it is not just the whole time the spotlights 
#     were on the whole night 
#          - just manually alter the times for the 2 lines per crew per night 
#             to be individual not as a whole per night 
#         
#         - need to go back thru datasheets from aug to find these times 
#           (route start time individually)

# 
# June df check :
#     - remove night were dawn crew did n and s route sites combined (dawn day 7)
#     - make note in script about dawn night 7 - needs to be replaced with an NA
#       & then filter out dawn night 7 moving forward becasue they were mixed n and s route
#       due to dusk crew getting stuck in mud and dawn taking over comb.hab and bigfoot sites 





##########################################################################





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
#Creating pathway to call the BTJR data from the common drive:
datapath <- "Z:/Common/Jackrabbits/Data/Spotlights/" 


#Calling Rab Loc data:  -----------
#Call in the desired csv's needed for next steps:

#June22:  -----------
#Original Data:
JDusk<- read.csv( file = paste0(datapath, "June22/Spotlight_June22_Dusk.csv"), 
                  #replaces those values with NA
                  na.strings = c(""," ","NA"), 
                  # includes column heading
                  header = TRUE )


JDawn<- read.csv( file = paste0(datapath, "June22/Spotlight_June22_Dawn.csv"), 
                  #replaces those values with NA
                  na.strings = c(""," ","NA"), 
                  # includes column heading
                  header = TRUE )


#Original data is exported directly from Basecamp and simplified in Excel
# to remove empty columns imported to csv (GPS unit specifics-unhelpful)


#Aug22:  -----------
#Individual GPS Data/original data:
#GPS2:
ADusk<- read.csv( file = paste0(datapath, "Aug22/GPS2_allRoutes22.csv"), 
                  #replaces those values with NA
                  na.strings = c(""," ","NA"), 
                  # includes column heading
                  header = TRUE )
#check
head(ADusk)
#GPS1:
ADawn<- read.csv( file = paste0(datapath, "Aug22/GPS.clip_allRoutes22.csv"),  
                  #replaces those values with NA
                  na.strings = c(""," ","NA"), 
                  # includes column heading
                  header = TRUE )
#check
head(ADawn)



#calling Site Data:  -----------
#Aug22:  -----------
Asite <- read.csv(paste0(datapath, "Aug22/BigRabdf_extended.csv") , 
                  #replaces those values with NA
                  na.strings = c(""," ","NA"), 
                  # includes column heading
                  header = TRUE )

#check
head(Asite)

#June22:  -----------
Jsite <- read.csv(paste0(datapath, "June22/Site_June22.csv") , 
                  #replaces those values with NA
                  na.strings = c(""," ","NA"), 
                  # includes column heading
                  header = TRUE )
#check
head(Jsite)



# Inspecting & Formatting data: ------------------------------------------------

# June 2022 RabLoc data:  -----------
#Formatting June22 Original dfs:  -----------
view(JDawn);str(JDawn)
view(JDusk);str(JDusk)

#changing ID column to "name" col. to match others 
names(JDusk)[names(JDusk)=="ID"] <-'name'
names(JDawn)[names(JDawn)=="ID"] <-'name'
#worked
#Remove unneeded columns:
JDawn <- JDawn %>%  dplyr::select( -OBJECTID ) 

JDusk <- JDusk %>%  dplyr::select( -OBJECTID ) 
#check
str(JDusk)#Worked
#check
str(JDawn)#Worked


#rename lat and lon columns:
names(JDawn)[names(JDawn)=="x"] <-'lon'
names(JDawn)[names(JDawn)=="y"] <-'lat'
#worked

names(JDusk)[names(JDusk)=="x"] <-'lon'
names(JDusk)[names(JDusk)=="y"] <-'lat'
#worked

# June 2022 site data:  -----------
view(Jsite);str(Jsite)
#Noticing that Aug22 has the following columns & June22 does not:
# $ Spotlight_Start.Time: chr  "21:38"  
# $ Spotlight_End.Time  : chr  "3:15" 
# $ StartDate           : chr  "8/1/2022"       *BUT DO HAVE START_TIME @ SITE LEVEL
# $ EndDate             : chr  "8/2/2022"       *BUT DO HAVE END_TIME @ SITE LEVEL
# $ Start_MST.time      : chr  "8/1/2022 21:38"
# $ End_MST.time        : chr  "8/2/2022 3:15"
# Will need to create these variables in following code

#also checking unique sites
unique(Jsite$Site)
#note that there are two naming formats for bigfoot
#fix
Jsite$Site[ which( Jsite$Site == "bigfoot_butte")] <- "Bigfoot_butte"

#Found Missing End_time (NA) Value
#Fix:
#fix missing value for Simco on Night number 7....assume it's 1 hour later 
#based on how long it took on other nights
NAid1 <- which( is.na( Jsite$End_time ))
Jsite$End_time[NAid1] <- "23:51"
# Because I changed this directly here, I did not need JC's following code:
# #fix missing value for Simco on Night number 7....assume it's 1 hour later 
# #based on how long it took on other nights
# #NAid <- which( is.na( Jsite$Route_end ))
# #Jsite$Route_end[NAid] <- Jsite$Route_start[NAid] + hours(1) 


# Need to make a start/end date for Jsite before can calc. correctly formatted 
# Route_start/endtime step 

## Create an start date column 
Jsite$StartDate<-c("2022-06-06","2022-06-07","2022-06-07",
                   "2022-06-07","2022-06-07","2022-06-08",
                   "2022-06-08","2022-06-08","2022-06-08",
                   "2022-06-08","2022-06-09","2022-06-09",
                   "2022-06-09","2022-06-10","2022-06-10",
                   "2022-06-10","2022-06-10","2022-06-11", 
                   "2022-06-11","2022-06-11","2022-06-11",
                   "2022-06-12","2022-06-12","2022-06-12", 
                   "2022-06-13","2022-06-14","2022-06-14",
                   "2022-06-15","2022-06-16","2022-06-16", 
                   "2022-06-16")

#have to do this to configure the correct end time below
#otherwise some of the dates would be off for dusk crew who worked from 10pm
#to 2am the next day 

## Create an end date column 
Jsite$EndDate<-c("2022-06-06","2022-06-07","2022-06-07",
                 "2022-06-07","2022-06-07","2022-06-08",
                 "2022-06-08","2022-06-09","2022-06-08",
                 "2022-06-08","2022-06-09","2022-06-09",
                 "2022-06-09","2022-06-10","2022-06-10",
                 "2022-06-11","2022-06-10","2022-06-11", 
                 "2022-06-11","2022-06-11","2022-06-11",
                 "2022-06-12","2022-06-12","2022-06-12", 
                 "2022-06-13","2022-06-14","2022-06-14",
                 "2022-06-15","2022-06-16","2022-06-16", 
                 "2022-06-16")
#check:
View(Jsite);str(Jsite)#worked 
#using date column from original data sheet lead to mistakes in the night
#end and start times because of the dusk teams recording the start of their 
#transects as diff days from the end of their transects for that night because 
#of thier surveys happening between 10pmand2am the next day




#configuring June rabloc data to align with Aug rablocs:  -----------

# Because June we used the tablets we are missing the exact time of night that 
# each gps points were taken --> so we Have to (at this time) configure hour and 
# MST.time with the following code:

### turn times into lubridate times and dates
Jsite$StartDate<-lubridate::ymd(Jsite$StartDate)
Jsite$EndDate<-lubridate::ymd(Jsite$EndDate)

View(Jsite);str(Jsite)#working



Jsite <- Jsite %>% 
  mutate( Route_start = lubridate::ymd_hm( paste( StartDate, Start_time, sep = " " ),
                                           tz = "MST" ), 
          Route_end = lubridate::ymd_hm( paste( EndDate, End_time, sep = " " ),
                                         tz = "MST" ) )
#check
head( Jsite)
View(Jsite);str(Jsite)#working


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
view(Jsite);head(Jsite)#Worked, ready to proceed 


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
             StartYday = first(DayofYr),
             EndYday = last(DayofYr) ) 

#check 
view(JRoutes);head(JRoutes)
#seems to be working



# Combining JDawn and JDusk : ---------------------------------------------

head( JDawn);head(JDusk)#perfect matching columns
Jtot <- bind_rows( JDawn, JDusk )
#can do it this way if your column names line up/match perfectly 
#check :
unique( Jtot$Rab.Obv )
Jtot[is.na( Jtot$Rab.Obv), ]
#These NAs will be filtered out in below process 
#these nas are from wpts that were not named or unknown rabits/not speciated 


# filter out Cottontails:  ------------------------------------------------
JCotton <- Jtot %>%
  filter( Rab.Obv == "Cottontail")
#filter out Jackrabbits:
JJrab <- Jtot %>%
  filter( Rab.Obv == "Jackrab")

unique(JJrab$Crew_name)

#there is a space in "dusk " crew name somewhere
#Fix:
JJrab$Crew_name[ which( JJrab$Crew_name == "Dusk ")] <- "Dusk"






# Duration Calculations: ---------------------------------------------------


#create an empty column for duration calculation to be put in for each site level df:
JRoutes$Duration<- "NA"

#Configuring Duration.Hrs column from start and end times:
JRoutes$Duration<-abs(difftime(JRoutes$Start_time, 
                               JRoutes$End_time, units = "mins"))




# Combining RabLoc and Routes/siteinfo: -----------------------------------

head(JJrab);head(JRoutes)
#checking that the variables we are going to join the dfs together with are in 
# the same format as each other :
unique( JJrab$Crew_name ); unique( JRoutes$Crew_name )#same

#checking the 2 other(group by) variables match each other in both dfs:
unique(JJrab$RouteID ); unique( JRoutes$RouteID )#same, good to go

unique( JJrab$Night_number ); unique( JRoutes$Night_number )#same, good to go


# Joining rablocations and site level info together: 
#june:
J <- JJrab %>% 
  #select( -DayOfYr, -Date, -RabID, -Rab.Obv ) %>% 
  left_join(., JRoutes, 
            by = c("Crew_name", "RouteID", "Night_number") )  

head(J);dim( J)#












# Aug22 data: -------------------------------------------------------------



#remove this column from other Aug22df -Dusk:
#Filter out empty columns from df
ADusk <- ADusk %>%
  dplyr::select(lat, lon, ele, time, name, CreationTime )
#Worked
head(ADusk)

#Creating Crew_name Column:
ADusk <- ADusk %>%
  mutate(Crew_name = "Dusk")

#converting time stamp from UTC to MST:  -----------
ADusk$MST.time <- with_tz (lubridate::ymd_hms( ADusk$time),
                           tzone= "US/Mountain" )

#create yday column:
ADusk$DayOfYr <- lubridate::yday(ADusk$MST.time)

#Extract hour of night:
ADusk$Hour <- lubridate::hour(ADusk$MST.time)#create new Hour column

#remove dates that do not fall with in Aug1-12, 2022 (Survey period):

ADusk <- ADusk %>% dplyr::filter(DayOfYr > 212 )
#check:
hist(ADusk$DayOfYr)# all dates fall between survey period
hist(ADusk$Hour)


head(ADusk )
## Repeate for Aug22 Dawn:
#remove this column from other Aug22df -Dusk:
#Filter out empty columns from df
ADawn <- ADawn %>%
  dplyr::select(lat, lon, ele, time, name, CreationTime )
#Worked

#Creating Crew_name Column:
ADawn <- ADawn %>%
  mutate(Crew_name = "Dawn")

#converting time stamp from UTC to MST:  -----------
ADawn$MST.time <- with_tz (lubridate::ymd_hms( ADawn$time),
                           tzone= "US/Mountain" )

#create yday column:
ADawn$DayOfYr <- lubridate::yday(ADawn$MST.time)

#Extract hour of night:
ADawn$Hour <- lubridate::hour(ADawn$MST.time)#create new Hour column

#remove dates that do not fall with in Aug1-12, 2022 (Survey period):

ADawn <- ADawn %>% dplyr::filter(DayOfYr > 212 )
#check:
hist(ADawn$DayOfYr)# all dates fall between survey period
hist(ADawn$Hour)


# Aug 2022 site data:  -----------
view(Asite); str(Asite)
unique(Asite$Site)#good to proceed



# Manipulating RabLoc Data: -----------------------------------------------

#Combining ADawn and ADusk :
head(ADusk)
head(ADawn)#have matching column names
#join both dataframes 
Atot <- bind_rows( ADawn, ADusk )
head(Atot)
dim( Atot); dim( ADusk); dim(ADawn)

#creating Survey Night column in jackrabbit df:  -----------
#creating for loop to assign survey night: 
#First create an empty column to be filled:
Atot$Night_number<-NA

#Create for loop to assign survey night dependant on yday and hour:

for (r in 1:dim(Atot)[1]){
  Atot$Night_number <-
    ifelse(Atot$DayOfYr == "213", "1",
    ifelse(Atot$DayOfYr == "214" & Atot$Hour < 20, "1",
    ifelse(Atot$DayOfYr == "214" & Atot$Hour > 20, "2", 
    ifelse(Atot$DayOfYr == "215" & Atot$Hour < 20, "2",
    ifelse(Atot$DayOfYr == "215" & Atot$Hour > 20, "3",
    ifelse(Atot$DayOfYr == "216" & Atot$Hour < 20, "3",
    ifelse(Atot$DayOfYr == "216" & Atot$Hour > 20, "4", 
    ifelse(Atot$DayOfYr == "217" & Atot$Hour < 20, "4",
    ifelse(Atot$DayOfYr == "219" & Atot$Hour > 20, "5",
    ifelse(Atot$DayOfYr == "220" & Atot$Hour < 20, "5",
    ifelse(Atot$DayOfYr == "220" & Atot$Hour > 20, "6", 
    ifelse(Atot$DayOfYr == "221" & Atot$Hour < 20, "6",
    ifelse(Atot$DayOfYr == "222" & Atot$Hour > 20, "7",
    ifelse(Atot$DayOfYr == "223" & Atot$Hour < 20, "7",
    ifelse(Atot$DayOfYr == "223" & Atot$Hour > 20, "8", 
    ifelse(Atot$DayOfYr == "224" & Atot$Hour < 20, "8",
    "NA"))))))))))))))))
}


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
view(ARoutes);head(ARoutes)




# Removing cottontails/creating cottontail df: ----------------------------

#Removing any cottontails from A/J Dusk/Dawn original data:

#Seperating cottontails from BTJR dfs :
unique( Atot$name )
#extract rows belonging to jackrabbits
jids <- grep("^J", Atot$name, ignore.case = TRUE,value = FALSE )
#make column for rab.obv naming all cottontail
Atot$Rab.Obv <- "Cottontail"
#replace those in jids with jackrab
Atot$Rab.Obv[ jids ] <- "Jackrab"
#check
tail(Atot)
#filter out Cottontails: 
ACotton <- Atot %>%
  filter( Rab.Obv == "Cottontail")
#filter out Jackrabbits:
AJrab <- Atot %>%
  filter( Rab.Obv == "Jackrab")

unique(AJrab$Crew_name)


head( AJrab )

# check if there are any duplicates
AJrab %>% 
  select( lat, lon, ele, MST.time ) %>% 
duplicated() %>% sum()



# Duration Calculations: ---------------------------------------------------


#create an empty column for duration calculation to be put in for each site level df:
ARoutes$Duration<- "NA"

#alter date/time format before Duration calc.s:
## turn times into lubridate times and dates
ARoutes$End_time <-lubridate::mdy_hm(ARoutes$End_time)
#already in MST from previous steps and dont need to convert it here 
ARoutes$Start_time <-lubridate::mdy_hm(ARoutes$Start_time)


#Calc. Aug duration col:
ARoutes$Duration<-abs(difftime(ARoutes$Start_time, 
                               ARoutes$End_time, units = "mins"))


head( ARoutes)


# Combining RabLoc and Routes/siteinfo: -----------------------------------


#Repeat for August:
#Check variables match:
head(AJrab);head(ARoutes)
#checking that the variables we are going to join the dfs together with are in 
# the same format as each other :
unique( AJrab$Crew_name ); unique( ARoutes$Crew_name )#same

#checking the 2 other(group by) variables match each other in both dfs:
unique(AJrab$RouteID ); unique( ARoutes$RouteID )#Ajrab missing RouteID 
# create RouteID Column for AJrab:
AJrab$RouteID<-NA

for (r in 1:dim(AJrab)[1]){
  AJrab$RouteID <-
    ifelse(AJrab$Crew_name == "Dawn" & AJrab$Night_number == "3", "S.Route",
    ifelse(AJrab$Crew_name == "Dawn" & AJrab$Night_number == "4", "N.Route",
    ifelse(AJrab$Crew_name == "Dawn" & AJrab$Night_number == "5", "S.Route",
    ifelse(AJrab$Crew_name == "Dawn" & AJrab$Night_number == "6", "N.Route", 
    ifelse(AJrab$Crew_name == "Dawn" & AJrab$Night_number == "7", "S.Route",
    ifelse(AJrab$Crew_name == "Dawn" & AJrab$Night_number == "8", "N.Route",
    ifelse(AJrab$Crew_name == "Dusk" & AJrab$Night_number == "3", "N.Route",
    ifelse(AJrab$Crew_name == "Dusk" & AJrab$Night_number == "4", "S.Route",
    ifelse(AJrab$Crew_name == "Dusk" & AJrab$Night_number == "5", "N.Route",
    ifelse(AJrab$Crew_name == "Dusk" & AJrab$Night_number == "6", "S.Route", 
    ifelse(AJrab$Crew_name == "Dusk" & AJrab$Night_number == "8", "S.Route",
    "NA"))))))))))) 
} 
                  
#Check:
head(AJrab)
tail(AJrab)
           
unique( AJrab$Night_number ); unique( ARoutes$Night_number )
#Ajrab night number needs to be altered from character to integer
#as.integer(AJrab)
#Error: 'list' object cannot be coerced to type 'integer'
ARoutes$Night_number <- as.character(ARoutes$Night_number)
unique( AJrab$Night_number ); unique( ARoutes$Night_number )




#combining august site an rabbit location dfs:

A <-  AJrab %>% 
  #select( -DayOfYr, -Hour, -Date, -RabID, -Rab.Obv ) %>% 
  left_join(., ARoutes, 
            by = c("Crew_name", "RouteID", "Night_number") )  

head(A);dim(A )





# Saving Data: ------------------------------------------------------------

## Save csv's:   -----------

# Save cleaned csv:
#Rab.locations:
write.csv( x = AJrab, file = paste0(datapath, "Aug22/AJrab.csv" ) )
write.csv( x = JJrab, file = paste0(datapath, "June22/JJrab.csv" ) )


#Site info:
write.csv( x = JRoutes, file = paste0(datapath, "June22/Jroutes.csv" ) )
write.csv( x = ARoutes, file = paste0(datapath, "Aug22/Aroutes.csv" ) )

#Rabloc & Site combined info:
write.csv( x = A, file = paste0(datapath, "Aug22/AJrabRoutes.csv" ) )
write.csv( x = J, file = paste0(datapath, "June22/JJrabRoutes.csv" ) )


## Save work space:   -----------
# saving all data to the path
save.image("Opt_RabLoc")

#load("Opt_RabLoc")

########################### end of script ####################################