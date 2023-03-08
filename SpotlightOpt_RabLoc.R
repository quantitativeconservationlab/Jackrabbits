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
# 




#######################################################################
# SCRIPT IN PROGRESS : ----------------------------------------------------

#CURRENTLY WORKING ON:

# ASSESSING NEW A&J DAWN&DUSK FILES 
# - NEED TO DISSCUSS AUG GPS OVER LAPS 


# NEED TO CREATE AUG COTTONTAIL DF 
# FININSH CREATING JOINED/COMBINED DF FOR AUG LIKE J DF
# REFERENCE AUDIO CLIP FROM Mar.2 MEETING FOR NEXT STEP TO DO 
# 

#######################################################################
# SCRIPT IN PROGRESS : ----------------------------------------------------

#load("Opt_RabLoc")
#######################################################################





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
JDawn<- read.csv( file = paste0(datapath, "June22/Spotlight_June22_Dawn.csv"), 
                  #replaces those values with NA
                  na.strings = c(""," ","NA"), 
                  # includes column heading
                  header = TRUE )

JDusk<- read.csv( file = paste0(datapath, "June22/Spotlight_June22_Dusk.csv"), 
                  #replaces those values with NA
                  na.strings = c(""," ","NA"), 
                  # includes column heading
                  header = TRUE )

#Original data is exported directly from Basecamp and simplified in Excel
# to remove empty columns imported to csv (GPS unit specifics-unhelpful)

#Altered data:
Jrab <- read.csv( file = paste0(datapath, "June22/Rab.Loc_Simplified.Edit.csv"), 
                  #replaces those values with NA
                  na.strings = c(""," ","NA"), 
                  # includes column heading
                  header = TRUE )

#Altered Data has been changed in previous R scripts 


#Aug22:  -----------
#Individual GPS Data/original data:
#GPS2:
ADusk<- read.csv( file = paste0(datapath, "Aug22/GPS2_allRoutes22.csv"), 
                  #replaces those values with NA
                  na.strings = c(""," ","NA"), 
                  # includes column heading
                  header = TRUE )
head(ADusk)
#GPS1:
ADawn<- read.csv( file = paste0(datapath, "Aug22/GPS.clip_allRoutes22.csv"),                  #"Aug22/SpotlightBTJR_Aug22_Dusk.csv"), 
                  #replaces those values with NA
                  na.strings = c(""," ","NA"), 
                  # includes column heading
                  header = TRUE )

##############################################################################
## I think there is some overlapping GPS pts in these data set !
## Something is wrong here!
## I think this over lap is from the first 2 days where we ran surveys as a four 
## person crew and the techs practiced using the GPS's together 
##
##  NEED JC HELP TO FIGURE OUT WHICH GPS POINTS R OVERLAPPING !!!
## These are the GPS points pulled directly from Basecamp 

#####################################################################


##############################################################################

#Calling Rab Loc data:  -----------

#Aug22:  -----------
#Original Data (both gps combined all rab spp.) :
AUG22<- read.csv( file = paste0(datapath, "Aug22/RabLoc_labBaseCamp.csv"), 
                  #replaces those values with NA
                  na.strings = c(""," ","NA"), 
                  # includes column heading
                  header = TRUE )





#calling Site Data:  -----------
Asite <- read.csv(paste0(datapath, "Aug22/BigRabdf_extended.csv") , 
                  #replaces those values with NA
                  na.strings = c(""," ","NA"), 
                  # includes column heading
                  header = TRUE )

Jsite <- read.csv(paste0(datapath, "June22/Site_June22.csv") , 
                  #replaces those values with NA
                  na.strings = c(""," ","NA"), 
                  # includes column heading
                  header = TRUE )





# Inspecting & Formatting data: ------------------------------------------------

# Aug 2022 RabLoc data:  -----------
view(AUG22);str(AUG22)
head(AUG22)

#Filter out empty columns from df
AUG22<- AUG22 %>%
  dplyr::select(lat, lon, ele, time, name, CreationTime )
#Worked

#converting time stamp from UTC to MST:  -----------
AUG22$MST.time <- with_tz(lubridate::ymd_hms( AUG22$time),
                           tzone= "US/Mountain" )


#create yday column:
AUG22$DayOfYr <- lubridate::yday(AUG22$MST.time)

#Extract hour of night:
AUG22$Hour <- lubridate::hour(AUG22$MST.time)#create new Hour column

#remove dates that do not fall with in Aug1-12, 2022 (Survey period):
AUG22<-AUG22 %>% dplyr::filter(DayOfYr > 213 )
#Worked

#Can create Night_number by assigning this df to be called Arab below in code 


#####################################################################

##############################################################
# INVESTIGATION: AUG22 GPS PT. PROBLEMS : Overlapping MST.time - which are unique?
# - is there repeated gps points in aug gps data?
# - Need help figureing this out before can join aug rabloc and Aroutes

AUG22 %>%
  summarise(n_distinct(MST.time))#CreationTime : 957
unique(AUG22$MST.time)

AUG22 %>% dplyr::filter(DayOfYr == 213 ) #0
AUG22 %>% 
  #group_by(Hour) %>%
  filter(DayOfYr == 214 ) %>% #31
  #unique()#31
  #summarise(n_distinct(MST.time))#31
  
dplyr:::filter.data.frame( AUG22, DayOfYr == 216)#31
dplyr:::filter.data.frame( AUG22, DayOfYr == 216)


####################################################################


#remove this column from other Aug22df -Dusk:
#Filter out empty columns from df
ADusk <- ADusk %>%
  dplyr::select(lat, lon, ele, time, name, CreationTime )
#Worked
<<<<<<< HEAD
head(ADusk)

#Creating Crew_name Column:
ADusk<- ADusk %>%
=======
head( ADusk )
#Creating Crew_name Column:
ADusk <- ADusk %>%
>>>>>>> a3c83dcbcca38d37be702c30bf95f1f5680a140e
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
<<<<<<< HEAD
hist(ADusk$Hour)

=======

head(ADusk )
>>>>>>> a3c83dcbcca38d37be702c30bf95f1f5680a140e
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















# June 2022 RabLoc data:  -----------
view(Jrab);str(Jrab)
#NOTICING MISTAKE : *** SOMEHOW JRAB LAT/LON GOT COL NAMES SWITCHED
#NEED TO FIX:
names(Jrab)[names(Jrab)=="lat"] <-'long'
names(Jrab)[names(Jrab)=="lon"] <-'lat'

View(Jrab)#Worked 
#now change long to lon to match Arab df:
names(Jrab)[names(Jrab)=="long"]<-'lon'
#Worked, ready to proceed 

#Remove unneeded columns:
Jrab <- Jrab %>%  dplyr::select( -X ) 
view(Jrab);str(Jrab)#removed correctly

#changing Name column to "name" col. to match others 
names(Jrab)[names(Jrab)=="Name"] <-'name'
#good to proceed


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






# Aug 2022 site data:  -----------
view(Asite); str(Asite)
unique(Asite$Site)#good to proceed


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
#using date column from original data sheet lead to mistakes in the night
#end and start times because of the dusk teams recording the start of their 
#transects as diff days from the end of their transects for that night because 
#of thier surveys happening between 10pmand2am the next day







# Manipulating RabLoc Data: -----------------------------------------------

## Renaming AUG22 as Arab for cohesion:  -----------
Arab<-AUG22

#creating Survey Night column in jackrabbit df:  -----------
#creating for loop to assign survey night: 
Arab <- AUG22
#First create an empty column to be filled:
Arab$Night_number<-NA

#Create for loop to assign survey night dependant on yday and hour:

for (r in 1:dim(Arab)[1]){
  Arab$Night_number <-
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
View(Arab); str(Arab)
#Arab missing RouteID 
#Will be trying to pull from Aug22 Original/individual gps data to try and fix this problem:
####################################################################################
# may run in to difficulties here because the ADusk and ADawn have overlapping data 
####################################################################################
# tried to address this issue above but need some help 



  

##############################################################################



#configuring June rabloc data to match Arab:  -----------

head(Jrab)
# --> Noticing that Jrab is lacking:
#     - Hour, MST.time 
#     - DayOfYr,(will add these cols. in raster script )
#
# Because June we used the tablets we are missing the exact time of night that 
# each gps points were taken --> so we Have to (at this time) configure hour and 
# MST.time with the following code:

### turn times into lubridate times and dates
Jsite$StartDate<-lubridate::ymd(Jsite$StartDate)
Jsite$EndDate<-lubridate::ymd(Jsite$EndDate)

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


Jsite <- Jsite %>% 
  mutate( Route_start = lubridate::ymd_hm( paste( StartDate, Start_time, sep = " " ),
                                           tz = "MST" ), 
          Route_end = lubridate::ymd_hm( paste( EndDate, End_time, sep = " " ),
                                           tz = "MST" ) )
#check
head( Jsite)


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



# #RabLoc data altering: --------------------------------------------------

#Now add end and start times to Jrab
tail(Jrab);dim(Jrab)#need to get rid of empty rows after data
#check data
unique(Jrab$RouteID)#"S.Route" "N.Route" NA

#note that there are missing values
Jrab <- Jrab[ which(!is.na(Jrab$Night_number ) ), ] 


#Lubridate Date Manipulation:
Jrab$Date<-lubridate::mdy(Jrab$Date)
view(Jrab)


#Making a yday column :  -----------

#Extract day of yr (out of 365):
Jrab$DayOfYr <- lubridate::yday(Jrab$Date)




#########################################################################
# Removing cottontails/creating cottontail df: ----------------------------

#Removing any cottontails from Jrab data:
unique(Jrab$Name)
unique(Jrab$Rab.Obv)#Arab: cottontails have already been taken out 
#remove all cottontails and unknown leporids:
J_btjr <- Jrab %>%
  dplyr::filter(Rab.Obv=="Jackrab")
#check:
unique(J_btjr$Rab.Obv)#Worked
#J_btjr now = updated Jrab


#Removing any cottontails from A/J Dusk/Dawn original data:
unique(ADawn$Rab.Obv)

#Combining ADawn and ADusk :
head(ADusk)
head(ADawn)
#join both dataframes 
Atot <- bind_rows( ADawn, ADusk )
head(Atot)
dim( Atot); dim( ADusk); dim(ADawn)
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

###########################################################################

<<<<<<< HEAD
#Combining JDawn and JDusk :

#put all data frames into list
Jdf_list <- list(JDusk, JDawn)

#merge all data frames in list
Jtot<-Reduce(function(x, y) merge(x, y, all=TRUE), Jdf_list)
unique(Jtot$Crew_name)


=======
#Combining JDawn and JDusk:
head( JDawn);head(JDusk)
Jtot <- bind_rows( JDawn, JDusk )

#check 
unique( Jtot$Rab.Obv )
Jtot[is.na( Jtot$Rab.Obv), ]
>>>>>>> a3c83dcbcca38d37be702c30bf95f1f5680a140e
#filter out Cottontails: 
JCotton <- Jtot %>%
  filter( Rab.Obv == "Cottontail")
#filter out Jackrabbits:
JJrab <- Jtot %>%
  filter( Rab.Obv == "Jackrab")

unique(JJrab$Crew_name)

#there is a space in "dusk " crew name somewhere
#Fix:
JJrab$Crew_name[ which( JJrab$Crew_name == "Dusk ")] <- "Dusk"



#####################################################################3



# Duration Calculations: ---------------------------------------------------


#create an empty column for duration calculation to be put in for each site level df:
JRoutes$Duration<- "NA"
ARoutes$Duration<- "NA"

#Configuring Duration.Hrs column from start and end times:
JRoutes$Duration<-abs(difftime(JRoutes$Start_time, 
                                    JRoutes$End_time, units = "mins"))


#alter date/time format before Duration calc.s:
## turn times into lubridate times and dates
ARoutes$End_time <-lubridate::mdy_hm(ARoutes$End_time)
#already in MST from previous steps and dont need to convert it here 
ARoutes$Start_time <-lubridate::mdy_hm(ARoutes$Start_time)


#Calc. Aug duration col:
ARoutes$Duration<-abs(difftime(ARoutes$Start_time, 
                               ARoutes$End_time, units = "mins"))


head( ARoutes)
head( JRoutes)


###################################################################
# Combining RabLoc and Routes/siteinfo: -----------------------------------

#checking that the variables we are going to join the dfs together with are in 
# the same format as each other :
unique( J_btjr$Crew_name ); unique( JRoutes$Crew_name )
#There is a space after some of the dawn and dusk entries 

#Fix:
J_btjr$Crew_name[ which( J_btjr$Crew_name == "Dusk ")] <- "Dusk"
J_btjr$Crew_name[ which( J_btjr$Crew_name == "Dawn ")] <- "Dawn"

#checking the 2 other(group by) variables match each other in both dfs:
unique(J_btjr$RouteID ); unique( JRoutes$RouteID )#same, good to go

unique( J_btjr$Night_number ); unique( JRoutes$Night_number )#same, good to go


# Joining rablocations and site level info together: 
#june:
J <- J_btjr %>% 
  select( -DayOfYr, -Date, -RabID, -Rab.Obv ) %>% 
  left_join(., JRoutes, 
               by = c("Crew_name", "RouteID", "Night_number") )  

head(J);dim( J)# worked!

#Repeat for August:
#Check variables match:
unique(Arab$)

A <-  Arab %>% 
  select( -DayOfYr, -Hour, -Date, -RabID, -Rab.Obv ) %>% 
  left_join(., ARoutes, 
            by = c("Crew_name", "RouteID", "Night_number") )  

head(A);dim(A )


  ####################################################################
   ################################################## !!!!!
  


# Saving Data: ------------------------------------------------------------

## Save csv's:   -----------

# Save cleaned csv:
#Rab.locations:
write.csv( x = Arab, file = paste0(datapath, "Aug22/Arab.csv" ) )
write.csv( x = Jrab, file = paste0(datapath, "June22/Jrab.csv" ) )
write.csv( x = J_btjr, file = paste0(datapath, "June22/J_btjr.csv" ) )
#J_btjr = Jrab data filtered for just jackrabs - excluding cottontails or unknowns 
#Arab = already filtered to only include jackrabs 

#Site info:
write.csv( x = JRoutes, file = paste0(datapath, "June22/Jroutes.csv" ) )
write.csv( x = ARoutes, file = paste0(datapath, "Aug22/Aroutes.csv" ) )




## Save work space:   -----------
# saving all data to the path
save.image("Opt_RabLoc")



########################### end of script ####################################