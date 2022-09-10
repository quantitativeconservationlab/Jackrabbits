
# Data Background/Description ---------------------------------------------
##This script was developed to clean and visualize black-tailed jackrabbit 
##count data collected using spotlight surveys conducted at dusk (10pm-2am) 
#and dawn(2am-6am) in the NCA by two teams of 2 trained technicians 
#(1 lead and 1 undergrad.) during 8 day period in August 2022. 


# Pseudo Code --------------------------------------------------------------
#(Focus on statements, mathematical operations, conditions, iterations, exceptions)

# START:

# INPUT:

# READ/GET:
#   (Input used when reading data )

# PRINT,DISPLAY,SHOW:
#   (Will show your output to a screen or the relevant output device)

# COMPUTE,CALCULATE,DETERMINE:
#   (Used to calc.the result of the expression) 

# SET,INIT:
#   (To initialize values)

# INCREMENT,BUMP:
#   (To incr.the value of a variable)

# DECREMENT:
#   (To reduce the value of a variable)




# Setup -------------------------------------------------------------------
## Set up your work space and load relevant packages -----------

# Clean your work space to reset your R environment. 
rm( list = ls() )

# load packages relevant to this script:
library( tidyverse ) #package for easy data manipulation
#install.packages("tidyverse") #Use this is package is not already installed or is not loaded properly 
library( lubridate ) #package for easy date and time manipulation
library( sf ) #package for spatial data manipulation 

# set option to see all columns and more than 10 rows
options( dplyr.width = Inf, dplyr.print_min = 100 )


## End of package load-------------


# Loading/Creating Data ---------------------------------------------------
## Load or create data -----------

# Set working directory. This is the path to your Rstudio folder for this 
# project. If you are in your correct Rstudio project then it should be:
getwd() 
##LC WD:"C:/Users/leticiacamacho/Documents/BTJR_MSProject/Rcode/Spotlights_Hab_2022/Jackrabbits"

# if so then:
workdir <- getwd() #creating working directory as an object so you can call it easier - without having to type it out

# set path to where you can access data in your computer. #
# Note that the path will be different in yours than other's.#
datapath <- "Z:/Common/Jackrabbits"


## Import km-level records:
# 
# rawrecs <- read.csv( file = paste0( datapath, 
#                                     'Records.csv'),  
#                      #replaces those values with NA
#                      na.strings = c(""," ","NA","Missing"), 
#                      # includes column heading
#                      header = TRUE )
#rawrec = 


      ##HELP:##############: HAVING TROUBLE CONNECTING TO THE Z DRIVE POSSIBLY ON MY END? - WILL LEAVE THIS CODE FOR JC OR OTHERS CONNECTED TO SERVER TO ACCESS DATA EASILT BUT WILL RUN WITH DATA LOCALLY ON MY COMPUTER FOR NOW

#Cannot open file Warning Message :
#In file(file, "rt") :
#cannot open file 'Z:/Common/Jackrabbits/BTJR_Aug22_Spotlight.SurveysRecords_Aug2022.csv': No such file or directory

## Ran Blow code instead until can ask JC about how to make this better;
#library(readr) # This package is used to import Excel files - unsure if needed now that I have updated the code Sept.5,2022
rawrecs<- read.csv("Records_Aug2022.csv", header = TRUE, na.strings = c(""," ","Missing"))
# includes column heading
#replaces those values with NA



#view
head( rawrecs ); dim(rawrecs)#head shows you first 6 columns and rows of dataset but ;dim(dataset name) shows all first 6 rows and all the columns in the called datasheet

#### Import site-level info

# siteraw <- read.csv( file = paste0( datapath, 
#                                     'SiteInfo.csv'),
#                      #replaces those values with NA
#                      na.strings = c(""," ","NA","Missing"), 
#                      # includes column heading
#                      header = TRUE )

##HELP:##############: Cannot open file Warning Message :
#In file(file, "rt") :
#cannot open file 'Z:/Common/JackrabbitsSiteInfo.csv': No such file or directory

## Ran Blow code instead until can ask JC about how to make this better;
#library(readr) # This package is used to import Excel files - unsure if needed now that I have updated the code Sept.5,2022

siteraw<- read.csv("Site_Aug2022.csv", header = TRUE, na.strings = c(""," ","Missing"))
# includes column heading
#replaces those values with NA

#view
head( siteraw );dim(siteraw)

##End of loading/creating data



# Cleaning Data ----------------------------------------------------------
#choose columns of interest from records:
head(rawrecs)


#create new dataframe to modify and select columns of interest
# in this case we focus on jackrabbits only
datadf <- rawrecs %>% 
  #simplify some column names also
  dplyr::select( Survey_ID, #Removed Survey_ID=1..Survey_ID , from this and following lines of code because it did not exist 
                 Km = Km_markingID,
                 JJ, JA, Junkn, CJ, CA, Cunkn ) #included cotton tail obs. too for AUG survey period
          ##Renaming column names to desired name within new dataframe 

#check
head( datadf )

#create new column for BTJR in dply using mutate function that is the total of all jackrabbit sightings#
# this means we are ignoring age for now. #
datadf <- datadf %>% 
  dplyr::mutate( Jackrabbits = JJ + JA + Junkn  ) 

#create new column for cotton tails in dply using mutate function that is the total of all jackrabbit sightings#
# this means we are ignoring age for now. #
datadf <- datadf %>% 
  dplyr::mutate( CottonTails = CJ + CA + Cunkn  )
#View datadf specs:
str(datadf)


#now clean site info
sitedf <- siteraw %>% 
  #import columns of interest and update desired names
  dplyr::select( Survey_ID, 
          Period = Crew_name, 
          Site, tempF = Start_temp.F., 
          WindKmHr = Start_wind.km.h.,
          Obv1 = Driver,
          Obv2 = Observer, 
          Night = Night_number,
          Date, Start_time )
#note we choose start wind and temperatures as measures
###This is because we are missing some of the end temps and wind speeds from some of the surveys in the field 

#view
head( sitedf )
str(sitedf ) ###Checking the specifics of the data in each given column 

#create a team column
sitedf <- sitedf %>% 
  mutate( Team =  ifelse( 
    startsWith( Obv1, "L" ),  "Leti",
    ifelse( 
      startsWith( Obv2, "L"), "Leti", "Zoe" ) ) )
###Doing this to see if there was a team/observers difference seen in the amount of BTJR observed per crew
###The reason we could not simply use period or dawn vs. dusk to look at this is becasue the dawn and dusk crews 
#switched period roles for one night during the 8 night survey period (the normal dawn crew surveyed during the dusk period and vise versa for dusk team)
#The Aug files will have more Leti Team nights because the first 2 nights we surveyed all four sites together as a crew of 4 technicians
# We all worked together for the first two nights with Leti and Zoe switching drivers half way through the night 


#check that new "team" column was added to site df
sitedf

#standardise site names:
sitedf$Site[ grep( "butte", sitedf$Site, ignore.case = TRUE, value = FALSE)] <- "Bigfoot"
sitedf$Site[ grep( "comb", sitedf$Site , ignore.case = TRUE, value = FALSE)] <- "Combined"
sitedf$Site[ grep( "Stand",sitedf$Site, ignore.case = TRUE, value = FALSE)] <- "Standifer" 

unique(sitedf$Site)# shows us what the unique site names are in the sitedf - it is all up to date and correct now

#combine date and time and format to lubridate:
sitedf$date <- lubridate::dmy_hm( paste( sitedf$Date, sitedf$Start_time),
                   tz = "MST" )###lubridate=package that helps organize/manipulate dates in datasets

##### HELP: WARNING MESSAGE: Warning message:
#All formats failed to parse. No formats found.
#Is this a concern ?
##HAVING TROUBLE:connecting or properly using lubridate package here


#extract hour
sitedf$hour <- lubridate::hour( sitedf$date )

#join data
alldf <- dplyr::left_join( datadf, sitedf, by = "Survey_ID" )
### joining both dataframes to one and using Survey_ID as the link between the two dfs
###to do this: using the dplyr function left_join and specifying by=survey_ID

#view
head( alldf ); dim( alldf )



# Visualizing Data --------------------------------------------------------
#combine and visualize data
alldf %>% 
  #group_by( Site, Km, Period ) %>% #Team - try instead of Period
  #group_by( Site, Km, Night ) %>% #Team - try instead of Km
  #group_by( Site, Km ) %>% 
  summarise( counts = mean(Jackrabbits) ) %>% 
  #summarise( counts = max( Jackrabbits ) ) %>% 
  #ggplot( ., aes( x = Km, y = counts, color = as.factor(Night) ) ) +
  ggplot( ., aes( x = Km, y = counts) ) +
    geom_line( size = 2 ) + 
    theme_bw( base_size = 17 ) +
    facet_wrap( ~Site )

#save relevant versions of this plot!!!! 
#LC used plots tab export to save an image of:
#countsPerKMbyNight-maxBTJR, - mean of this graph looks the same to me so i am not going to save/make an other plot with mean like the max one
#MeancountsPerKM by site 

#### HELP: NEED TO MAKE SOME MORE GRAPHS HERE 
#A. to compare dawn and dusk mean BTJR counts per km 
#B. #BTJR and Cottontails in incidental obs. - need to work with ZB to upload tracks ASAP 




#### look at observations vs detection variables:
#time:
ggplot(alldf, aes( x = hour, y = Jackrabbits ) ) +
  theme_classic( base_size = 17 ) +
  geom_point( size = 2 )
#####HELP: HAVING TROUBLE WITH LUBRIDATE PACKAGE EXTRACTING HOUR AND DATE
#above code has the error message i was getting when working with lubridate package

#NOTES FROM JUNE SURVEYS:
# Note that it is only 2 records at 21 and 23 that are higher
# than what is observed at other times. Not enough variability to 
# include time in detection


#Temp
ggplot(alldf, aes( x = tempF, y = Jackrabbits ) ) +
  theme_classic( base_size = 17 ) +
  geom_point( size = 2 )

#Wind
ggplot(alldf, aes( x = WindKmHr, y = Jackrabbits ) ) +
  theme_classic( base_size = 17 ) +
  geom_point( size = 2 )

#by night
ggplot(alldf, aes( x = Night, y = Jackrabbits ) ) +
  theme_classic( base_size = 17 ) +
  geom_point( size = 2 )


#pull per site
alldf %>% 
  group_by( Night, Site ) %>% 
  dplyr::summarise( counts = mean(Jackrabbits),
                    tempF = mean(tempF) ) %>% 
  ggplot(., aes( x = tempF, y = counts ) ) +
  theme_classic( base_size = 17 ) +
  geom_point( size = 2 )

alldf %>% 
  group_by( Night, Site ) %>% 
  dplyr::summarise( counts = mean(Jackrabbits),
                    wind = mean(WindKmHr) ) %>% 
  ggplot(., aes( x = wind, y = counts ) ) +
  theme_classic( base_size = 17 ) +
  geom_point( size = 2 )

#correlation among temperature predictors 



# Saving Data -------------------------------------------------------------
# save cleaned sitedf, datadf,and alldf
write.csv( x = sitedf, file = "AugSitedf.csv" )
write.csv( x = datadf, file = "AugDatadf.csv" )
write.csv( x = alldf, file = "AugAlldf.csv" )


# End of Script -----------------------------------------------------------


