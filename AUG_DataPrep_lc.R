
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



# Pseudo Code --------------------------------------------------------------

#(Focus on statements, mathematical operations, conditions, iterations, exceptions)

# START:

# INPUT:
#         Using the quantitativeconservationlab / Jackrabbits GitHub Repository 
#          to store scripts. URL:https://github.com/quantitativeconservationlab/Jackrabbits.git
#


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
options( dplyr.width = Inf, dplyr.print_min = 100 )#for visual aid - easier to view data 


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
datapath <- "Z:/Common/Jackrabbits" #May not need this is new script since data is Github repository and accessable 


## Import km-level records:
# 
# rawrecs <- read.csv( file = paste0( datapath, 
#                                     'Records.csv'),  
#                      #replaces those values with NA
#                      na.strings = c(""," ","NA","Missing"), 
#                      # includes column heading
#                      header = TRUE )


      ##HELP:##############: HAVING TROUBLE CONNECTING TO THE Z DRIVE POSSIBLY ON MY END? - WILL LEAVE THIS CODE FOR JC OR OTHERS CONNECTED TO SERVER TO ACCESS DATA EASILT BUT WILL RUN WITH DATA LOCALLY ON MY COMPUTER FOR NOW

#Cannot open file Warning Message :
#In file(file, "rt") :
#cannot open file 'Z:/Common/Jackrabbits/BTJR_Aug22_Spotlight.SurveysRecords_Aug2022.csv': No such file or directory
## Ran Blow code instead until can ask JC about how to make this better;

rawrecs<- read.csv("Records_Aug2022.csv", header = TRUE, na.strings = c(""," ","Missing"))
#                    Includes column heading & replaces those values listed with NA
#Creates an object in your environment called rawrecs that accesses the Records csv data
#containing rabbit count data for each individual km marking along the site's route. 
#
#Each site is given two unique Survey_ID per night (1 for dawn crew & 1 for dusk crew)


#correlated to Site csv


#Records csv also contains count data (per km) of other species observed during each night's survey
#such as burrowing owls or kangaroo rats, etc. that were seen while conducting indv. night site level surveys
#Purpose of step: To access rabbit count data per site by km
#                 will be used to tie to geographical locations of each km marking along route
#                 to gain information about the abundance of rabbits in the NCA


#view
head( rawrecs ); dim(rawrecs)
#head shows you first 6 columns and rows of dataset but 
#;dim(dataset name) shows all first 6 rows and all the columns in the called datasheet




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

siteraw<- read.csv("Site_Aug2022.csv", header = TRUE, na.strings = c(""," ","Missing"))
#                    Includes column heading & replaces those values listed with NA
#Creates an object in your environment called siteraw that accesses the Site csv data
#containing survey specific details for each night. 
#Site csv also contains the corresponding Survey_ID corresponding to the Records csv./rawrecs. 
#This unique Survey_ID links the individual km (markings) surveyed each night (rawrecs) to
#the site name/location, date, time and other details of the survey. 
#Purpose of step: To link the rawrecs/rabbit count data per km along each night's route
#                 to the survey specific conditions such as weather, 
#                 technician roles, each route's specific start and end time,
#                 each night's specific start and end time, etc. 


#view new created siteraw object 
head( siteraw );dim(siteraw)

##End of loading/creating data



# Cleaning Data ----------------------------------------------------------

#choose columns of interest from records:
head(rawrecs)#shows you the first six rows of data in each column of the rawrecs object
#Purpose of step: Shows all the columns of the rawrecs object so we can choose columns of interest 
#and create a new datframe with them in it. 


#creating new df of selected data
datadf <- rawrecs %>% 
  #simplify some column names also:
  dplyr::select( Survey_ID, #Removed Survey_ID=1..Survey_ID , from this and following lines of code because it did not exist in the Aug data/csv's
                 Km = Km_markingID, #Renaming column names to desired name within new dataframe 
                 JJ, JA, Junkn, CJ, CA, Cunkn ) 
#Purpose of step: Create an new object in your environment that is a focused dataframe
#                 of jackrabbits and cottontail rabbits counts per km with their corresponding SurveyID.
#                 This dataframe is created to be modified and select columns of interest.


#check progess of df created:
head( datadf )
#make sure all variables/columsn of interest are properly included 


#Adding to the df: total jackrabbits per km
datadf <- datadf %>% 
  dplyr::mutate( Jackrabbits = JJ + JA + Junkn  )#mutate function creates a new column
#Purpose of step:create new column containing the total number of jackrabbits counted 
#                for each km in a survey in dplyr using mutate function. 
#                This means we are ignoring age for now, and pooling together 
#                all age classes to get a total jackrabbit count per km.


#Adding to the df: total cottontail rabbits per km
datadf <- datadf %>% 
  dplyr::mutate( CottonTails = CJ + CA + Cunkn  )#mutate function creates a new column
#Purpose of step:create new column containing the total number of cottontail rabbits counts 
#                per km in a survey in dplyr using mutate function. 
#                This means we are ignoring age for now, and pooling together 
#                all age classes to get a total cottontail rabbit count per km.


#View datadf specs:
str(datadf)#shows the structure of the object in question


#Cleaning site info:
sitedf <- siteraw %>% 
  #import columns of interest and update desired names
  dplyr::select( Survey_ID, #select function used to select columns of interest and can also be used to change the names of column headings within the pipe
          Period = Crew_name, 
          Site, tempF = Start_temp.F., 
          WindKmHr = Start_wind.km.h.,#note we choose start wind and temperatures as measures. This is because we are missing some of the end temps and wind speeds from some of the surveys in the field 
          Obv1 = Driver,
          Obv2 = Observer, 
          Night = Night_number,# Number between 1-8 (8 night total of repeat surveys per site - 4dawn,4dusk period)
          Date, Start_time )
#Purpose of step:Making a new df (saved to environment as an object) for the site 
#                data as we did for the records data above, selecting columns 
#                of interest and reformat them so they have more clear column names. 



#View datadf specs:
head( sitedf )#showing all columns with first 6 rows
str(sitedf ) #shows structure of sitedf 


#create a team column
sitedf <- sitedf %>% 
  mutate( Team =  ifelse( 
    startsWith( Obv1, "L" ),  "Leti",
    ifelse( 
      startsWith( Obv2, "L"), "Leti", "Zoe" ) ) )
#Purpose of step:To see if there was a team/observers difference seen in the 
#                amount of BTJR observed per crew. The reason we could not simply 
#                use period or dawn vs. dusk to look at this is because the dawn 
#                and dusk crews switched period roles in JUNE surveys for one night 
#                during the 8 night survey period (the normal dawn crew surveyed during 
#                the dusk period and vise versa for dusk team).

#                ******AUG surveys = we can just use period******


#check that new "team" column was added to site df *****AUG surveys = we can just use period******
sitedf #does show us that the site names could be cleaned up here and renamed below


#standardise site names:
sitedf$Site[ grep( "butte", sitedf$Site, ignore.case = TRUE, value = FALSE)] <- "Bigfoot"
sitedf$Site[ grep( "comb", sitedf$Site , ignore.case = TRUE, value = FALSE)] <- "Combined"
sitedf$Site[ grep( "Stand",sitedf$Site, ignore.case = TRUE, value = FALSE)] <- "Standifer" 

unique(sitedf$Site)# shows us what the unique site names are in the sitedf - it is all up to date and correct now
#Purpose of Step: Make sure that all the site names are spelled the same/properly
#                 and then checking that each site name is accurate/there are only 
#                 4 unique site names in sitedf.


#Manipulating date format:

#combine date and time and format to lubridate:
#lubridate=package that helps organize/manipulate/parse dates in datasets
sitedf$Date.Time <- lubridate::mdy_hm( paste( sitedf$Date, sitedf$Start_time),
                   tz = "MST" )
              #Combines Date and time columns (from sitedf) into 1 new column 
              #named Date.Time in desired format mdy_hm. 
#There is one NA date entry - noted, 9Sept2022


#Extract hour into sep. column
sitedf$Hour <- lubridate::hour( sitedf$date )
#Purpose of above steps: The data was entered in a less desirable format from datasheets. 
#                       Wanted to alter the format in R and isolate time (hour of the night)
#                       from date/time combo.


#join data
alldf <- dplyr::left_join( datadf, sitedf, by = "Survey_ID" )
#Purpose of step:joining both dataframes to one and using Survey_ID as the link 
#                between the two dfs to do this: using the dplyr function 
#               left_join and specifying by=survey_ID.

#view combined df with all rab.counts, site info.,weather,obsv.,period,time,date together
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

#by period
ggplot(alldf, aes( x = Period, y = Jackrabbits ) ) +
  theme_classic( base_size = 17 ) +
  geom_point( size = 2 )

#pull per site
alldf %>% 
  group_by( Night, Site ) %>% 
  dplyr::summarise( counts = mean(Jackrabbits),
                    tempF = mean(tempF) ) %>% 
  ggplot(., aes( x = tempF, y = counts ) ) +
  theme_classic( base_size = 17 ) +
  geom_point( size = 2 )+
  labs(title = "Mean Temp. Across Sites in Comparison to Jackrabbit Counts", 
       y="Jackrabbit Counts", x="Mean Temp.F")


alldf %>% 
  group_by( Night, Site ) %>% 
  dplyr::summarise( counts = mean(Jackrabbits),
                    wind = mean(WindKmHr) ) %>% 
  ggplot(., aes( x = wind, y = counts ) ) +
  theme_classic( base_size = 17 ) +
  geom_point( size = 2 )+
  labs(title = "Mean Wind Spead Across Sites vs. Jackrabbit Counts", 
       y="Jackrabbit Counts", x="Mean Wind Speed (km/Hr)")+
  theme(plot.title = element_text(colour = "blue", size = 12,face = "bold.italic"))

#correlation among temperature predictors 
cor(alldf$tempF,alldf$Jackrabbits)



# Saving Data -------------------------------------------------------------
# save cleaned sitedf, datadf,and alldf
write.csv( x = sitedf, file = "AugSitedf.csv" )
write.csv( x = datadf, file = "AugDatadf.csv" )
write.csv( x = alldf, file = "AugAlldf.csv" )


# End of Script -----------------------------------------------------------


