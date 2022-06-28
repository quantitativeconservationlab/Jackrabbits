############################################################################
####    This script was developed by Jen Cruz to clean and visualize     ###
## black-tailed jackrabbit count data collected using spotlight surveys  ###
## conducted at dawn and dusk during 8 day period in June 2020 ###
############################################################################
##### Set up your workspace and load relevant packages -----------
# Clean your workspace to reset your R environment. #
rm( list = ls() )

# load packages relevant to this script:
library( tidyverse ) #easy data manipulation
# set option to see all columns and more than 10 rows
options( dplyr.width = Inf, dplyr.print_min = 100 )
library( lubridate ) #easy date and time manipulation
library( sf ) #for spatial data

## end of package load ###############

###################################################################
#### Load or create data -----------------------------------------
# Set working directory. This is the path to your Rstudio folder for this 
# project. If you are in your correct Rstudio project then it should be:
getwd()
# if so then:
workdir <- getwd()

# set path to where you can access data in your computer. #
# Note that the path will be different in yours than mine.#
datapath <- "Z:/Common/Jackrabbits/"

#import km-level records:
rawrecs <- read.csv( file = paste0( datapath, 
                    'Records.csv'),
                     #replaces those values with NA
                     na.strings = c(""," ","NA","Missing"), 
                     # includes column heading
                     header = TRUE )

#view
head( rawrecs ); dim(rawrecs)
#import site-level info
siteraw <- read.csv( file = paste0( datapath, 
                                    'SiteInfo.csv'),
                     #replaces those values with NA
                     na.strings = c(""," ","NA","Missing"), 
                     # includes column heading
                     header = TRUE )
#view
head( siteraw );dim(siteraw)

#######################################################################
######## cleaning data ###############################################

#choose columns of interest from records:
head(rawrecs)

#create new dataframe to modify and select columns of interest
# in this case we focus on jackrabbits only
datadf <- rawrecs %>% 
  #simplify some column names also
  dplyr::select( Survey_ID = ï..Survey_ID,
                 Km = Km_markingID,
                 JJ, JA, Junkn )

#check
head( datadf )

#create new column that is the total of all jackrabbit sightings#
# this means we are ignoring age for now. #
datadf <- datadf %>% 
  mutate( Jackrabbits = JJ + JA + Junkn  ) 

#now clean site info
sitedf <- siteraw %>% 
  #import columns of interest and update desired names
  select( Survey_ID = ï..Survey_ID, 
          Period = Crew_name, 
          Site, tempF = Start_temp.F., 
          WindKmHr = Start_wind.km.h.,
          Obv1 = Driver,
          Obv2 = Observer, 
          Night = Night_number,
          Date, Start_time )
#note we choose start wind and temperatures as measures
#view
head( sitedf )
#create a team column
sitedf <- sitedf %>% 
  mutate( Team =  ifelse( 
    startsWith( Obv1, "L" ),  "Leti",
    ifelse( 
      startsWith( Obv2, "L"), "Leti", "Zoe" ) ) )

#check
sitedf
#noticed that the last row is empty except for dawn in #
# one column so remove
sitedf <-sitedf[ !is.na( sitedf$Site ),]


############### end of script #################################