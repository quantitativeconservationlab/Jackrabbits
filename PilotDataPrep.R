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
  dplyr::mutate( Jackrabbits = JJ + JA + Junkn  ) 

#now clean site info
sitedf <- siteraw %>% 
  #import columns of interest and update desired names
  dplyr::select( Survey_ID = ï..Survey_ID, 
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
str(sitedf )

#create a team column
sitedf <- sitedf %>% 
  mutate( Team =  ifelse( 
    startsWith( Obv1, "L" ),  "Leti",
    ifelse( 
      startsWith( Obv2, "L"), "Leti", "Zoe" ) ) )

#check
sitedf

#standardise site names:
sitedf$Site[ grep( "butte", sitedf$Site, ignore.case = TRUE, value = FALSE)] <- "Bigfoot"
sitedf$Site[ grep( "comb", sitedf$Site , ignore.case = TRUE, value = FALSE)] <- "Combined"
sitedf$Site[ grep( "Stand",sitedf$Site, ignore.case = TRUE, value = FALSE)] <- "Standifor"

#combine date and time and format to lubridate:
sitedf$date <- lubridate::dmy_hm( paste( sitedf$Date, sitedf$Start_time),
                   tz = "MST" )
#extract hour
sitedf$hour <- lubridate::hour( sitedf$date )

#join data
alldf <- dplyr::left_join( datadf, sitedf, by = "Survey_ID" )
#view
head( alldf ); dim( alldf )

#########################################################
########## visualize data ########################

#combine and visualise data
alldf %>% 
  #group_by( Site, Km, Period ) %>% 
  group_by( Site, Km, Night ) %>% 
  #group_by( Site, Km ) %>% 
  #summarise( counts = mean(Jackrabbits) ) %>% 
  summarise( counts = max( Jackrabbits ) ) %>% 
  ggplot( ., aes( x = Km, y = counts, color = as.factor(Night) ) ) +
  #ggplot( ., aes( x = Km, y = counts) ) +
    geom_line( size = 2 ) + 
    theme_bw( base_size = 17 ) +
    facet_wrap( ~Site )

#save relevant versions of this plot!!!! 


#### look at observations vs detection variables:
#time
ggplot(alldf, aes( x = hour, y = Jackrabbits ) ) +
  theme_classic( base_size = 17 ) +
  geom_point( size = 2 )
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

############### end of script #################################