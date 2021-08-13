############################################################################
####    This script was developed by Jen Cruz to clean and visualize     ###
## black-tailed jackrabbit count data collected by Idaho National Guard ####
## using single visit spotlight surveys conducted (near new moon) once a ###
##  month from Apr-Sep of 2013 to 2020.                                  ###
### Surveys include other species. In 2020 methods were improved to collect #
# bearing and distance as well as details of nearby vegetation.           ##
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

# set path to where you can access the Access database in your computer. #
# Note that the path will be different in yours than mine.#
datapath <- "Z:/Common/Jackrabbits/"
#import relevant tables, which have been exported from Access database as #
# .csv files# 

#import records: calls file by pasting datapath to filename:
rawdata <- read.csv( file = paste0( datapath,"BTJR_alldata.csv" ),
                     #replaces those values with NA
                     na.strings = c(""," ","NA"), 
                     # includes column heading
                     header = TRUE )

#view
head( rawdata ); dim(rawdata)

#import track shapefile
tracks <- sf::st_read( paste0( datapath, "2020_BTJRRoutes_ALL.shp" ) )
#import observations as shapefile
rawlocs <- sf::st_read( paste0( datapath, "JackrabbitData_All.shp" ) )
#import NCA shapefile
NCA <- sf::st_read( "Z:/Common/QCLData/Habitat/NCA/GIS_NCA_IDARNGpgsSampling/BOPNCA_Boundary.shp")

#######################################################################
######## cleaning data ###############################################

# Check if shapefiles are in the same coordinate system
tracks
rawlocs
NCA
# yes. In NAD83 zone11N (so eastings and northings)

# rawlocs and rawdata may be the same 
colnames( rawlocs )
colnames( rawdata )
# possibly. Start with rawlocs
# remove extra columns:
all_df <- rawdata %>% 
  dplyr::select( TranName, Species, NumInd, Dated, ObTime, TempF, Temperatur,
    Habitat, dominantsh, nonshrub, isshrub, Distance, DistanceM, 
    Side, Bearing, Comments, Comment, 
    EASTING, NORTHING, ELEV) #, geometry )

#check
head( all_df ); dim( all_df )
str(all_df)
#combine similar columns
all_df <- all_df %>% rowwise() %>% 
  mutate( Temp = sum( TempF, Temperatur),
          Dist = sum(Distance, DistanceM ),
          Comments = paste( Comments, Comment, sep = " " ), 
          Hab = paste( Habitat, dominantsh, nonshrub, sep = " " ))%>% 
  dplyr::select( -TempF, -Temperatur, -Distance, -DistanceM,
          -Comment, -Habitat, -dominantsh, -nonshrub, -isshrub )
#check
head( all_df ); dim( all_df )

# NA are 1 I think
all_df$NumInd[ is.na(all_df$NumInd) ] <- 1
# Turn date into lubridate
all_df <- all_df %>% 
  mutate( Date = lubridate::dmy( Dated ),
          Year = lubridate::year( Date ),
          Month = lubridate::month( Date ) )

#what species were detected:
unique( all_df$Species )


#select rabbit records only, with location data
rabbit_df <- all_df %>% 
  #keep only jackrabbit records
  dplyr::filter( Species == "BTJR" ) %>% 
  filter( !is.na(EASTING) ) %>% 
  #turn into an sf dataframe using CRS from tracks
  st_as_sf( .,coords = c( 'NORTHING', 'EASTING'),
            crs = st_crs(tracks) )
  
#check
head( rabbit_df); dim( rabbit_df )

###########################################################
#### visualize data
#plot 
ggplot( rabbit_df ) +
  theme_classic( base_size = 17 ) +
  geom_sf() + 
   geom_sf( data = tracks ) +
   facet_wrap( ~Year )
 #geom_sf( data = NCA )

####################### end of script #########################################
