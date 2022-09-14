############ Black-tailed Jackrabbit Project Data Prep Using 300mx300m Grid Cells from NLCD ########

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

#Using the quantitativeconservationlab / Jackrabbits GitHub Repository 
#to store scripts. URL:https://github.com/quantitativeconservationlab/Jackrabbits.git


# Pseudo Code --------------------------------------------------------------

#(Focus on statements, mathematical operations, conditions, iterations, exceptions)

# START: 
#This script is intended to link the GPS points of rabbits observed and all driving routes
#taken during the spotlight surveys conducting in the NCA in August to 300x300m grid cell 
#for the NCA that correlates to the NLCD habiat  layers for this study area

# INPUT:
#Common Z drive - Habitat - NLCD_new - NCA_raster_summaries_300m
#GPS locations of rabbits along all site routes and the incidental observation routes 
#NLCD habitat layers - eventually will be looking at habitat associations using both NLCD raster data
#and the Guard's polygon habitat data 


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
# set option to see all columns and more than 10 rows
options( dplyr.width = Inf, dplyr.print_min = 100 )#for visual aid - easier to view data
library( sf )#package for spatial data manipulation for vector data 
#install.packages("sf")
library( raster ) #for raster manipulation
#Can create raster layers objects using raster function within raster package:
library( rasterVis ) #for raster visualization
library(RColorBrewer)
library( terra ) #for raster vis and manipulation 
library(lubridate)#used to organize and format date and time data 

## End of package load-------------


# Load and Create Data ----------------------------------------------------

# Set working directory. This is the path to your Rstudio folder for this 
# project. If you are in your correct Rstudio project then it should be:
getwd() #"C:/Users/leticiacamacho/Documents/BTJR_MSProject/Rcode/Spotlights_Hab_2022/Jackrabbits"


#set data paths:

#Creating pathway to call the habitat NLCD data from the common drive
habpath <- "Z:/Common/QCLData/Habitat/" 
#Creating pathway to call the BTJR data from the common drive
datapath <- "Z:/Common/Jackrabbits/BTJR_Aug22_Spotlight.Surveys/" 
#creating pathway to call the 300x300m grid cells of NCA
rastpath<-"Z:/Common/QCLData/Habitat/NLCD_new/NCA_raster_summaries_300m/"



#Importing Data:

#import 300mx300m grid cell NCA raster from NLCD_new folder:
NCAgrid300m<- raster::raster( paste0(rastpath,
                                  "c20_agg.img")) 

#e need make sure all spatial data imported match. - will do this in later step 
#since this is a raster file and the other files we are currently working with
#are vector data we will fit the crs to match this raster 
plot(NCAgrid300m)

##Another way to check the crs of any spatial file:
sf::st_crs( NCAgrid300m )

#import NCA shapefile from habitat folder:
NCAboundary <- sf::st_read( paste0( habpath, 
          "NCA/GIS_NCA_IDARNGpgsSampling/BOPNCA_Boundary.shp"))
#Note that this is a polygon or vector file so we will need to make sure it 
#matches the above rater NCAboundary layer/file
#NCAboundary Projected CRS: NAD83 / UTM zone 11N + NAVD88 height
#Geometry type: POLYGON, 1 feature and 10 fields,
plot(NCAboundary)


#Importing Survey Routes:

#Importing Southern Routes:
route_S <- sf::st_read( paste0(datapath, 
      "BTJR_Aug22_Spotlights_shp/Bigfoot_Simco_transect.shp" ) )
#Note that this is a polygon or vector file so we will need to make sure it 
#matches the above rater NCAboundary layer/file
plot(route_S )
route_S

#Import Northern Route:
route_N <- sf::st_read( paste0(datapath, 
                               "BTJR_Aug22_Spotlights_shp/Standifer_Combined_transect.shp" ) )
plot(route_N )
route_N



# Preparing Data: ---------------------------------------------------------

#Selecting columns of interest for routes:
#Southern Routes:
route_S <- route_S %>% 
  #keep columns of interest only
  dplyr::select( ID, trksegID, lat, lon, geometry )

#view
route_S

#Northern Routes:
route_N <- route_N %>% 
  #keep columns of interest only
  dplyr::select( ID, trksegID, lat, lon, geometry )

#view
route_N


#convert geometrey from points to a line:

#southern route:
class(st_geometry(route_S)) #"sfc_POINT" "sfc"
#converting from point to line string:
route_S_line<-route_S$geometry %>% 
  st_coordinates("sfc") %>% 
  st_linestring()

route_S_line<-

#check
class(st_geometry(route_S_line))#"sfc_LINESTRING" "sfc"
plot(route_S_line)


#northern route:
class(st_geometry(route_N))#"sfc_POINT" "sfc"

route_N_line<-route_N$geometry %>% 
  st_coordinates("sfc") %>% 
  st_linestring()
#check
class(st_geometry(route_N_line))#"sfc_LINESTRING" "sfc"
plot(route_N_line)




#Match crs of all vector data to raster layer crs:

#identify what crs the data is currently in:
sf::st_crs(route_N)#"WGS 84"
sf::st_crs(route_S)#"WGS 84"
sf::st_crs(NCAboundary)#"WGS 84"
sf::st_crs(route_N_line)#NA
sf::st_crs(route_S_line)#"NA"

######### HELP: what do you do if the line i just created with my points from 
#the GPS now made it so that the 
####### WHY dont these lines now not have a crs???



#create a version that matches coordinates of the predictor raster:
NCAboundary <- sf:: st_transform( NCAboundary, st_crs( NCAgrid300m ) ) 
route_S_line<- sf:: st_transform( route_S_line, st_crs( route_S) ) 
route_N_line<- sf:: st_transform( route_S_line, st_crs( NCAgrid300m ) )
.#############HELP NOT WORKING FOR LINES BECASUE THEIR CRS=NA





############### TRYING THIS APPROACH?:

myfiles <- dir( paste0( datapath, rabpath ), 
                pattern = '\\.shp', full.names = TRUE )

# Read each shapefile and return a list of sf objects
listOfShp <- lapply(myfiles, st_read)
# Look to make sure they're all in the same CRS
unique(sapply(listOfShp, crs))








# Cleaning Data -----------------------------------------------------------


#Cleaning Rabbit Observation Data (GPS Locations):

#make sure all names are the same for obs. on site and incidentals. 
#create separate df containing columns of interest
#clean date and time format with lubridate package










