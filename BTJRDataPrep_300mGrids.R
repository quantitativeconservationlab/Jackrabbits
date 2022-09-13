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
library( sf ) #package for spatial data manipulation 
library( raster ) #for raster manipulation
###Can create raster layers objects using raster function within raster package
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
habpath <- "Z:/Common/QCLData/Habitat/" #Creating pathway to call the habitat NLCD data from the common drive
datapath <- "Z:/Common/Jackrabbits/BTJR_Aug22Spotlight.Surveys/" #Creating pathway to call the BTJR data from the common drive
rastpath<-"Z:/Common/QCLData/Habitat/NLCD_new/NCA_raster_summaries_300m/"#creating pathway to call the 300x300m grid cells of NCA


#Importing Data:

#import 300mx300m grid cell NCA raster from NLCD_new folder:
NCAgrid300m<- sf::st_read( paste0(rastpath,
                                  "c20_agg")) ####################### MISSING SOMETHING HERE ####################
#note that they are all in different utms, so we need make sure all spatial data imported match. 
#NCAGRID300M Projected CRS: - CANT FIND FILE AS OF RIGHT NOW

#import NCA shapefile from habitat folder:
NCAboundary <- sf::st_read( paste0( habpath, 
                            "NCA/GIS_NCA_IDARNGpgsSampling/BOPNCA_Boundary.shp"))
#note that they are all in different utms, so we need make sure they all match. 
#NCAboundary Projected CRS: NAD83 / UTM zone 11N + NAVD88 height
#Geometry type: POLYGON, 1 feature and 10 fields,

#Import rabbit GPS data: 
dawnrabbits<-read.csv(file = paste0( datapath, "BTJR_Dawn_Aug22.csv"), na.strings = c(""," ","NA","Missing"), header = TRUE)
duskrabbits<-read.csv(file = paste0( datapath, "BTJR_Dusk_Aug22.csv"), na.strings = c(""," ","NA","Missing"), header = TRUE)














