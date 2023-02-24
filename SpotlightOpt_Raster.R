# Background --------------------------------------------------------------
# This script in the 3rd of 3 spotlight methodology optimization scripts 
# This script's purpose:
#        - combine rabloc to NCA 300m raster 
#        - plot routes 
#        - calculate duration and sampling efforts 
#        







# Setup -------------------------------------------------------------------

## Set up your work space and load relevant packages -----------

## Clean your work space to reset your R environment. 
rm( list = ls() )



## Load packages relevant to this script:  -------------

## Some packages may not been installed - install them if needed
## Some packagees may not be needed for different parts of this script 
## Load the packages needed - when needed


#install.packages("oce")
library(oce)# used for moon phase information
library( tidyverse ) #package for easy data manipulation
#install.packages("tidyverse") 
library(ggplot2)
library(lubridate)
library(tidyr)
library(dplyr)
library(sf)
library(raster)



# Setting Working Directory -----------------------------------------------


# Set working directory. This is the path to your Rstudio folder for this 
# project. If you are in your correct Rstudio project then it should be:

getwd()#"C:/Users/leticiacamacho/Documents/BTJR_MSProject/Rcode/Spotlights_Hab_2022/Jackrabbits"

# if so then:
workdir <- getwd() 
#creating working directory as an object so you can call it easier 
#     - without having to type it out or re-run each time.






# Load and Create Data ----------------------------------------------------
# Set data paths:  -----------

#Creating pathway to call the habitat NLCD data from the common drive
habpath <- "Z:/Common/QCLData/Habitat/" 
#Creating pathway to call the BTJR data from the common drive
datapath <- "Z:/Common/Jackrabbits/Data/Spotlights/" 

#creating pathway to call the 300x300m grid cells of NCA
rastpath<-"Z:/Common/QCLData/Habitat/NLCD_new/NCA_raster_summaries_300m/"





# Importing Data: ---------------------------------------------------------

## Import Rabbit Data:  -----------

# August22:

Asite<- read.csv(paste0(datapath, "Aug22/BigRabdf_extended.csv"))

ARabLoc<-
  #st_read(paste0(datapath, Arab_sf.txt))
  #read.csv(paste0(datapath, "Arab_sf.csv"))################### in SpotlightOpt_RabLoc.R - Arab_sf = in correct format but having trouble calling it in to this script 
  Arab_sf
# MAYBE SAVE NOT AS AN SF OBJECT IN LAST SCRIPT AND CHANGE IT TO AN SF OBJECT HERE IN THIS SCRIPT 
# - DONT CHANGE CREATED A&Jrabloc DFS ANS SF PRIOR TO CALLING IT HERE 
# June22:






##############################################################################

N.Route <-sf::st_read(paste0(datapath,
                             "Shapefiles/Aug22_Spotlights_shp/N_RouteLine.shp"))
plot(S.Route)

S.Route <-sf::st_read(paste0(datapath,
                             "Shapefiles/Aug22_Spotlights_shp/S_RouteLine.shp"))