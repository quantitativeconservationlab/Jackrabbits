## This script was developed by Leticia Camacho to research the jackrabbit 
##  spotlight survey 2022 data and determine optimal spotlight survey methodology



# Setup -------------------------------------------------------------------

## Set up your work space and load relevant packages -----------

## Clean your work space to reset your R environment. 
rm( list = ls() )

## Load packages relevant to this script:  -------------

## Some packages may not been installed - install them if needed
## Some packagees may not be needed for different parts of this script 
## Load the packages needed - when needed


#install.packages("oce")
library(oce)
library( tidyverse ) #package for easy data manipulation
#install.packages("tidyverse") 
library(ggplot2)
library(lubridate)
#library(terra)
#library(sp)
#install.packages("stars")
#library(stars)
#library(raster)
#library(sf)
#install.packages("fasterize")
#library(fasterize)
#install.packages(rgis)




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
datapath <- "Z:/Common/Jackrabbits/BTJR_Aug22_Spotlight.Surveys/" 

#creating pathway to call the 300x300m grid cells of NCA
rastpath<-"Z:/Common/QCLData/Habitat/NLCD_new/NCA_raster_summaries_300m/"