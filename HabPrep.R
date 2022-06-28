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
library( sf ) #for polygons and point data
library( raster ) #for raster manipulation
library( rasterVis ) #for raster visualization
## end of package load ###############

###################################################################
#### Load or create data -----------------------------------------
# Set working directory. This is the path to your Rstudio folder for this 
# project. If you are in your correct Rstudio project then it should be:
getwd()

#set path to habitat data
habpath <- "Z:/Common/QCLData/Habitat/"

# Import .img file as a raster file using the "raster" package.
shrubs <- raster::raster( paste0( habpath, 
  "NLCD_new/Shrub/Shrub_2009_2020/rcmap_shrub_2020.img" ) )
#import invasive raster
invasives <- raster::raster( paste0( habpath, 
  "NLCD_new/Annual_Herbaceous/Annual_Herbaceous_2009_2020/rcmap_annual_herbaceous_2020.img" ) )

#import routes
routes <- sf::st_read( paste0( datapath,
      "BTJR_KM_Markings/Km_Markings_2022/Km_Markings_2022.shp" ) )

#import NCA shapefile from habitat folder:
NCA <- sf::st_read( paste0( habpath, 
      "NCA/GIS_NCA_IDARNGpgsSampling/BOPNCA_Boundary.shp") )
#note that they are all in different utms


# Check the data by making a basic plot.
rasterVis::levelplot( shrubs )

#######################################################################
######## preparing data ###############################################

#get coordinates from shapefile
crstracks <- sf::st_crs( NCA )

# checking outline of NCA
sf::st_bbox( NCA )
# We define available habitat as area of NCA with a small buffer #
# around it and draw points from it #
#create a buffer around the NCA using outline of NCA and sf package:
NCA_buf <- NCA %>% sf::st_buffer( dist =5e3 )
#create a version that matches coordinates of the predictor raster:
NCA_trans <- sf::st_transform( NCA_buf, st_crs( shrubs ) ) 
#compare outline of trasformed polygon:
sf::st_bbox( NCA_trans )

#crop raster to buffered NCA if you have a raster object:
shrub_cropped <- raster::crop( shrubs, NCA_trans )
#crop invasive raster:
inv_cropped <- raster::crop( invasives, NCA_trans )

#extracting habitat data for our kms:


###########################################################
### Save desired results                                  #
# we can save the movement model results
#save workspace if in progress
save.image( 'HabResults.RData' )

####################### end of script #########################