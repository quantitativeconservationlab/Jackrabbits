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
library( terra ) #for raster vis and manipulation 
## end of package load ###############

###################################################################
#### Load or create data -----------------------------------------
# Set working directory. This is the path to your Rstudio folder for this 
# project. If you are in your correct Rstudio project then it should be:
getwd()

#set path to habitat data
habpath <- "Z:/Common/QCLData/Habitat/"
datapath <- "Z:/Common/Jackrabbits/"
rabpath <- "BTJR_June22_Spotlights_shp/"
# Import .img file as a raster file using the "raster" package.
shrubs <- raster::raster( paste0( habpath, 
  "NLCD_new/TimeSeriesCover/Shrub/Shrub_2009_2020/rcmap_shrub_2020.img" ) )
#import invasive raster
invasives <- raster::raster( paste0( habpath, 
  "NLCD_new/TimeSeriesCover/Annual_Herbaceous/Annual_Herbaceous_2009_2020/rcmap_annual_herbaceous_2020.img" ) )

# Check the raster loaded
rasterVis::levelplot( shrubs )

#import routes
# routes <- sf::st_read( paste0( datapath,
#       "BTJR_KM_Markings/Km_Markings_2022/Km_Markings_2022.shp" ) )
routes <- sf::st_read( paste0( datapath,
        "BTJR_KM_Markings/Km_Markings_2022/Line_transects.shp" ) )

#import NCA shapefile from habitat folder:
NCA <- sf::st_read( paste0( habpath, 
      "NCA/GIS_NCA_IDARNGpgsSampling/BOPNCA_Boundary.shp") )
#note that they are all in different utms

#import rabbit observations
myfiles <- dir( paste0( datapath, rabpath ), 
                pattern = '\\.shp', full.names = TRUE )
fnames <- dir( paste0( datapath, rabpath ), 
               pattern = '\\.shp', full.names = FALSE )
#used <- sf::st_read( myfiles[1] )
# Read each shapefile and return a list of sf objects
listOfShp <- lapply(myfiles, st_read)

# Look to make sure they're all in the same CRS
unique(sapply(listOfShp, crs))

# Combine the list of sf objects into a single object
rabbits <- do.call( what = sf:::rbind.sf, args=listOfShp )
#now import cleaned site file
sitedf <- read.csv( "sitedf.csv")
head(sitedf)
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
# Note that transforming (projecting) raster data is fundamentally #
#different from transforming vector data. Vector data can be transformed #
#and back-transformed without loss in precision and without changes in #
#the values. This is not the case with raster data. In each transformation#
#the values for the new cells are estimated in some fashion. Therefore, #
#if you need to match raster and vector data for analysis, #
#you should generally transform the vector data. # 

#crop raster to buffered NCA if you have a raster object:
shrub_cropped <- raster::crop( shrubs, NCA_trans )
#crop invasive raster:
inv_cropped <- raster::crop( invasives, NCA_trans )
# Now that we have cropped it to the appropriate area it should be faster #
# to process eventhough we are still using raster #

#view shrubs
shrub_cropped
#Plot sagebrush
terra::plot( shrub_cropped, main = "Native shrubs (2020)" )
#or 
rasterVis::levelplot( shrub_cropped )
#Note that the max observed percentage of sagebrush ~70 %

#invasives
rasterVis::levelplot( inv_cropped )

###buffer lines to get habitat at the site level ######
site_buf <- routes %>% 
  sf::st_buffer( dist = 5000 )
#transform to extract hab
site_trans <- sf::st_transform( site_buf, st_crs( shrubs ) )
###play around with the distance value to whatever is #
#suitable #
site_shrub <- raster::extract( x = shrub_cropped, 
                   site_trans, 
           fun  = mean, na.rm = TRUE )
#extract invasive for the sites
site_inv <- raster::extract( x = inv_cropped, 
                 site_trans, 
               fun  = mean, na.rm = TRUE )
#combine to site shapefile
site_cover <- cbind( site_buf, site_shrub, site_inv )
site_cover 
################################################
#extracting habitat data for our kms:
# Start by creating a buffer aroundt the road tracks: 
km_buf <- routes %>% 
  sf::st_buffer( dist = 50 )

#now that we have a buffer we can sample points within it as #
#observed but not detected
rm_pnts <- sf::st_sample( km_buf, size = 300, type = "random",
                          by_polygon = TRUE )
#check
rm_pnts
#now work out which polygon they belong to
polids <- t(st_within(rm_pnts, km_buf))
#create random points in shrub crs for extracting
rm_trans <- as.data.frame(sf::st_transform( rm_pnts, st_crs( shrubs ) ) )
#now add row ids
rm_trans <- tibble::rowid_to_column( rm_trans, "rowID")
#check
head(rm_trans)
#Add site names
#starting with first site name to all cells:
rm_trans$Site <- routes$site_name[1]
#now use loop to correctly place other site names:
for( i in 2:length(routes$site_name) ){
rm_trans$Site[polids[[i]]] <- routes$site_name[i]
}
#repeat process with jackrabbit locations:
#select IDs to keep
unique(rabbits$ID)
#select which ones to keep:
keep <- c( "Junkn", "Ja", "JA", "Jj" )
#filter shapefile to keep only those of interest:
Jpoints <- rabbits %>% 
  dplyr::filter( ID %in% keep )
#check
head(Jpoints)
dim(Jpoints);dim(rabbits)
#work out which points belong to which sites
Jsites <- t(st_within(Jpoints, km_buf))
#add site names:
Jpoints$Site <- routes$site_name[1]
#now assign to site ids
for( i in 2:length(routes$site_name) ){
  Jpoints$Site[Jsites[[i]]] <- routes$site_name[i]
}
#check
head(Jpoints)
head(rm_trans)
#add column to determine use
Jpoints$use <- "yes"
rm_trans$use <- "no"
#convert rabbit points to raster crs:
j_trans <- sf::st_transform( Jpoints, st_crs( shrubs ) ) 
#Now combine detected and undetected points:
all_trans <- j_trans %>% 
  dplyr::select( rowID = OBJECTID,
                 Site, use, geometry )
#Note we ignored repeated sampling for now
all_trans <- bind_rows( all_trans, rm_trans )
#check
tail(all_trans); dim( all_trans )
#now that we have all points we can extract habitat
#define buffer around points
buf <- 100
shrub_vals <- raster::extract( x = shrub_cropped, 
                  all_trans, buffer = buf,
                fun  = mean, na.rm = TRUE )
#check
head( shrub_vals )
inv_vals <- raster::extract( x = inv_cropped, 
                               all_trans, buffer = buf,
                               fun  = mean, na.rm = TRUE )

#now we can combine to our points and transform back to 
# local projection
# I choose the NCA one (NAD83, zone11) but we can also go with WGS84
alldf <- all_trans %>% 
  dplyr::select( -rowID ) %>% 
  st_transform( st_crs(NCA ) )
#combine with cover measures
alldf <- cbind( alldf, shrub_vals, inv_vals )
#view
head( alldf )
str(alldf)
#check for missing vlaues 
sum(is.na(alldf$inv_vals))
sum(is.na(alldf$shrub_vals))
#check for autocorrelation among cover values
cor( cbind( alldf$shrub_vals, alldf$inv_vals) )

##########################################################
######### visualize data #############################
# visualize polygon data
ggplot( NCA ) +
  theme_classic( ) +
  geom_sf() +
  geom_sf( data = routes ) +
  #geom_sf( data = km_buf, color = "blue" )
  geom_sf( data = site_buf, color = "blue" )

#zooming to transects:
ggplot( km_buf ) +
  theme_classic( ) +
  geom_sf() +
  geom_sf( data = rabbits, color = "red" ) +
  geom_sf( data = rm_pnts, color = "black" ) +
  geom_sf(data = Jpoints, color = "blue" ) 

#plotting cover
ggplot( alldf ) +
  theme_classic( base_size = 17) +
  geom_histogram( aes(x = shrub_vals, fill = use) ) +
  facet_wrap( ~Site )
#invasive species                  
ggplot( alldf ) +
  theme_classic( base_size = 17) +
  geom_point( aes( x = inv_vals, y = shrub_vals, color = use ))

 ggplot( alldf ) +
   theme_classic( base_size = 17) +
   geom_histogram( aes(x = inv_vals, fill = use) ) +
   facet_wrap( ~Site )
 
########
###########################################################
### Save desired results                                  #
# we can save alldf as shapefile:
st_write( alldf, "allpoints.shp" )

#save workspace if in progress
save.image( 'HabResults.RData' )

####################### end of script #########################