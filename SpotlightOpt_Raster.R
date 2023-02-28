# Background --------------------------------------------------------------
# This script in the 3rd of 3 spotlight methodology optimization scripts 
# This script's purpose:
#        - combine rabloc to NCA 300m raster 
#             - configuration of Jrab and Arab csv's to sf objects 
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
# library(ggplot2)
# library(lubridate)
# library(tidyr)
# library(dplyr)
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
datapath <- "Z:/Common/Jackrabbits/Data/" 

#creating pathway to call the 300x300m grid cells of NCA
rastpath<-"Z:/Common/QCLData/Habitat/NLCD_new/NCA_raster_summaries_300m/"





# Importing Data: ---------------------------------------------------------

## Import Rabbit Data:  -----------

# August22:

Asite <- read.csv(paste0(datapath, "Spotlights/Aug22/BigRabdf_extended.csv"))
#site level info for August 2022 spotlight surveys 
Arab <- read.csv(paste0(datapath, "Spotlights/Aug22/Arab.csv"))
#Rab locations for August 2022 spotlight surveys 
  
# June22:

Jsite<-read.csv(paste0(datapath, "Spotlights/June22/Site_June22.csv"))
#site level info for June 2022 spotlight surveys 

Jrab <- read.csv(paste0(datapath, "Spotlights/June22/Jrab.csv"))
#Rab locations for June 2022 spotlight surveys 

str(Jrab)
#NOTICING MISTAKE : *** SOMEHOW JRAB LAT/LON GOT COL NAMES SWITCHED
#NEED TO FIX:
names(Jrab)[names(Jrab)=="lat"] <-'long'
names(Jrab)[names(Jrab)=="lon"] <-'lat'

View(Jrab)#Worked 
#now change long to lon to match Arab df:
names(Jrab)[names(Jrab)=="long"]<-'lon'
#Worked, ready to proceed 


# Manipulating RabLoc to sf Objects: --------------------------------------


#Manipulating June df to include a sf geometry object/col:  -----------

#June22:  -----------
#proj <- st_crs('+proj=longlat +datum=WGS84')
proj <- 4326
# Jlon <- Jrab$lon
# Jlat <- Jrab$lat
# 
# st_multipoint(cbind(Jlon, Jlat)) %>% st_sfc(., crs = proj)
# 

# #plot points:
# plot(st_multipoint(cbind(Jlon, Jlat)) %>% 
#        st_sfc(., crs = proj))#worked 


#Convert a dataframe to a sf object:
Jrab_sf <- st_as_sf( Jrab, coords = c("lon", "lat"), 
                   crs = proj )# now will have a geometry column 

Jrab_sf
#Manipulating AUG df to include a sf geometry object/col:  -----------

# Alon <- Arab$lon
# Alat <- Arab$lat
# st_multipoint(cbind(Alon, Alat)) %>% st_sfc(., crs = proj)
# 
# 
# #plot points:
# plot(st_multipoint(cbind(Alon, Alat)) %>% 
#        st_sfc(., crs = proj))#worked 
# 

#Convert a dataframe to a sf object:
Arab_sf <- st_as_sf(Arab, coords = c("lon", "lat"), 
                   crs = 4326)# now will have a geometry column 

#Check:
Arab_sf






# Importing Routes: -------------------------------------------------------


N.Route <-sf::st_read(paste0(datapath,
                             "Shapefiles/Aug22_Spotlights_shp/N_RouteLine.shp"))
plot(N.Route)

S.Route <-sf::st_read(paste0(datapath,
                             "Shapefiles/Aug22_Spotlights_shp/S_RouteLine.shp"))
plot(S.Route)




## Importing Raster Data:  -----------

#import 300mx300m grid cell NCA raster from NLCD_new folder:  -----------
NCAgrid300m<- raster::raster( paste0(rastpath,
                                     "c20_agg.img")) 

#import NCA shapefile from habitat folder:  -----------
NCAboundary <- sf::st_read( paste0( habpath, 
                                    "NCA/GIS_NCA_IDARNGpgsSampling/BOPNCA_Boundary.shp"))









# Checking Geometries and Matching CRS's ----------------------------------


##Checking if geometry of spatial objects are valid :  -----------

#Aug22:
all(st_is_valid(Arab_sf))#TRUE
all(is.na(st_dimension(Arab_sf)))#no missing geometry
#FALSE : is there NA=false for all ?

#June22:
all(st_is_valid(Jrab_sf))#TRUE
all(is.na(st_dimension(Arab_sf)))#no missing geometry
#FALSE : is there NA=false for all ?


#NCA boundary:
all(sf:: st_is_valid(NCAboundary))#TRUE
all(is.na(st_dimension(NCAboundary)))#no missing geometry
#FALSE: is there NA=false for all


#Routes:
all(sf:: st_is_valid(N.Route))#TRUE
all(is.na(st_dimension(N.Route)))#no missing geometry
#FALSE: is there NA=false for all

all(sf:: st_is_valid(S.Route))#TRUE
all(is.na(st_dimension(S.Route)))#no missing geometry
#FALSE: is there NA=false for all





#Checking CRS of all objects :  -----------
sf::st_is_longlat(NCAgrid300m)#FALSE

sf::st_crs(Jrab_sf)#WGS84, EPSG:4326
sf::st_crs(Arab_sf)#WGS84, EPSG:4326

sf::st_crs(NCAboundary)#NAD83 / UTM zone 11N + NAVD88 height

sf::st_crs(NCAgrid300m)#+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs 

sf::st_crs(N.Route)#WGS84
sf::st_crs(S.Route)#WGS84




#Transforming crs to match raster :  -----------


#create a version that matches coordinates of the predictor raster:

NCAb_rast <- sf::st_transform( NCAboundary, st_crs( NCAgrid300m ) )#worked
#NCA boundary crs now same as NCSgrid300m raster 
#Check that this is correct:
st_crs(NCAb_rast) == st_crs(NCAgrid300m)
#TRUE - so this is good to go

#Check to see that the rest of the objects are in the same crs as raster:
Jrab_rast<-sf::st_transform( Jrab_sf, st_crs( NCAgrid300m ) )
st_crs(Jrab_rast) == st_crs(NCAgrid300m)
#TRUE 

Arab_rast<-sf::st_transform( Arab_sf, st_crs( NCAgrid300m ) )
st_crs(Arab_rast) == st_crs(NCAgrid300m)
#TRUE


Nroute_rast<-sf::st_transform( N.Route, st_crs( NCAgrid300m ) )
st_crs(Nroute_rast) == st_crs(NCAgrid300m)
#TRUE 

Sroute_rast<-sf::st_transform( S.Route, st_crs( NCAgrid300m ) )
st_crs(Sroute_rast) == st_crs(NCAgrid300m)
#TRUE 







#Plotting Raster and Vector objects together to check :  -----------
plot(NCAgrid300m)
plot(st_geometry(NCAb_rast), add=TRUE)
plot(st_geometry(Arab_rast), add=TRUE, col = "green")
plot(st_geometry(Jrab_rast), add=TRUE, col="black")

plot(st_geometry(Nroute_rast), add=TRUE, col="red")
plot(st_geometry(Sroute_rast), add=TRUE, col="blue")
#HAVING SOME TROUBLE WITH GETTING THE 300M RASTER TO SHOW IN BACKGROUND OF THIS PLOT
# ASK JC FROM RASTER VISUALIZATION TIPS 
# ALSO AS JC FOR HELP WITH CROPPING TO NCA BOUNDARY /ZOOM IN HERE TO SEE ROUTES AND PTS EASIER 




# Cropping raster to fit NCAboundary: -------------------------------------

#NCA300rast_crop<-raster::crop(x=NCAgrid300m, y=NCAb_rast)



####################################################################




# Checking extent and cropping raster to fit data more closely ------------

##Checking Extents :  -----------
st_bbox(Arab_rast)
#    xmin     ymin     xmax     ymax 
# -1626979  2409339 -1601886  2441791 

st_bbox(Jrab_rast)
# xmin     ymin     xmax     ymax 
# -1627120  2408216 -1599687  2442709 

st_bbox(NCAb_rast)
# xmin     ymin     xmax     ymax 
# -1650957  2367880 -1565864  2449339

terra::ext(NCAgrid300m)
#SpatExtent : -1660905, -1555905, 2357685, 2459385 (xmin, xmax, ymin, ymax)


#########################################################################
#WHAT DO I DO WITH THIS INFORMATION - HOW TO I CROP TO BETTER FIT THE 
#JACKRABBIT DATA?

#######################################################################


##Checking Resolutions :  -----------
res(NCAgrid300m)#300 300
ncol(NCAgrid300m)#350
NCAgrid300m


###################################################################
# NEED TO CROP TO MATCH NCA BOUNDARY SO IT IS MORE ZOOMED IN 
# NEED TO FIGURE OUT HOW WE WANT TO PROCEED WITH MISSING JUNE22 GPS EXACT TIMES 
# - CONFIGURING MST.TIME, DURATION , ETC. 
# 



# THEN CAN PULL FROM LINES 546-603 IN 300mGrid_BTJRAbundanceCounts.R to :
#
#  WORKING TOWARDS ANALYSIS STEPS - CALCULATING SAMPLING EFFORT
# AND ASSIGNING TRANSECTING GRID CELLS TO POLYGON ROUTE LINES


















##################################################################
####### VISUALITATIONS AND MOON PHASE ASSESSMENT #########
#### WILL PULL FROM LINE 735-844 in 300mGrid_BTJRAbundanceCounts.R ######


# Calculating Moon Phase: -------------------------------------------------

# t<-Jrab_sf$Start_MST.time
# f<-oce::moonAngle(t=t, 
#                   longitude = -63.6,
#                   latitude = 44.65)$illuminatedFraction
# 
# plot(t, f, xlab="Day of 2022", ylab="Moon Fraction")
# grid()




#############################################################################






# Saving Data -------------------------------------------------------------
## Save csv's:   -----------

# Save cleaned csv:
#write.csv( x = ... , file = "...." )





## Save work space:   -----------
# saving all data to the path
save.image("SpotlightOpt_Raster.RData")




# loading the workspace
#load("SpotlightOpt_Raster.RData")

