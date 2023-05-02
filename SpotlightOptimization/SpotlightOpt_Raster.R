# Background --------------------------------------------------------------
# 
# This script's purpose:
#        - Develope spatial objects from the rab loc data to be used in next steps (Spatial analysis)
#        - combine rabloc to NCA 300m raster 
#             - configuration of Jrab and Arab csv's to sf objects 
#        - plot routes 
#        - calculate duration and sampling efforts by assigning raster cellID to 
#           RabLoc data for 2022 spotlight surveys 



# Setup -------------------------------------------------------------------

## Setting up work space and load packages -----------

## Clean your work space : reset R environment. 
rm( list = ls() )


## Load packages relevant to this script:  -------------

library( tidyverse ) #package for easy data manipulation
library(sf) # used for manipulating simple features such as polygons,lines,points etc
library(raster) #used for manipulation and calling of raster objects 
library(terra) #also used for raster data 
library( tidyverse ) #package for easy data manipulation
# set option to see all columns and more than 10 rows
options( dplyr.width = Inf, dplyr.print_min = 100 )

# Setting Working Directory -----------------------------------------------


# Set working directory. =The path to your Rstudio folder for this 
# project (LC=on desktop Rscripts folder). 

getwd()

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

# August22:  -----------

# Asite <- read.csv(paste0(datapath, "Spotlights/Aug22/ARoute.csv"))
# #site level info for August 2022 spotlight surveys 

Arab <- read.csv(paste0(datapath, "Spotlights/Aug22/Arab.csv"))
#Rab locations for August 2022 spotlight surveys 

Arabsite <- read.csv(paste0(datapath, "Spotlights/Aug22/AJrabRoutes.csv"))
#combined site level info and rabbit locations for aug22 spotlight surveys 



# June22:  -----------

#Jsite<-read.csv(paste0(datapath, "Spotlights/June22/JRoutes.csv"))
#site level info for June 2022 spotlight surveys 

Jrab <- read.csv(paste0(datapath, "Spotlights/June22/Jrab.csv"))
#Rab locations for June 2022 spotlight surveys 
str(Jrab)

Jrabsite <- read.csv(paste0(datapath, "Spotlights/June22/JJrabRoutes.csv"))
#combined site level info and rabbit locations for aug22 spotlight surveys 



# Manipulating RabLoc to sf Objects: --------------------------------------


#Manipulating June df to include a sf geometry object/col:  -----------

#June22:  -----------
#proj <- st_crs('+proj=longlat +datum=WGS84')
proj <- 4326 #WGS84 code

#Convert a dataframe to a sf object:
Jrabsite_sf <- st_as_sf( Jrabsite, coords = c("lon", "lat"), 
                   crs = proj )# now will have a geometry column 

Jrabsite_sf
plot(Jrabsite_sf)
#Manipulating AUG df to include a sf geometry object/col:  -----------

#Convert a dataframe to a sf object:
Arabsite_sf <- st_as_sf(Arabsite, coords = c("lon", "lat"), 
                   crs = 4326)# now will have a geometry column 

#Check:
Arabsite
plot(Arabsite_sf)





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
all(st_is_valid(Arabsite_sf))#TRUE
all(is.na(st_dimension(Arabsite_sf)))#no missing geometry
#FALSE : is there NA=false for all ?

#June22:
all(st_is_valid(Jrabsite_sf))#TRUE
all(is.na(st_dimension(Jrabsite_sf)))#no missing geometry
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

sf::st_crs(Jrabsite_sf)#WGS84, EPSG:4326
sf::st_crs(Arabsite_sf)#WGS84, EPSG:4326

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
Jrabsite_rast<-sf::st_transform( Jrabsite_sf, st_crs( NCAgrid300m ) )
st_crs(Jrabsite_rast) == st_crs(NCAgrid300m)
#TRUE 

Arabsite_rast<-sf::st_transform( Arabsite_sf, st_crs( NCAgrid300m ) )
st_crs(Arabsite_rast) == st_crs(NCAgrid300m)
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
plot(st_geometry(Arabsite_rast), add=TRUE, col = "green")
plot(st_geometry(Jrabsite_rast), add=TRUE, col="black")

plot(st_geometry(Nroute_rast), add=TRUE, col="red")
plot(st_geometry(Sroute_rast), add=TRUE, col="blue")
#HAVING SOME TROUBLE WITH GETTING THE 300M RASTER TO SHOW IN BACKGROUND OF THIS PLOT
# ASK JC FROM RASTER VISUALIZATION TIPS 
# ALSO AS JC FOR HELP WITH CROPPING TO NCA BOUNDARY /ZOOM IN HERE TO SEE ROUTES AND PTS EASIER 


##################################################################


# Cropping raster to fit NCAboundary: -------------------------------------

#NCA300rast_crop<-raster::crop(x=NCAgrid300m, y=NCAb_rast)




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

#write csv /sf object dataframe for NCAb_rast, Jrabsite_rast, Arabsite_rast, 
#Nroute_rast, Sroute_rast:

#MAY NOT BE ABLE TO SAVE RASTER/SF OBJECTS AS A CSV HERE 
# NEED TO REMEMEBER HOW TO SAVE A SPATIAL OBJECT AND SAVE THESE TO BE USED IN 
# RASTER2 SCRIPT AND JUST CALL THEM WITH CORRECT CRS AND SUCH THAT IS CHECKED 
#IN THIS SCRIPT ABOVE 

## S3 method for class 'sf' data.frame': 
#st_write(obj, dsn, layer = NULL, ...)

st_write(NCAb_rast, 
         dsn = paste0(datapath, "Spotlights/Spatial.Data/NCAb_rast.shp"))


st_write(Jrabsite_rast, 
         dsn = paste0(datapath, "Spotlights/Spatial.Data/Jrabsite_rast.shp"))
#worked
st_write(Arabsite_rast, 
         dsn = paste0(datapath, "Spotlights/Spatial.Data/Arabsite_rast.shp"))


st_write(Nroute_rast, 
         dsn = paste0(datapath, "Spotlights/Spatial.Data/Nroute_rast.shp"))

st_write(Sroute_rast, 
         dsn = paste0(datapath, "Spotlights/Spatial.Data/Sroute_rast.shp"))

# LOAD WORKSPACE AND WORK THIS ABOVE SAVING STEP FOR SF/SPATIAL DATAFRAME 

## Save work space:   -----------
# saving all data to the path
save.image("SpotlightOpt_Raster.RData")




# loading the workspace
#load("SpotlightOpt_Raster.RData")

