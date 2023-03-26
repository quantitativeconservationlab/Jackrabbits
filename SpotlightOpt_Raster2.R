# Background --------------------------------------------------------------
# This script in the 4th of 3 spotlight methodology optimization scripts 
# This script's purpose:

#       - create empty raster that matched specs of original raster (300m rast) 
#           ref chapter 6 for notes and steps:https://r.geocompx.org/raster-vector.html#rasterization

#       - add route shape files (polygons) to the blank raster (~"overlap")

#       - calc how much of the route lines fall within each grid cell (terra::extract)(rasterization)




# Setup -------------------------------------------------------------------

## Set up your work space and load relevant packages -----------

## Clean your work space to reset your R environment:  -----------
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

# August22:  -----------

#Asite <- read.csv(paste0(datapath, "Spotlights/Aug22/ARoute.csv"))
#site level info for August 2022 spotlight surveys 

#Arab <- read.csv(paste0(datapath, "Spotlights/Aug22/Arab.csv"))
#Rab locations for August 2022 spotlight surveys 

Arabsite <- read.csv(paste0(datapath, "Spotlights/Aug22/AJrabRoutes.csv"))
#combined site level info and rabbit locations for aug22 spotlight surveys 

#check:
str(Arab)
unique(Arab$Rab.Obv)#only jackrbabits 
str(Arabsite)


# June22:  -----------

#Jsite<-read.csv(paste0(datapath, "Spotlights/June22/JRoutes.csv"))
#site level info for June 2022 spotlight surveys 

#Jrab <- read.csv(paste0(datapath, "Spotlights/June22/Jrab.csv"))
#Rab locations for June 2022 spotlight surveys 

Jrabsite <- read.csv(paste0(datapath, "Spotlights/June22/JJrabRoutes.csv"))
#combined site level info and rabbit locations for aug22 spotlight surveys 
#check:
str(Jrab)
unique(Jrab$Rab.Obv)#jackrabbit , unknown, and cottontails
str(Jrabsite)#only jackrab. - dont use jrab



##Create sfobjects out of the june and aug rab data:  -----------

#June22:  -----------
#Manipulating June df to include a sf geometry object/col:  -----------
#Convert a dataframe to a sf object:
Jrab_sf <- st_as_sf( Jrabsite, coords = c("lon", "lat"), 
                     crs = 4326 )#WGS84
# now will have a geometry column 
#check:
Jrab_sf

#Aug22:  -----------
#Manipulating AUG df to include a sf geometry object/col:  -----------
#Convert a dataframe to a sf object:
Arab_sf <- st_as_sf(Arabsite, coords = c("lon", "lat"), 
                    crs = 4326)#WGS84
# now will have a geometry column 
#Check:
Arab_sf





# Importing Routes:  -----------


N.Route <-sf::st_read(paste0(datapath,
                             "Shapefiles/Aug22_Spotlights_shp/N_RouteLine.shp"))
plot(N.Route)

S.Route <-sf::st_read(paste0(datapath,
                             "Shapefiles/Aug22_Spotlights_shp/S_RouteLine.shp"))
plot(S.Route)
#looks good 



## Importing Raster Data:  -----------

#import 300mx300m grid cell NCA raster from NLCD_new folder:  -----------
NCAgrid300m<- raster::raster( paste0(rastpath,
                                     "c20_agg.img")) 

#import NCA shapefile from habitat folder:  -----------
NCAboundary <- sf::st_read( paste0( habpath, 
                                    "NCA/GIS_NCA_IDARNGpgsSampling/BOPNCA_Boundary.shp"))






# Evaluating Raster Layer  ------------------------------------------------

# figure out what the 300m NCA habitat raster map specs are so that we can 
# create a mathing blank raster:


##Checking Extents :  -----------
terra::ext(NCAgrid300m)
#SpatExtent : -1660905, -1555905, 2357685, 2459385 (xmin, xmax, ymin, ymax)

##Checking Resolutions :  -----------
res(NCAgrid300m)#300 300
ncol(NCAgrid300m)#350
NCAgrid300m



# Creating a Blank Raster to Match Original  ------------------------------
rast_template <- rast(ext(NCAgrid300m),resolution = 300, 
                      crs = st_crs(NCAgrid300m)$wkt)
plot(rast_template)
plot(NCAgrid300m)



# Matching CRS's, geometries and Projection Specs  -------------------------------------
##Checking if geometry of spatial objects are valid :  -----------
#Aug22 :  -----------
all(st_is_valid(Arab_sf))#TRUE
all(is.na(st_dimension(Arab_sf)))#no missing geometry
#FALSE : is there NA=false for all 
#June22 :  -----------
all(st_is_valid(Jrab_sf))#TRUE
all(is.na(st_dimension(Arab_sf)))#no missing geometry
#FALSE : is there NA=false for all 
#NCA boundary :  -----------
all(sf:: st_is_valid(NCAboundary))#TRUE
all(is.na(st_dimension(NCAboundary)))#no missing geometry
#FALSE: is there NA=false for all
#Routes :  -----------
all(sf:: st_is_valid(N.Route))#TRUE
all(is.na(st_dimension(N.Route)))#no missing geometry
#FALSE: is there NA=false for all
all(sf:: st_is_valid(S.Route))#TRUE
all(is.na(st_dimension(S.Route)))#no missing geometry
#FALSE: is there NA=false for all


#Checking CRS of all objects :  -----------
sf::st_crs(Jrab_sf)#WGS84, EPSG:4326
sf::st_crs(Arab_sf)#WGS84, EPSG:4326

sf::st_crs(NCAboundary)#NAD83 / UTM zone 11N + NAVD88 height

sf::st_crs(NCAgrid300m)#+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs 

sf::st_crs(rast_template)#PROJCRS["unknown",BASEGEOGCRS["GCS_unknown", DATUM["World Geodetic System 1984",

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


crs(NCAgrid300m)#+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0+datum=WGS84 +units=m +no_defs
crs(rast_template) <- "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0
+datum=WGS84 +units=m +no_defs"

crs(rast_template)
####################################################################
# I AM UNSURE IF THESE ARE MATCHING EACHOTHER OR NOT HERE
####################################################################


# Manipulating Blank Raster  ----------------------------------------------
#Plotting Raster and Vector objects together to check :  -----------
plot(rast_template)
plot(st_geometry(NCAb_rast), add=TRUE)
#plot(st_geometry(Arab_rast), add=TRUE, col = "green")
#plot(st_geometry(Jrab_rast), add=TRUE, col="black")

plot(st_geometry(Nroute_rast), add=TRUE, col="red")
plot(st_geometry(Sroute_rast), add=TRUE, col="blue")







# Analyzing Spatial Data --------------------------------------------------
