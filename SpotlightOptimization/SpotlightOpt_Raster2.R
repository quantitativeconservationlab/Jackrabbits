# Background --------------------------------------------------------------
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

## Set data paths:  -----------

#Creating pathway to call the habitat NLCD data from the common drive
habpath <- "Z:/Common/QCLData/Habitat/" 
#Creating pathway to call the BTJR data from the common drive
datapath <- "Z:/Common/Jackrabbits/Data/" 

#creating pathway to call the 300x300m grid cells of NCA
rastpath<-"Z:/Common/QCLData/Habitat/NLCD_new/NCA_raster_summaries_300m/"



# Importing Data: ---------------------------------------------------------

## Import Rabbit Data:  -----------

Arabsite <- read.csv(paste0(datapath, "/Spotlights/Aug22/AJrabRoutes.csv"))
#combined site level info and rabbit locations for aug22 spotlight surveys 

#check:
str(Arabsite)
#several NAs in several columns 
#need to asses this

unique(Arabsite$Rab.Obv)#only jackrbabits 

## Aug22 spatial data: 
Arabsite_rast <- st_read(paste0(datapath,
                                "Spotlights/Spatial.Data/Arabsite_rast.shp"))
#importing jrabbit sf objects created in SpotlightOpt_Raster.R
#validated geometry and matching crs to NCAgrid300m raster in SpotlightOpt_Raster


# June22:  -----------

Jrabsite <- read.csv(paste0(datapath, "Spotlights/June22/JJrabRoutes.csv"))
#combined site level info and rabbit locations for aug22 spotlight surveys 
#check:
str(Jrabsite)
unique(Jrabsite$Rab.Obv)#jackrabbit only
head(Jrabsite)
#june location data of each observed jackrab was taken using tablets and the 
#exact time of each point was not collected using these devices instead of 
#gps units - switched to gps units in aug22, = can calc. hour, duration, etc. for aug22
summary(Jrabsite)


## June22 spatial data: 
#June has fewer col. than Aug22 becasue of tablet use = exact time stamps lacking 
Jrabsite_rast <- st_read(paste0(datapath,
                                "Spotlights/Spatial.Data/Jrabsite_rast.shp"))
#importing jrabbit sf objects created in SpotlightOpt_Raster.R
#validated geometry and matching crs to NCAgrid300m raster in SpotlightOpt_Raster


# Importing Routes:  -----------

## Routes Spatial Data:
Nroute_rast <- st_read(paste0(datapath,
                              "Spotlights/Spatial.Data/Nroute_rast.shp"))


Sroute_rast <- st_read(paste0(datapath, 
                              "Spotlights/Spatial.Data/Sroute_rast.shp"))
#BTJR 2022 spotlight survey routes sf objects created in SpotlightOpt_Raster.R
#validated geometry and matching crs to NCAgrid300m raster in SpotlightOpt_Raster


#importing route data: (MAY NOT BE NEEDED:)

##use these if need single line shape file from arcgis:
#N.Route <-sf::st_read(paste0(datapath,"Shapefiles/Aug22_Spotlights_shp/N_RouteLine.shp"))

#S.Route <-sf::st_read(paste0(datapath,"Shapefiles/Aug22_Spotlights_shp/S_RouteLine.shp"))
#looks good - but will need to match crs to raster tho is use these :

## Converting CRS of routes to match raster:
#Nroute_rast<-sf::st_transform( N.Route, st_crs( NCAgrid300m ) )
# st_crs(Nroute_rast) == st_crs(NCAgrid300m)
# #TRUE 
# 
# Sroute_rast<-sf::st_transform( S.Route, st_crs( NCAgrid300m ) )
# st_crs(Sroute_rast) == st_crs(NCAgrid300m)
# #TRUE 





## Importing Raster Data:  -----------

#import 300mx300m grid cell NCA raster from NLCD_new folder:  -----------
NCAgrid300m<- raster::raster( paste0(rastpath,
                                     "c20_agg.img")) 


#import NCA shapefile from habitat folder:  -----------
NCAb_rast <-sf::st_read(paste0(datapath,
                               "Spotlights/Spatial.Data/NCAb_rast.shp"))
#NCA Boundary line sf object created in SpotlightOpt_Raster.R
#validated geometry and matching crs to NCAgrid300m raster in SpotlightOpt_Raster






# Evaluating Raster Layer  ------------------------------------------------

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
#Check:
plot(rast_template)
plot(NCAgrid300m)


#Making sure template raster crs matches with raster NCAgrid300m:
crs(NCAgrid300m)#+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0+datum=WGS84 +units=m +no_defs

st_crs(rast_template) == st_crs(NCAgrid300m)
#TRUE

##Checking Extents :  -----------
terra::ext(rast_template)
#SpatExtent : -1660905, -1555905, 2357685, 2459385 (xmin, xmax, ymin, ymax)
#matches

##Checking Resolutions :  -----------
res(rast_template)#300 300
ncol(rast_template)#350
#matches




# Manipulating Blank Raster  ----------------------------------------------

#Creating cropped raster so can zoom in move on NCA and actually see routes
rast_template_crop <- terra::crop(rast_template, NCAb_rast)


#Plotting Raster and Vector objects together to check :  -----------
plot(rast_template_crop)
plot(st_geometry(NCAb_rast), add=TRUE)
plot(st_geometry(Arabsite_rast), add=TRUE, col = "green")
plot(st_geometry(Jrabsite_rast), add=TRUE, col="black")

plot(st_geometry(Nroute_rast), add=TRUE, col="red")
plot(st_geometry(Sroute_rast), add=TRUE, col="blue")





#################### STILL WORKING ON : #######################################

## Calc how much of the route lines fall within each grid cell (terra::extract)(rasterization)

#need to change sf objects to spatvector first before can use Rasterize:
Nroute_spat <- terra::vect(Nroute_rast)
#check:
class(Nroute_spat)#worked

#Rasterization of Routes:
NRoute_test <- terra::rasterize(Nroute_spat, rast_template_crop)#worked
# But what is it telling us?
summary(NRoute_test)#dont understand what this tells us?
plot(NRoute_test)#just shows Nroute basically 

#notes from chapt.6 on rasterization:
# The fun argument specifies summary statistics used to convert multiple 
# observations in close proximity into associate cells in the raster object. 
# By default fun = "last" is used but other options such as fun = "length" 
# can be used, in this case to count the number of cycle hire points in each grid cell 
#Example text from chapt.6:
#ch_raster2 = rasterize(cycle_hire_osm_projected, raster_template, 
#fun = "length")

#I tried to add this to the end of Nroute_test line but it did not see to change anything?
#what is the output we are looking for here?
NRoute_test2 <- terra::rasterize(Nroute_spat, rast_template_crop, fun = "length")
plot(NRoute_test2)
#this is not working 
# they just look the same 
# i think it is because we are using a line here with the routes not points like
#is used in chapt. 6

# I am going to try it with the rabbit location data ,, see what happens?
class(Arabsite_rast)
#need to change sf objects to spatvector first before can use Rasterize:
Arabsite_spat <- terra::vect(Arabsite_rast)
class(Arabsite_spat)

Arabsite_test <- terra::rasterize(Arabsite_spat, rast_template_crop, fun = "length")
plot(Arabsite_test)#shows the number or rab obs in each grid cell 

#helped a little bit to zoom in but still very hard to see but this method did 
#work for the points/ rab location 


##repeat for June22 rab loc:
class(Jrabsite_rast)
#need to change sf objects to spatvector first before can use Rasterize:
Jrabsite_spat <- terra::vect(Jrabsite_rast)
class(Jrabsite_spat)

Jrabsite_test <- terra::rasterize(Jrabsite_spat, rast_template_crop, fun = "length")
plot(Jrabsite_test)#shows the number or rab obs in each grid cell 
#works but still very zoomed out to be able to see much here



#transect each polygon with a grid cell :  -----------




############################################################################
#These steps appear to not work here - Matt's method/steps may need to be adjusted 
# because this is not working here 

st_intersection(rast_template, Nroute_rast)
#cant use on raster 

terra:: extract(x = rast_template, y = Nroute_rast)
#HELP NOTES::
# Extract values from a SpatRaster for a set of locations. 
#The locations can be a SpatVector (points, lines, polygons), 
#a matrix with (x, y) or (longitude, latitude – in that order!) 
#coordinates, or a vector with cell numbers.
# 
# When argument y is a SpatVector, and list=FALSE, the first column has the ID 
#(record number) of the SpatVector used.

# S4 method for signature 'SpatRaster,SpatVector'
# extract(x, y, fun=NULL, method="simple", list=FALSE, factors=TRUE, 
#         cells=FALSE, xy=FALSE, weights=FALSE, exact=FALSE,
#         touches=is.lines(y), layer=NULL, ...)



#creating Objects of class SpatVector.:

Nroute_rast<-terra::vect(Nroute_rast)
#check:
class(Nroute_rast)
#spatvector 


#try again 
terra:: extract(x = rast_template, y = Nroute_rast)
#Error: [extract] raster has no value

terra:: extract(x = NCAgrid300m, y = Nroute_rast)
#nope

st_area(Nroute_rast)#0 [m^2]
#Doesnt work 

#############################################################################






# Analyzing Spatial Data --------------------------------------------------







##############################################################################
#raster::rasterize		= Rasterize points, lines, or polygons

#terra::rasterize		  = Rasterize vector data

# do we need to rasterize vector data to analyze ? 
# - i belive so 
# so i will be doing that now - chapt 6 ref :
# arguments are, x, vector object to be rasterized and, y, a ‘template raster’ object defining the extent, resolution and CRS of the output. 
# By default fun = "last" is used but other options such as fun = "length" can be used, in this case to count the number of cycle hire points in each grid cell
# Nroute_rast needs to be turned into a spat vector here firts before u can run terra::rasterize()
#creating Objects of class SpatVector.:

Nroute_rast<-terra::vect(Nroute_rast)
#check:
class(Nroute_rast)
#spatvector 


A<-terra::rasterize(Nroute_rast, rast_template, fun = "length")
plot(A)

#help docs info:
## S4 method for signature 'SpatVector,SpatRaster'
# rasterize(x, y, field="", fun, ..., background=NA, touches=FALSE,
#           update=FALSE, sum=FALSE, cover=FALSE, filename="", overwrite=FALSE, wopt=list())


#############################################################################




## Save work space:   -----------
# saving all data to the path
save.image("SpotlightOpt_Raster2.RData")




# loading the workspace
#load("SpotlightOpt_Raster2.RData")
