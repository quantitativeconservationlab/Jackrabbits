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


#Making sure teplate raster crs matches with raster NCAgrid300m:
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

#Plotting Raster and Vector objects together to check :  -----------
plot(rast_template)
plot(st_geometry(NCAb_rast), add=TRUE)
#plot(st_geometry(Arabsite_rast), add=TRUE, col = "green")
#plot(st_geometry(Jrabsite_rast), add=TRUE, col="black")

plot(st_geometry(Nroute_rast), add=TRUE, col="red")
plot(st_geometry(Sroute_rast), add=TRUE, col="blue")




#transect each poly.with a grid cell :  -----------
st_intersection(rast_template, Nroute_rast)
#cant use on raster 
############################################################################
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


#############################################################################






# Analyzing Spatial Data --------------------------------------------------


st_area(Nroute_rast)#0 [m^2]
#Doesnt work 





##############################################################################
#raster::rasterize		= Rasterize points, lines, or polygons

#terra::rasterize		  = Rasterize vector data

# do we need to rasterize vector data to analyze ? 





#############################################################################




## Save work space:   -----------
# saving all data to the path
save.image("SpotlightOpt_Raster2.RData")




# loading the workspace
#load("SpotlightOpt_Raster2.RData")