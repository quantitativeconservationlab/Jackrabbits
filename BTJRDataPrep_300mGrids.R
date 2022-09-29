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


## Github Code Source:  -----------

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

#CONFIGURE/MANIPULATE:
#Make all the vector crs's match the 300m grid rater 
#Create cleaned dfs for the rabbit data and locations 
#Create cleaned df for the routes - where routes are changed from points to lines 



# Setup -------------------------------------------------------------------

## Set up your work space and load relevant packages -----------

## Clean your work space to reset your R environment. 
rm( list = ls() )




## Load packages relevant to this script:  -------------

library( tidyverse ) #package for easy data manipulation
#install.packages("tidyverse") 
#Use this is package is not already installed or is not loaded properly 
# set option to see all columns and more than 10 rows
options( dplyr.width = Inf, dplyr.print_min = 100 )#for visual aid 
library( sf )#package for spatial data manipulation for vector data 
#install.packages("sf")
library( raster ) #for raster manipulation
#Can create raster layers objects using raster function within raster package:
library( rasterVis ) #for raster visualization
library(RColorBrewer)
library( terra ) #for raster vis and manipulation 
library(lubridate)#used to organize and format date and time data 
library(rgdal)
library(ggplot2)

## End of package load -------------






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
datapath <- "Z:/Common/Jackrabbits/BTJR_Aug22_Spotlight.Surveys/" 
#creating pathway to call the 300x300m grid cells of NCA
rastpath<-"Z:/Common/QCLData/Habitat/NLCD_new/NCA_raster_summaries_300m/"



## Importing Data:  -----------

#import 300mx300m grid cell NCA raster from NLCD_new folder:  -----------
NCAgrid300m<- raster::raster( paste0(rastpath,
                                  "c20_agg.img")) 

#We need make sure all spatial data imported match. 
#- will do this in later step 

##since this is a raster file and the other files we are currently working with
#are vector data we will fit the crs to match this raster 

#View Raster:
plot(NCAgrid300m)



#import NCA shapefile from habitat folder:  -----------
NCAboundary <- sf::st_read( paste0( habpath, 
          "NCA/GIS_NCA_IDARNGpgsSampling/BOPNCA_Boundary.shp"))
#NCAboundary Projected CRS: NAD83 / UTM zone 11N + NAVD88 height
#Geometry type: POLYGON, 1 feature and 10 fields,
plot(NCAboundary)


##Importing Survey Routes:  -----------
#Importing Southern Routes:
route_S <- sf::st_read( paste0(datapath, 
      "BTJR_Aug22_Spotlights_shp/Bigfoot_Simco_transect.shp" ) )
#Note that this is a polygon or vector file so we will need to make sure it 
#matches the above rater NCAgrid300m file 

#Viewing imported Southern routes:
plot(route_S )
route_S#geometry: POINT, CRS: WGS84

#Import Northern Route:
route_N <- sf::st_read( paste0(datapath, 
                               "BTJR_Aug22_Spotlights_shp/Standifer_Combined_transect.shp" ) )
#Viewing imported Northern routes:
plot(route_N )
route_N#geometry: POINT, CRS: WGS84





## Import Rabbit Locations:  -----------
rabsdawn_tot <- sf::st_read( paste0(datapath, 
                            "BTJR_Aug22_Spotlights_shp/BTJR_Dawn_Aug22.shp") )
#View(rabsdawn_tot)

rabsdusk_tot <- sf::st_read( paste0(datapath, 
                                "BTJR_Aug22_Spotlights_shp/BTJR_Dusk_Aug22.shp") )
#View(rabsdusk_tot)

#Checking structure of imported rabbit locations:
str(rabsdawn_tot)
str(rabsdusk_tot)





# Preparing Data: ---------------------------------------------------------

## Cleaning data [ROUTES] :  -----------

#Selecting columns of interest for routes:  -----------

#Southern Routes:
route_S <- route_S %>% 
  #keep columns of interest only
  dplyr::select( ID, trksegID, lat, lon, geometry )
#view
route_S


#Northern Routes:
route_N <- route_N %>% 
  #keep columns of interest only
  dplyr::select( ID, trksegID, lat, lon, geometry )
#view
route_N



## Cleaning data [RABBITS] :  -----------

#Selecting columns of interest for rabbits:  -----------

#dusk crew
rabsdusk<- rabsdusk_tot %>% 
  dplyr::select(ID, name,lat, lon, time, geometry)
#view
rabsdusk


#dawn crew
rabsdawn<- rabsdawn_tot %>% 
  dplyr::select(ID, name,lat, lon, time, geometry)
#view
rabsdawn
#Checking if there are differences in the rabbit naming scheme for each crew





##Cleaning Rabbit Observation Data (GPS Locations):  -----------

#Checking that the rabbit dfs have the same columns before combining dfs together
head( rabsdusk ); dim(rabsdusk)
head( rabsdawn ); dim(rabsdawn)

#combine dawn and dusk rabbit dfs to create 1 total rabbit df:  -----------

rabs_tot<- dplyr::bind_rows(rabsdawn, rabsdusk)
head( rabs_tot ); dim(rabs_tot)
#verified, when you add together the total observations for each df 
#you get the correct amount of observations transferred over 
#and combined correctly.

unique(rabs_tot$name) 
#Clean up name column in new df 
# do this - when we want to look at age categories of rabbits
#not needed right now though 


#Creating New column in new df:
#Converting name column to have simply jackrabbit and cottontail observations
rabs_tot <- rabs_tot %>%
  dplyr::mutate(Rab.Obv = ifelse(startsWith(name, "J"),"Jackrab", 
                                 ifelse(
                                   startsWith(name, "J"), "Jackrab","Cottontail")))
view(rabs_tot)




# Creating Jackrabbit Specific DF: ----------------------------------------


#Creating a separate jackrabbit specific df - w/no cottontails:  -----------
#Also, creating age column in new btjr df
BTJRdf <- rabs_tot %>% 
  filter(Rab.Obv=="Jackrab")


####### TRIED SO MANY WAYS TO HAVE THIS COLUMN INCLUDE 3 AGE CLASSES:  - UNSUCCSESFULLY #########
########### TRIED TO ADD AGE COLUMN: UNSUCESSFULLY ############
#%>%
  #dplyr::mutate(BTJR.age = 
                 # ifelse(startsWith( name, "Ja"), "Adult", "Juv"))
#####GOT THIS TO WORK BUT NOW THE JUV INCLUDE THE UNKN

view(BTJRdf)
#Arrange by name? 
#When we need to look at age classes



#Cleaning up date/time to workable format for lubridate package:  -----------
#creating duplicate/new jackrabbit df to manipulate:
BTJRdf <- BTJRdf %>%
  dplyr::select(name, lat, lon, time, geometry, Rab.Obv)



#create an empty column for new date and time format to fit into
BTJRdf$NewDate.Time<-NA
#view
dim(BTJRdf)#check contains new column 
#creating a for loop that will go through each row of time column in df to 
#   split up the current format and change it to ymd_hms format
#   this will get rid of the confusing format it is currently in w/ Ts and Zs in it
for( i in 1:dim(BTJRdf)[1] ) {
  a <- str_split(BTJRdf$time[i], "T" )[[1]]
  b <- str_split(a[2], "Z")[[1]] [1]
  BTJRdf$NewDate.Time[i]<-paste(a [1], b, sep = " ")
}



view(BTJRdf)
#has new date.time column with format able to work with lubridate package


#add new column of unique IDs using row numbers:  -----------
#   - the reason we need to do this is because when we downloaded the GPS pts 
#     from our original 2 GPS's they automatically assigned each pt with a number
#     behind our naming scheme and as an ID. The ID numbers repeated themselves
#     in original df because they were from 2 separate GPS's 
BTJRdf<- BTJRdf %>%
  dplyr::mutate(RabID=row_number())
#view
head(BTJRdf)

#use
BTJRdf$NewDate.Time <- lubridate::ymd_hms( paste( BTJRdf$NewDate.Time),
                                           tz = "MST" )
#view
head(BTJRdf)
view(BTJRdf)

##########################################################################

#converting time stamp from UTC to MST:

BTJRdf$TEST.time <- with_tz(lubridate::ymd_hms(paste( BTJRdf$NewDate.Time), 
                                               tzone= "US/Mountain" )




###########################################################################

#Extract hour of night:
BTJRdf$Hour <- lubridate::hour(BTJRdf$NewDate.Time)#create new Hour column

#Extract day of yr (out of 365):
BTJRdf$DayOfYr <- lubridate::yday(BTJRdf$NewDate.Time)#create new Day of yr col.

#Extract date:
BTJRdf$Date <- lubridate::date(BTJRdf$NewDate.Time)

#View
view(BTJRdf)


#reduce size of final df to only include columns of interest:
BTJRdf <- subset(BTJRdf, select = -time)


#is wanted to remove more than one column the code would be:
#   df <- subset (df, select = -c(x,y))


#Re-ordering the columns of df :
BTJRdf<- BTJRdf %>% dplyr::select(RabID,Rab.Obv, name, Date, Hour, DayOfYr, 
                                  NewDate.Time, lat, lon, geometry )
#view
view(BTJRdf)



















# Checking Geometries: ----------------------------------------------------

##Checking if geometry of spatial objects are valid :  -----------
sf::st_is_valid(NCAboundary)#TRUE
sf::st_is_valid(route_N)#TRUE - so these are valid geometry
sf::st_is_valid(route_S)#TRUE
any(is.na(st_is_valid(route_S)))#FALSE - checked both N and S
#So, there is no NAs in these lines 


## Other ways to check geometries :  -----------

#empty geometries, using any(is.na(st_dimension(x)))
#corrupt geometries, using any(is.na(st_is_valid(x)))
#invalid geometries, using any(na.omit(st_is_valid(x)) == FALSE); in case of corrupt and/or invalid geometries,
#in case of invalid geometries, query the reason for invalidity by st_is_valid(x, reason = TRUE)
any(is.na(st_dimension(route_S)))#FALSE - Meaning there is no NAs 
#checked both N and S

#checked rabdawn and dusk, and NCAboundary 
#wont work on rater layer though 
any(is.na(st_is_valid(NCAboundary)))#All FALSE
any(na.omit(st_is_valid(NCAboundary)) == FALSE)
#all FALSE 
#good to go on geometry - except altering CRS



## Assessing CRS of spatial objects  -----------

#identify what crs the data is currently in:
sf::st_crs(route_N)#"WGS 84"
sf::st_crs(route_S)#"WGS 84"
sf::st_crs(NCAgrid300m)#"WGS 84"
sf::st_crs(NCAboundary)#"NAD 83"
sf::st_crs(BTJRdf) #"WGS 84"


#another way to check this is useing :
#st_is_longlat()





# Altering Geometries  ----------------------------------------------------

##Changing CRS of spatial objects :  -----------

#changing NCA boundary to match NCA grid 300m raster:

#create a version that matches coordinates of the predictor raster:
NCAb_trans <- sf::st_transform( NCAboundary, st_crs( NCAgrid300m ) )#worked
#NCA boundary crs now same as NCSgrid300m raster 
#Check that this is correct:
st_crs(NCAb_trans) == st_crs(NCAgrid300m)
#TRUE - so this is good to go


#Another way to check this:
#identicalCRS(as(NCAb_trans, "Spatial"), NCAgrid300m)
#TRUE



#Plotting Raster and Vector objects together to check :  -----------
plot(NCAgrid300m)
plot(st_geometry(NCAb_trans), add=TRUE)
##SAVED IMAGE IN COMMON > JACKRABBITS > AUG SPOTLIGHT SURVEYS > R PLOTS > NCA300GRID+NCA.BOUNDARY






# Convert Route and BTJR Points to Raster CRS -----------------------------

##Northern Route:  [LINES!] -----------

route_N$lon<-as.numeric(route_N$lon)
route_N$lat<-as.numeric(route_N$lat)

pointz_Nroute<-cbind(x=route_N$lon, y=route_N$lat)


crs<-"+proj=longlat +datum=WGS84"

Nroute_line<-terra::vect(pointz_Nrroute, crs=crs, type="line")


plot(Nroute_line)# is in WGS84

class(Nroute_line)#NOW A SPATRASTER/TERRA OBJECT 
# [1] "SpatVector"
# attr(,"package")
# [1] "terra"

#checking crs matches raster:
terra::crs(Nroute_line)#WGS84





##Southern Route:  [LINES!] -----------

route_S$lon<-as.numeric(route_S$lon)
route_S$lat<-as.numeric(route_S$lat)

pointz_Srroute<-cbind(x=route_S$lon, y=route_S$lat)


Sroute_line<-terra::vect(pointz_Srroute, crs=TEST_crs, type="line")

plot(Sroute_line)# is in WGS84

class(Sroute_line)#NOW A SPATRASTER/TERRA OBJECT 
# [1] "SpatVector"
# attr(,"package")
# [1] "terra"

#checking crs matches raster:
terra::crs(Sroute_line)#WGS84






##Northern Route:  [POINTS] -----------

routeN_coord<-route_N # THIS WORKED 

route_coord_crs<- sp::CRS("+proj=longlat
                           +datum=WGS84")


Ncoordinates<-sf:: st_as_sf(routeN_coord, 
                            coords=c("Longitude", "Latitude"),
                            crs=route_coord_crs)

st_crs(Ncoordinates)

Ncoordinates_aes<-sf::st_transform(Ncoordinates, crs(NCAgrid300m))
#Checking that CSR of vector matches raster:
st_crs(Ncoordinates_aes) == st_crs(NCAgrid300m)#TRUE!!
#sf :: st_crs

#View:
plot(NCAgrid300m)
plot(st_geometry(Ncoordinates_aes), add=TRUE)

####### THAT WORKED!! 
##SAVED IMAGE IN COMMON > JACKRABBITS > AUG SPOTLIGHT SURVEYS > R PLOTS > NCA300GRID+NorthRoute(pts)




## Southern route: [POINTS] ----------- 
routeS_coord<-route_S

Scoordinates<-sf:: st_as_sf(routeS_coord, 
                            coords=c("Longitude", "Latitude"),
                            crs=route_coord_crs)

st_crs(Scoordinates)

Scoordinates_aes<-sf::st_transform(Scoordinates, crs(NCAgrid300m))

st_crs(Scoordinates_aes) == st_crs(NCAgrid300m)#TRUE!!
#sf :: st_crs

plot(NCAgrid300m)
plot(st_geometry(Scoordinates_aes), add=TRUE)
##SAVED IMAGE IN COMMON > JACKRABBITS > AUG SPOTLIGHT SURVEYS > R PLOTS > NCA300GRID+NorthRoute(pts)





## BRJR points:  -----------

BTJR_coord<-BTJRdf$geometry

BTJRcoordinates<-sf:: st_as_sf(BTJR_coord, 
                            coords=c("Longitude", "Latitude"),
                            crs=route_coord_crs)

st_crs(BTJRcoordinates)

BTJRcoordinates_aes<-sf::st_transform(BTJRcoordinates, crs(NCAgrid300m))

st_crs(BTJRcoordinates_aes) == st_crs(NCAgrid300m)#TRUE!!
#sf :: st_crs

#View:
plot(NCAgrid300m)
plot(st_geometry(BTJRcoordinates_aes), add=TRUE)#WORKED
##SAVED IMAGE IN COMMON > JACKRABBITS > AUG SPOTLIGHT SURVEYS > R PLOTS > NCAgrid300m.Rast+BTJR.Obs(pts)















# Visualizing Date --------------------------------------------------------

BTJRdf %>%
  dplyr::summarise()

ggplot(BTJRdf, aes(x=Hour, y=  )) +
  theme_classic(base_size = 17) +
  geom_bar(size = 2)







#Plotting spatvector route lines on to the raster 300m grid:  -----------

#create a df from raster
Nroute.line_df<-raster::as.data.frame(Nroute_line, XY=TRUE)
######REFERENCING MATT CLARKS SCRIPTS IN SLACK ON HOW HE DID THIS BUT 
###### RUNNING IN TO ERROR HERE WHEN RUNNING HTIS FIRST LINE 
#Error in data.frame(x$values(), check.names = check.names, stringsAsFactors = stringsAsFactors,  : 
#arguments imply differing number of rows: 0, 1







# Saving Data -------------------------------------------------------------
## Save csv's:   -----------

# Save cleaned BTJRdf csv:
write.csv( x = BTJRdf, file = "BTJR.obs_AUG_Clean.csv" )
#includes geometry, time, date, lat, long locations, etc.





## Save work space:   -----------
# saving all data to the path
save.image("saveworkspace.RData")




# loading the workspace
load("saveworkspace.RData")
