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

## Load packages relevant to this script:
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


# Load and Create Data ----------------------------------------------------

# Set working directory. This is the path to your Rstudio folder for this 
# project. If you are in your correct Rstudio project then it should be:
getwd() #"C:/Users/leticiacamacho/Documents/BTJR_MSProject/Rcode/Spotlights_Hab_2022/Jackrabbits"


## Set data paths:  -----------

#Creating pathway to call the habitat NLCD data from the common drive
habpath <- "Z:/Common/QCLData/Habitat/" 
#Creating pathway to call the BTJR data from the common drive
datapath <- "Z:/Common/Jackrabbits/BTJR_Aug22_Spotlight.Surveys/" 
#creating pathway to call the 300x300m grid cells of NCA
rastpath<-"Z:/Common/QCLData/Habitat/NLCD_new/NCA_raster_summaries_300m/"



## Importing Data:  -----------

#import 300mx300m grid cell NCA raster from NLCD_new folder:
NCAgrid300m<- raster::raster( paste0(rastpath,
                                  "c20_agg.img")) 

#We need make sure all spatial data imported match. 
#- will do this in later step 
plot(NCAgrid300m)
#since this is a raster file and the other files we are currently working with
#are vector data we will fit the crs to match this raster 


#import NCA shapefile from habitat folder:
NCAboundary <- sf::st_read( paste0( habpath, 
          "NCA/GIS_NCA_IDARNGpgsSampling/BOPNCA_Boundary.shp"))
#NCAboundary Projected CRS: NAD83 / UTM zone 11N + NAVD88 height
#Geometry type: POLYGON, 1 feature and 10 fields,
plot(NCAboundary)


#Importing Survey Routes:

#Importing Southern Routes:
route_S <- sf::st_read( paste0(datapath, 
      "BTJR_Aug22_Spotlights_shp/Bigfoot_Simco_transect.shp" ) )
#Note that this is a polygon or vector file so we will need to make sure it 
#matches the above rater NCAgrid300m file 
plot(route_S )
route_S
#geometry: POINT
#WGS84

#Import Northern Route:
route_N <- sf::st_read( paste0(datapath, 
                               "BTJR_Aug22_Spotlights_shp/Standifer_Combined_transect.shp" ) )
plot(route_N )
route_N

#geometry: POINT
#WGS84


## Import Rabbit Locations:
rabsdawn_tot <- sf::st_read( paste0(datapath, 
                            "BTJR_Aug22_Spotlights_shp/BTJR_Dawn_Aug22.shp") )

rabsdusk_tot <- sf::st_read( paste0(datapath, 
                                "BTJR_Aug22_Spotlights_shp/BTJR_Dusk_Aug22.shp") )

str(rabsdawn_tot)
str(rabsdusk_tot)


# Preparing Data: ---------------------------------------------------------

## Cleaning data:  -----------

#Selecting columns of interest for routes:
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


#Selecting columns of interest for rabbits:

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
unique(rabsdusk$name) 
#there are too many unique names because of the GPS's and the complicated
#in field naming scheme. Need to simplify this 



## Manipulating data:  -----------

##convert geometry from points to a line for routes :
#southern route:

#checking current geometry classification
class(st_geometry(route_S)) 
#"sfc_POINT" "sfc"


#converting from point to line string:

#Creating an object for the geometry column in the south route df
Sgeo<-route_S$geometry
#A step you need to do to inform the function below that is building the line
n<-length(Sgeo)-1
#creating function that is creating line strings from the point data 
Sroute<-lapply(X=1:n, FUN=function(x){
  pair<-st_combine(c(Sgeo[x], Sgeo[x+1]))
  line<-st_cast(pair,"LINESTRING")
  return(line)
})
#combining all the line strings together to create the continuous route 
Sroute_line<-st_multilinestring(do.call("rbind",Sroute))
#view new line
plot(Sroute_line)
class(Sroute_line)#"MULTILINESTRING" "sfg"




#northern route:

#Creating an object for the geometry column in the south route df
Ngeo<-route_N$geometry
#A step you need to do to inform the function below that is building the line
n2<-length(Ngeo)-1
#creating function that is creating line strings from the point data 
Nroute<-lapply(X=1:n2, FUN=function(x){
  pair<-st_combine(c(Ngeo[x], Ngeo[x+1]))
  line<-st_cast(pair,"LINESTRING")
  return(line)
})

head( Nroute)

#combining all the line strings together to create the continuous route 
Nroute_line <- st_multilinestring(do.call("rbind",Nroute))
#######################JC NOTES FROM MEETING: ########################
Nroute_2 <- do.call( what = sf:::rbind.sf,
                     args = Nroute )
#################################################################

#view new line
plot(Nroute_line)
class(Nroute_line)#"MULTILINESTRING" "sfg"



###Checking Geometries:

sf::st_is_valid(NCAboundary)#TRUE
sf::st_is_valid(Nroute_line)#TRUE - so these are valid geometry
sf::st_is_valid(Sroute_line)#TRUE
#Need to assign these lines a crs 
any(is.na(st_is_valid(Sroute_line)))#FALSE - checked both N and S
#So, there is no NAs in these lines 

#empty geometries, using any(is.na(st_dimension(x)))
#corrupt geometries, using any(is.na(st_is_valid(x)))
#invalid geometries, using any(na.omit(st_is_valid(x)) == FALSE); in case of corrupt and/or invalid geometries,
#in case of invalid geometries, query the reason for invalidity by st_is_valid(x, reason = TRUE)
any(is.na(st_dimension(Nroute_line)))#FALSE - Meaning there is no NAs 
#checked both N and S

#checked rabdawn and dusk, and NCAboundary 
#wont work on rater layer though 
any(is.na(st_is_valid(NCAboundary)))#All FALSE
any(na.omit(st_is_valid(NCAboundary)) == FALSE)
#all FALSE 
#good to go on geometry - except altering CRS



## match crs of all vector data to raster layer crs:

#identify what crs the data is currently in:
sf::st_crs(route_N)#"WGS 84"
sf::st_crs(route_S)#"WGS 84"
sf::st_crs(NCAgrid300m)#"WGS 84"
sf::st_crs(NCAboundary)#"NAD 83"
sf::st_crs(Nroute_line)#NA
sf::st_crs(Sroute_line)#"NA"

#another way to check this is useing :
#st_is_longlat(Nroute_line)
#Also returns an NA
sf::st_crs(rabsdawn) #"WGS 84"


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
plot(NCAgrid300m)
plot(st_geometry(NCAb_trans), add=TRUE)
#plot(st_geometry(rabs_tot), add=TRUE)
########### Would adding rab points work if it was more zoomed in on the routes?
#raster cropped? #########################



# change route_lines to same crs as NCA boundary / or raster 

#Nrouteline_trans<- sf::st_transform( Nroute_line, st_crs( NCAgrid300m ) )#ERROR
#i think it is giving an error because it is a multi line string 
identicalCRS(as(Nroute_line, "Spatial"), NCAgrid300m)
#FALSE
new.crs<-CRS("+init=epsg:4326")#WGS84
##########THIS WAY NOT SF PACKAGE COMPATIABLE THIS =SP PACKAGE ###############
###########JC MEETING NOTES:
st_crs( Nroute_line)
class( Nroute_line)
head( Nroute_line) 
Nroute_pr <-  Nroute_line

st_crs(Nroute_pr) = st_crs(4326) 

Nroute.proj<- Nroute_line %>% st_transform(., new.crs)#didnt work - not creating object even 
#Nroute.proj<-spTransform(Nroute_line, new.crs) 
#also didnt work 

#######################STOPPING HERE BECASUE CANT GET MULTILINE STRING OF ROUTES
# TO MATCH THE RASTER YET HERE ############################
############################ MOVING ON TO CLEANING RAB DF #############



#Check if it worked:
st_crs(Nroute.proj) == st_crs(NCAgrid300m)#FALSE



# Cleaning Data -----------------------------------------------------------


#Cleaning Rabbit Observation Data (GPS Locations):

##### TO DO:
#make sure all names are the same for obs. on site and incidentals. 
#create separate df containing columns of interest
#

#Checking that the rabbit dfs have the same columns before combining dfs together
head( rabsdusk ); dim(rabsdusk)
head( rabsdawn ); dim(rabsdawn)

#combine dawn and dusk rabbit dfs 
rabs_tot<- dplyr::bind_rows(rabsdawn, rabsdusk)
head( rabs_tot ); dim(rabs_tot)
#verified correct amount of observations transferred over and combined correctly

unique(rabs_tot$name) 
#Clean up name column in new df

#Converting name column to have simply jackrabbit and cottontail observations
rabs_tot <- rabs_tot %>%
  dplyr::mutate(Rab.Obv = ifelse(startsWith(name, "J"),"Jackrab", 
                          ifelse(
                            startsWith(name, "J"), "Jackrab","Cottontail")))
view(rabs_tot)

#Creating a separate jackrabbit specific df - w/no cottontails 
#Also, creating age column in new btjr df
btjr.df <- rabs_tot %>% 
  filter(Rab.Obv=="Jackrab") %>%
  dplyr::mutate(BTJR.age = 
                  ifelse(startsWith( name, "Ja"), "Adult", "Juv"))
#####GOT THIS TO WORK BUT NOW THE JUV INCLUDE THE UNKN
view(btjr.df)
#Arrange by name? 
arrange(btjr.df, name)

                         
      ####### TRIED SO MANY WAYS TO HAVE THIS COLUMN INCLUDE 3 AGE CLASSES: #########
                         
               # ifelse(startsWith( name, "Jj"), "Juv", "Unknown")))
                         
                         
                         
                         #ifelse(startsWith("Ja"), "Juv", "Unknown")))
                         
                         #
                         # ifelse(startsWith(name, "Jj" ), "Juv", "Unknown",
                         #        
                         #        ))) 
                         
                         ####
                  # ifelse(startsWith( name, "Ja"), "Adult", "Juv",
                  # ifelse(startsWith( name, "Jj"), "Juv",
                  # ifelse(startsWith( name, "Jj"), "Juv", "Unknown")))))
                  
 ################## STILL IN PROGRESS: WHEN ADDED ON "Jj" IFELSE STATEMENTS, 
#I GOT MANY ERRORS - I MAY NOT UNDERSTAND HOW TO USE THIS FUNCTION WHEN THERE
#IS MORE THAN 2 OPTIONS 
# - BUT WHEN RAN WITH OUT IT, THE NEW COLUMN IDENTIFIES JUV.s AS ADULTS AND 
#UNKNOWNS AS JUV.s  ############################

#try diff way?:
#btjr.df$BTJR.age<-



####################### JC NOTES FROM MEETING: ##############################
#FOR NOW IGNORE AGE AND FOCUS ON CREATING NEW BTJR DF WITH ACCURATE 
#DATE, TIME, ROW NAMES, ETC.
#############################################################################

## Try plotting BTJR points on map:
plot(BTJRdf$geometry)
crs(BTJRdf$geometry)#WGS84

plot(NCAgrid300m)
plot(st_geometry(BTJRdf), add=TRUE)#Can not see BTJR points on map 
#Do we need to crop to be able to see points?????


  


## Creating new cleaned btjr df with usable date, time, locations, and IDs:

#Cleaning up date/time to workable format for lubridate package:
#creating duplicate/new jackrabbit df to manipulate:
BTJRdf <- btjr.df %>%
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


#add new column of unique IDs using row numbers
#   - the reason we need to do this is becasue when we downloaded the GPS pts 
#     from our original 2 GPS's they automatically assigned each pt with a number
#     behind our naming scheme and as an ID. The ID numbers repeated themselves
#     in original df becasue they were from 2 seperate GPS's 
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

 

#Extract hour of night:
BTJRdf$Hour <- lubridate::hour(BTJRdf$NewDate.Time)#create new Hour column
#View
view(BTJRdf)
#Extract day of yr (out of 365):
BTJRdf$DayOfYr <- lubridate::yday(BTJRdf$NewDate.Time)#create new Day of yr col.
#View
view(BTJRdf)
#Extract date:
BTJRdf$Date <- lubridate::date(BTJRdf$NewDate.Time)


#reduce size of final df to only include columns of interest:
BTJRdf <- subset(BTJRdf, select = -time)
#is wanted to remove more than one column the code would be:
#   df <- subset (df, select = -c(x,y))


#Re-ordering the columns of df :
BTJRdf<- BTJRdf %>% dplyr::select(RabID,Rab.Obv, name, Date, Hour, DayOfYr, 
                         NewDate.Time, lat, lon, geometry )
#view
view(BTJRdf)



# Visualizing Date --------------------------------------------------------

BTJRdf %>%
  dplyr::summarise()

ggplot(BTJRdf, aes(x=Hour, y=  )) +
  theme_classic(base_size = 17) +
  geom_bar(size = 2)








# Saving Data -------------------------------------------------------------
# save cleaned BTJRdf csv:
write.csv( x = BTJRdf, file = "AugBTJR_Clean.csv" )





