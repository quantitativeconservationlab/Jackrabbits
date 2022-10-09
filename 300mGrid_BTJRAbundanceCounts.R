# Setup -------------------------------------------------------------------

## Set up your work space and load relevant packages -----------

## Clean your work space to reset your R environment. 
rm( list = ls() )




## Load packages relevant to this script:  -------------

library( tidyverse ) #package for easy data manipulation
#install.packages("tidyverse") 
library(ggplot2)
library(lubridate)
library(sf)
library(terra)
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

## Set data paths:  -----------

#Creating pathway to call the habitat NLCD data from the common drive
habpath <- "Z:/Common/QCLData/Habitat/" 
#Creating pathway to call the BTJR data from the common drive
datapath <- "Z:/Common/Jackrabbits/BTJR_Aug22_Spotlight.Surveys/" 

#creating pathway to call the 300x300m grid cells of NCA
rastpath<-"Z:/Common/QCLData/Habitat/NLCD_new/NCA_raster_summaries_300m/"



# Importing Data: ---------------------------------------------------------

## Import Rabbit Data:  -----------

site<- read.csv(paste0(datapath, "Site_Aug2022.csv"))

#Jackrabbits<-read.csv(datapath, "BTJR.obs_AUG_Clean.csv")
#Re-created this df below because this csv saved and created in the 
#BTJRDataPrep_300mGrid script lost the sf geometry column somehow 

BigRabdf_extended<-read.csv(paste0(datapath, "BigRabdf_extended.csv"))

## Importing Raster Data:  -----------

#import 300mx300m grid cell NCA raster from NLCD_new folder:  -----------
NCAgrid300m<- raster::raster( paste0(rastpath,
                                     "c20_agg.img")) 

#import NCA shapefile from habitat folder:  -----------
NCAboundary <- sf::st_read( paste0( habpath, 
                                    "NCA/GIS_NCA_IDARNGpgsSampling/BOPNCA_Boundary.shp"))

#view:
View(site)
View(BigRabdf_extended)

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











# Cleaning dfs: -----------------------------------------------------------

##############################################################################


##########
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
##########










#Recreating rabbit df :

#saved BTJR.obs_AUG_clean csv lost the sf geometry column somehow 
## Import Rabbit Locations:  -----------
rabsdawn_tot <- sf::st_read( paste0(datapath, 
                                    "BTJR_Aug22_Spotlights_shp/BTJR_Dawn_Aug22.shp") )

rabsdusk_tot <- sf::st_read( paste0(datapath, 
                                    "BTJR_Aug22_Spotlights_shp/BTJR_Dusk_Aug22.shp") )
rabsdusk<- rabsdusk_tot %>% 
  dplyr::select(ID, name,lat, lon, time, geometry)

rabsdawn<- rabsdawn_tot %>% 
  dplyr::select(ID, name,lat, lon, time, geometry)

rabs_tot<- dplyr::bind_rows(rabsdawn, rabsdusk)

head( rabs_tot ); dim(rabs_tot)

rabs_tot <- rabs_tot %>%
  dplyr::mutate(Rab.Obv = ifelse(startsWith(name, "J"),"Jackrab", 
                                 ifelse(
                                   startsWith(name, "J"), "Jackrab","Cottontail")))

BTJRdf <- rabs_tot %>% 
  filter(Rab.Obv=="Jackrab")

BTJRdf <- BTJRdf %>%
  dplyr::select(name, lat, lon, time, geometry, Rab.Obv)

BTJRdf<- BTJRdf %>%
  dplyr::mutate(RabID=row_number())


## converting time stamp from UTC to MST:  -----------

BTJRdf$MST.time <- with_tz (lubridate::ymd_hms( BTJRdf$time),
                            tzone= "US/Mountain" )
#Extract hour of night:
BTJRdf$Hour <- lubridate::hour(BTJRdf$MST.time)#create new Hour column

#Extract day of yr (out of 365):
BTJRdf$DayOfYr <- lubridate::yday(BTJRdf$MST.time)#create new Day of yr col.

#Extract date:
BTJRdf$Date <- lubridate::date(BTJRdf$MST.time)

#Re-ordering the columns of df :
BTJRdf<- BTJRdf %>% dplyr::select(RabID,Rab.Obv, name, Date, Hour, DayOfYr, 
                                  MST.time, lat, lon, geometry )

#View
view(BTJRdf)



Jackrabbits<-BTJRdf



#creating Survey Night column in jackrabbit df:

#creating for loop to assign survey night: 

#First create an empty column to be filled:
Jackrabbits$SurveyNight<-NA

#Create for loop:


for (r in 1:dim(Jackrabbits)[1]){
  Jackrabbits$SurveyNight <-
     ifelse(Jackrabbits$DayOfYr == "213", "1",
     ifelse(Jackrabbits$DayOfYr == "214" & Jackrabbits$Hour < 20, "1",
     ifelse(Jackrabbits$DayOfYr == "214" & Jackrabbits$Hour > 20, "2", 
     ifelse(Jackrabbits$DayOfYr == "215" & Jackrabbits$Hour < 20, "2",
     ifelse(Jackrabbits$DayOfYr == "215" & Jackrabbits$Hour > 20, "3",
     ifelse(Jackrabbits$DayOfYr == "216" & Jackrabbits$Hour < 20, "3",
            
     ifelse(Jackrabbits$DayOfYr == "216" & Jackrabbits$Hour > 20, "4", 
     ifelse(Jackrabbits$DayOfYr == "217" & Jackrabbits$Hour < 20, "4",
     ifelse(Jackrabbits$DayOfYr == "219" & Jackrabbits$Hour > 20, "5",
     ifelse(Jackrabbits$DayOfYr == "220" & Jackrabbits$Hour < 20, "5",
            
     ifelse(Jackrabbits$DayOfYr == "220" & Jackrabbits$Hour > 20, "6", 
     ifelse(Jackrabbits$DayOfYr == "221" & Jackrabbits$Hour < 20, "6",
     ifelse(Jackrabbits$DayOfYr == "222" & Jackrabbits$Hour > 20, "7",
     ifelse(Jackrabbits$DayOfYr == "223" & Jackrabbits$Hour < 20, "7",
            
     ifelse(Jackrabbits$DayOfYr == "223" & Jackrabbits$Hour > 20, "8", 
     ifelse(Jackrabbits$DayOfYr == "224" & Jackrabbits$Hour < 20, "8",
            
            "NA"))))))))))))))))
     }






View(Jackrabbits)

#Jackrabbits %>%
#  filter(SurveyNight == "1")




# Cleaning "when" df: -----------------------------------------------------

#"When" df will be constructed from the site.csv information

#Select columns of interest:
BTJR_When.df <- site %>%
  dplyr::select(Survey_ID,Date, Crew_name,Night_number, Start_time, End_time,
                Site, Start_temp.F.,Start_wind.km.h., Spotlight_Start.Time, 
                Spotlight_End.Time) 

#Creating columns want to create:
BTJR_When.df$Duration.Hrs<- "NA"
BTJR_When.df$Sampling.Effort<- "NA"
BTJR_When.df$CellID <- "NA"

#Making a start and end time in lubridate format for each site specific time:
BTJR_When.df$Date <-lubridate::mdy(BTJR_When.df$Date)


#Combining date with start time to one column :  -----------
BTJR_When.df$Start_MST.time <- paste(BTJR_When.df$Date, BTJR_When.df$Start_time, 
                                    sep = " ")

#Combining date with end time to one column :  -----------
BTJR_When.df$End_MST.time <- paste(BTJR_When.df$Date, BTJR_When.df$End_time, 
                                     sep = " ")


#Putting created columns in lubridate format:
BTJR_When.df$Start_MST.time<-lubridate::ymd_hm(BTJR_When.df$Start_MST.time)
BTJR_When.df$End_MST.time<-lubridate::ymd_hm(BTJR_When.df$End_MST.time)

#rearrange df and remove unneeded columns:

BTJR_When.df <- BTJR_When.df %>%
  dplyr::select(Survey_ID,Night_number, Crew_name, 
                Site, Start_MST.time, End_MST.time,
                Start_temp.F.,Start_wind.km.h.,
                Duration.Hrs, Sampling.Effort, CellID) 

#Configuring Duration.Hrs column from start and end times:
BTJR_When.df$Duration.Hrs<-abs(difftime(BTJR_When.df$Start_MST.time, 
                                    BTJR_When.df$End_MST.time, units = "mins"))

#there is a problem with a few rows in the duration column
#when it is switching from 23hr to midnight 00 that same night this function is
#calculating the incorrect difftime in mins so i am going to manually change 
#these values for now:
BTJR_When.df$Duration.Hrs[2]<-"33"
BTJR_When.df$Duration.Hrs[13]<-"35"
BTJR_When.df$Duration.Hrs[15]<-"36"



#Creating a RouteID column to specify if this was a North or South route:
BTJR_When.df$RouteID <- NA

for (r in 1:dim(BTJR_When.df)[1]){
  BTJR_When.df$RouteID <- ifelse(BTJR_When.df$Site == "Bigfoot_butte" |
                                BTJR_When.df$Site == "Simco" , "S.Route","N.Route")

}
# | is used as an OR argument in the ifelse statements 
# you would use & if you wanted an AND argument in the ifelse statement 




View(BTJR_When.df)





#############################################################################
# NEED TO FIGURE OUT HOW TO ASSIGN GRID CELL ID'S TO BOTH DFS : 
# - WHEN.DF AND JACKRABBITS DF 


#############################################################################











# Checking Geometries: ----------------------------------------------------

#Changing Jackrabbit df from dataframe to sf object :  -----------
sf::st_as_sf(Jackrabbits)

#############################################################################
#DO I NEED TO DO THIS FOR ANY OTHER OBJECTS?

############################################################################
  
  

##Checking if geometry of spatial objects are valid :  -----------
#
all(st_is_valid(Jackrabbits))#TRUE
all(is.na(st_dimension(Jackrabbits)))#no missing geometry
#FALSE : is there NA=false for all ?

all(sf:: st_is_valid(NCAboundary))#TRUE
all(is.na(st_dimension(Jackrabbits)))#no missing geometry
#FALSE: is there NA=false for all


all(sf:: st_is_valid(route_N))#TRUE
all(is.na(st_dimension(route_N)))#no missing geometry
#FALSE: is there NA=false for all

all(sf:: st_is_valid(route_S))#TRUE
all(is.na(st_dimension(route_S)))#no missing geometry
#FALSE: is there NA=false for all




#Checking CRS of all objects :  -----------
sf::st_is_longlat(NCAgrid300m)#TRUE: Jackrabbit,N&Sroute, 
#FALSE: boundary . 300mgrid

sf:: st_crs(Jackrabbits)#WGS84
sf:: st_crs(NCAboundary)#NAD83 / UTM zone 11N + NAVD88 height
sf:: st_crs(NCAgrid300m)#+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs 
sf:: st_crs(route_N)#WGS84
sf:: st_crs(route_S)#WGS84








#Transforming crs to match raster :  -----------

#create a buffer around the NCA using outline of NCA and sf package:
NCA_buf <- NCAboundary %>% sf::st_buffer( dist =5e3 )
#create a version that matches coordinates of the predictor raster:
NCAb_trans <- sf::st_transform( NCA_buf, st_crs( NCAgrid300m ) )#worked
#NCA boundary crs now same as NCSgrid300m raster 
#Check that this is correct:
st_crs(NCAb_trans) == st_crs(NCAgrid300m)
#TRUE - so this is good to go

#Check to see that the rest of the objects are in the same crs as raster:
Jackrabbits_trans<-sf::st_transform( Jackrabbits, st_crs( NCAgrid300m ) )
st_crs(Jackrabbits_trans) == st_crs(NCAgrid300m)
#TRUE 

Nroute_trans<-sf::st_transform( route_N, st_crs( NCAgrid300m ) )
st_crs(Nroute_trans) == st_crs(NCAgrid300m)
#TRUE 

Sroute_trans<-sf::st_transform( route_S, st_crs( NCAgrid300m ) )
st_crs(Sroute_trans) == st_crs(NCAgrid300m)
#TRUE 




#Plotting Raster and Vector objects together to check :  -----------
plot(NCAgrid300m)
plot(st_geometry(NCAb_trans), add=TRUE)
plot(st_geometry(Jackrabbits_trans), add=TRUE)
plot(st_geometry(Nroute_trans), add=TRUE)
plot(st_geometry(Sroute_trans), add=TRUE)

#Worked good 

















# Checking extent and cropping raster to fit data more closely ------------

##Checking Extents :  -----------
st_bbox(Jackrabbits_trans)
#  xmin     ymin     xmax     ymax 
#-1626979  2409339 -1601886  2441791 
st_bbox(NCAb_trans)
#  xmin     ymin     xmax     ymax 
#-1655936  2362852 -1560889  2454363
ext(NCAgrid300m)
#SpatExtent : -1660905, -1555905, 2357685, 2459385 (xmin, xmax, ymin, ymax)

#WHAT DO I DO WITH THIS INFORMATION - HOW TO I CROP TO BETTER FIT THE 
#JACKRABBIT DATA?




##Checking Resolutions :  -----------
res(NCAgrid300m)#300 300
ncol(NCAgrid300m)#350
NCAgrid300m


Jackrabbits_trans


###########################################################################
# NEXT NEED TO CROP THE MAP TO BETTER FIT THE BTJR POINTS ON THE MAP 

##########################################################################

##########
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

####################





































# Visualizations: ---------------------------------------------------------

#Making Histograms of Weather Variables:  -----------

hist(BigRabdf_extended$Start_temp.F.)
hist(BigRabdf_extended$Start_wind.km.h.)

ggplot(data=BigRabdf_extended, aes(x=Start_temp.F.))+
  geom_histogram(binwidth = 2, color="black", fill="seagreen")+
  theme_bw()+
  labs(x= "Start Temp. (F)", 
       y= "Counts", 
       title = "Histogram of Starting Tempatures (deg. F) 
       for August 2022 Spotlight Surveys")
#This is so much uglier then the base hist()?



ggplot(data=BigRabdf_extended, aes(x=Start_wind.km.h.))+
  geom_histogram(binwidth = 2, color="black", fill="seagreen")+
  theme_bw()+
  labs(x= "Start Wind. (Km/hr)", 
       y= "Counts", 
       title = "Histogram of Starting Wind (Km/hr) 
       for August 2022 Spotlight Surveys")



#Making histogram of Jackrabbit obs:

ggplot(data = Jackrabbits, aes(x = DayOfYr))+
  geom_histogram(binwidth = 1)

ggplot(data = Jackrabbits, aes(x = Hour))+
  geom_histogram(binwidth = 1)
#NEED TO DEFINE X AXIS BORDERS 
ggplot(data = Jackrabbits, aes(x=Hour))+
  geom_bar()
#NEED TO DEFINE X AXIS BORDERS - that show hour: 20-5 continuously to show 
#distubution of #BTJR seen during these times




# Saving Data -------------------------------------------------------------
## Save csv's:   -----------

# Save cleaned csv:
write.csv( x = Jackrabbits , file = "Jackrabbits.Aug.csv" )





## Save work space:   -----------
# saving all data to the path
#save.image("300mgrid_Jackrabbit.RData")




# loading the workspace
load("300mgrid_Jackrabbit.RData")









