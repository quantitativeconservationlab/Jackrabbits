# Setup -------------------------------------------------------------------

## Set up your work space and load relevant packages -----------

## Clean your work space to reset your R environment. 
rm( list = ls() )




## Load packages relevant to this script:  -------------
#install.packages("oce")
library(oce)
library( tidyverse ) #package for easy data manipulation
#install.packages("tidyverse") 
library(ggplot2)
library(lubridate)
library(terra)
#library(sp)
#install.packages("stars")
library(stars)
library(raster)
library(sf)
#install.packages("fasterize")
library(fasterize)
install.packages(rgis)








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


##########################################################################

# Importing North and South Route [Line Shape files]: ------------------------
lineN<- sf::st_read( paste0(datapath, 
                            "BTJR_Aug22_Spotlights_shp/N_RouteLine.shp"))


lineS<- sf::st_read( paste0(datapath, 
                            "BTJR_Aug22_Spotlights_shp/S_RouteLine.shp"))







# Cleaning dfs: -----------------------------------------------------------

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
BTJR_When.df$Duration<- "NA"
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
                Duration, Sampling.Effort, CellID) 

#Configuring Duration.Hrs column from start and end times:
BTJR_When.df$Duration<-abs(difftime(BTJR_When.df$Start_MST.time, 
                                    BTJR_When.df$End_MST.time, units = "mins"))

#there is a problem with a few rows in the duration column
#when it is switching from 23hr to midnight 00 that same night this function is
#calculating the incorrect difftime in mins so i am going to manually change 
#these values for now:
BTJR_When.df$Duration[2]<-"33"
BTJR_When.df$Duration[13]<-"35"
BTJR_When.df$Duration[15]<-"36"



#Creating a RouteID column to specify if this was a North or South route:
BTJR_When.df$RouteID <- NA

for (r in 1:dim(BTJR_When.df)[1]){
  BTJR_When.df$RouteID <- ifelse(BTJR_When.df$Site == "Bigfoot_butte" |
                                BTJR_When.df$Site == "Simco" , "S.Route","N.Route")

}
# | is used as an OR argument in the ifelse statements 
# you would use & if you wanted an AND argument in the ifelse statement 




View(BTJR_When.df)


# Calculating Moon Phase: -------------------------------------------------

t<-BTJR_When.df$Start_MST.time
f<-oce::moonAngle(t=t, 
             longitude = -63.6,
             latitude = 44.65)$illuminatedFraction

plot(t, f, xlab="Day of 2022", ylab="Moon Fraction")
grid()


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

all(sf:: st_is_valid(lineN))#TRUE
all(is.na(st_dimension(lineN)))#no missing geometry
#FALSE: is there NA=false for all

all(sf:: st_is_valid(lineS))#TRUE
all(is.na(st_dimension(lineS)))#no missing geometry
#FALSE: is there NA=false for all




#Checking CRS of all objects :  -----------
sf::st_is_longlat(NCAgrid300m)#TRUE: Jackrabbit,N&Sroute, 
#FALSE: boundary . 300mgrid

sf:: st_crs(Jackrabbits)#WGS84
sf:: st_crs(NCAboundary)#NAD83 / UTM zone 11N + NAVD88 height
sf:: st_crs(NCAgrid300m)#+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs 
sf:: st_crs(route_N)#WGS84
sf:: st_crs(route_S)#WGS84
sf:: st_crs(lineN)#WGS84
sf:: st_crs(lineS)#WGS84









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




#Creating buffer around the N. and S. line routes before transforming them: -----------
lineN_buf<-lineN%>%
  sf::st_buffer(dist = 10)#10m buffer

lineS_buf<-lineS%>%
  sf::st_buffer(dist = 10)#10m buffer


#transforming vector to match raster:
lineN_trans<-sf::st_transform( lineN_buf, st_crs( NCAgrid300m ) )
st_crs(lineN_trans) == st_crs(NCAgrid300m)
#TRUE 

lineS_trans<-sf::st_transform( lineS_buf, st_crs( NCAgrid300m ) )
st_crs(lineS_trans) == st_crs(NCAgrid300m)
#TRUE






#Plotting Raster and Vector objects together to check :  -----------
plot(NCAgrid300m)
plot(st_geometry(NCAb_trans))
plot(st_geometry(Jackrabbits_trans), add=TRUE)

plot(st_geometry(lineN_trans), add=TRUE)
plot(st_geometry(lineS_trans), add=TRUE)
#Finally worked as one continuous line!


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

#########################################################################
#WHAT DO I DO WITH THIS INFORMATION - HOW TO I CROP TO BETTER FIT THE 
#JACKRABBIT DATA?

#######################################################################


##Checking Resolutions :  -----------
res(NCAgrid300m)#300 300
ncol(NCAgrid300m)#350
NCAgrid300m


Jackrabbits_trans


###########################################################################
# NEXT NEED TO CROP THE MAP TO BETTER FIT THE BTJR POINTS ON THE MAP 

NCA_crop<-raster::crop(x=NCAgrid300m, y=NCAb_trans)
plot(NCA_crop)
plot(st_geometry(NCAb_trans), add=TRUE)
plot(st_geometry(Jackrabbits_trans), add=TRUE)

plot(st_geometry(lineN_trans), add=TRUE, col="red")
plot(st_geometry(lineS_trans), add=TRUE)

############################################################################
# NEED TO FIGURE OUT HOW TO CHANGE THE COLORS OF EACH LAYER OR HOW TO USE 
# GGPLOT WITH SPATIAL DATA 
###########################################################################






##############################################################################
##############################################################################
##############################################################################
# BELOW CODE = WORKING TOWARDS ANALYSIS STEPS - CALCULATING SAMPLING EFFORT
#               AND ASSIGNING TRANSECTING GRID CELLS TO POLYGON ROUTE LINES
##############################################################################

#Converting transects to area (polygons) :  -----------

#Nline_trans and Sline_trans = already a polygon sf data.frame
#   - don't need to convert here it looks like 



#transect each poly.with a grid cell :  -----------

class(lineN_trans)#"sf" "data.frame"

#Looks like i need to rasterize these polygons before next steps(?):
stars::st_rasterize()
######## ???



sf::st_intersects(x=st_geometry(lineN_trans), y=NCA_crop, sparse = TRUE, simplify=FALSE )
#Error in UseMethod("st_geometry") : 
#no applicable method for 'st_geometry' applied to an object of class 
#"c('RasterLayer', 'Raster', 'BasicRaster')"


#sf::st_transect(..., simplify=FALSE)
st_intersecting(x, y, sparse = TRUE, ...)


#############################################################################3
# DONT KNOW HOW TO GET THE RASTER LAYER TO OVERLAY WITH POLYGON SF DATA.FRAME
# - INTERSECTS OR TRANSECTS SF OBJECT WITH RASTER GRID CELLS 
# - DONT KNOW HOW TO ASSIGN GRID CELL ID'S TO LINES 
# - DO I NEED TO CONVERT RASTER LAYER TO A VECTOR SF OBJECT TO DO THIS?
# 
# AFTER THIS I CAN CALCULATE AREA OF EACH POLY IN GRIDCELLS USING ST_AREA
#############################################################################3



#Estimate the area of intersecting polys. with a grid cell :  -----------

#This step is to figure out how much of each poly. in each cell;
#   -Want area pieces of geometery
sf::st_area()


# To get proportion and sampling effort :  -----------
terra::extract()

#####################################################

polygon_to_raster(lineN_trans, NCAgrid300m, field = NULL, fun, background = NA_real_,
                  by = NULL)










##############################################################################
##############################################################################
##############################################################################





##############################################################################
# THIS IS THE OLD WAY (WITH OUT USING ROUTE LINE SHAPE FILES CREATED IN ARCGIS)
#TO CONVERT THE POINTS COLLECTED FROM GPS UNITS OF N. AND S. ROUTES 
# WAS NOT ABLE TO GET THESE TO PLOT ON THE RASTER STILL - SO I CREATED POINT TO
#LINE SHAPE FILES FOR THE N. AND S. ROUTES IN ARCGIS TO BY PASS THIS PROBLEM 
# FOR NOW - TO HASSEN ANALYSIS STEPS NEEDED TO TAKE ABOVE
##############################################################################

# Convert Route and BTJR Points to Raster CRS -----------------------------
##Northern Route:  [LINES!] -----------

route_N$lon<-as.numeric(route_N$lon)
route_N$lat<-as.numeric(route_N$lat)

pointz_Nroute<-cbind(x=route_N$lon, y=route_N$lat)


crs<-"+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs "


Nroute_line<-terra::vect(pointz_Nroute, crs=crs, type="line")

plot(Nroute_line)# is in WGS84
class(Nroute_line)#NOW A SPATRASTER/TERRA OBJECT 
# [1] "SpatVector"
# attr(,"package")
# [1] "terra"


plot(NCAgrid300m)
plot(NCAgrid300m, add=TRUE)#WONT PLOT LINES ON TOP OF NCA GRID RASTER

#checking crs matches raster:
terra::crs(Nroute_line)#WGS84
st_crs(Nroute_line) == st_crs(NCAgrid300m)
#TRUE





##Southern Route:  [LINES!] -----------

Sroute_trans$lon<-as.numeric(Sroute_trans$lon)
Sroute_trans$lat<-as.numeric(Sroute_trans$lat)

pointz_Sroute<-cbind(x=Sroute_trans$lon, y=Sroute_trans$lat)

crs<- "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
  #WONT WORK:st_crs(NCAgrid300m)


Sroute_line<-terra::vect(pointz_Sroute, crs=crs, type="line")

plot(Sroute_line)# is in WGS84

class(Sroute_line)#NOW A SPATRASTER/TERRA OBJECT 
# [1] "SpatVector"
# attr(,"package")
# [1] "terra"


#checking crs matches raster:
st_crs(Sroute_line) == st_crs(NCAgrid300m)
#TRUE

plot(NCAgrid300m)
plot
#terra::plot(Sroute_line, col,pch=16, alpha=0.5, lwd=1, lty=1, add=TRUE)#WONT PLOT LINES 

###############################################################################
#Trying to make raster 300m grid a spatvector to match terra:: spatvector the lines created

Sroute_line<-terra::rast()
#this did not help plot the line routes on top of the 300m grid

##############################################################################



































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




# #Making histogram of Jackrabbit obs: ------------------------------------

#Hist. of Day of yr.:
ggplot(data = Jackrabbits, aes(x = DayOfYr))+
  geom_histogram(binwidth = 1.5, color="black", fill="seagreen")+
  theme_bw()+
  labs(x= "Day of Year (Aug.2022)", 
       y= "Black-tailed Jackrabbit Counts", 
       title = "Histogram of Jackrabbits per Day of Year for August 2022 Spotlight Surveys")

#Hist. of hour of day:
ggplot(data = Jackrabbits, aes(x = Hour))+
  geom_histogram(binwidth = 1.5, color="black", fill="seagreen")+
  theme_bw()+
  labs(x= "Hour of Night (Aug.2022)", 
       y= "Black-tailed Jackrabbit Counts", 
       title = "Histogram of Jackrabbits per Hour of Night for August 2022 Spotlight Surveys")
#NEED TO DEFINE X AXIS BORDERS - that show hour: 20-5 continuously to show 
#distubution of #BTJR seen during these times

#Hist. of Survey Night using jackrabbit df:
#Jackrabbits$SurveyNight<-as.factor(Jackrabbits$SurveyNight)
ggplot(data = Jackrabbits_trans, aes(x = as.numeric(DayOfYr) ))+
  geom_histogram()
#Does not work 

ggplot(Jackrabbits, aes(SurveyNight))+
  geom_bar(color="black", fill="seagreen")+
  theme_classic()+
  labs(x= "Survey Night (Aug.2022)", 
       y= "Black-tailed Jackrabbit Counts", 
       title = "Bar Graph of Jackrabbits per Survey Night for August 2022 Spotlight Surveys")


#Duration of time Sites were surveyed 
#ggplot( data = BTJR_When.df, aes(x = RouteID, y = Duration.Hrs)
#ggplot(BTJR_When.df, aes(x=))

ggplot(BTJR_When.df, aes(paste0(Duration)))+
  geom_bar()
#not really what i am looking for i dont think 
ggplot(BTJR_When.df, aes(x=RouteID, y=paste0(Duration), color=RouteID))+
  theme_bw()+
  geom_point()+
  labs(x= "Routes", 
       y= "Duration (mins) Spent Surveying the Sites Found within each Route", 
       title = "Survey Duration Time per Route for August 2022 Spotlight Surveys")
#worked but, the points look like they should be bars but this ggplot wont 
#work if i use geom_bar right now


#site vs jackrabbit obs:
ggplot()






# Correlation Plots: ------------------------------------------------------

#Weather conditions; wind, temp:
cor.test(BTJR_When.df$Start_temp.F., BTJR_When.df$Start_wind.km.h.)
#cor =0.137755
#Not strongly correlated 
# A correlation coefficient higher than 0.8 or lower than -0.8 
#is considered strongly correlated 


#duration and route ID:
# BTJR_When.df$Duration<-as.numeric(BTJR_When.df$Duration)
# cor.test(BTJR_When.df$Duration, BTJR_When.df$RouteID)
#DOESNT WORK - DONT USE FACTOR VARIABLES (EX:ROUTEID) IN COR TEST




#moon phase and jackrabbit obs: ?


#time of night and jackrabbit obs.: ? 












# Saving Data -------------------------------------------------------------
## Save csv's:   -----------

# Save cleaned csv:
write.csv( x = Jackrabbits , file = "Jackrabbits.Aug.csv" )





## Save work space:   -----------
# saving all data to the path
#save.image("300mgrid_Jackrabbit.RData")




# loading the workspace
load("300mgrid_Jackrabbit.RData")









