# Setup -------------------------------------------------------------------

## Set up your work space and load relevant packages -----------

## Clean your work space to reset your R environment. 
rm( list = ls() )




## Load packages relevant to this script:  -------------

library( tidyverse ) #package for easy data manipulation
#install.packages("tidyverse") 
library(ggplot2)
library(sf)
library(terra)
library(raster)
library(lubridate)







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

BigRabdf_extended<-read.csv(datapath, "BigRabdf_extended.csv")

## Importing Raster Data:  -----------

#import 300mx300m grid cell NCA raster from NLCD_new folder:  -----------
NCAgrid300m<- raster::raster( paste0(rastpath,
                                     "c20_agg.img")) 

#import NCA shapefile from habitat folder:  -----------
NCAboundary <- sf::st_read( paste0( habpath, 
                                    "NCA/GIS_NCA_IDARNGpgsSampling/BOPNCA_Boundary.shp"))

#view:
View(site)
View(Jackrabbits)
View(BigRabdf_extended)





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
       





# Cleaning dfs: -----------------------------------------------------------

##############################################################################

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






################################################################


#Creating cleaned Rab.df:  -----------

#First need to combine geometry column together because it got split somehow:
#Jackrabbits$geometry<-paste(Jackrabbits$geometry, Jackrabbits$X, sep = ",")
#remove un-needed X column
#Jackrabbits<-subset(Jackrabbits, select = -X)


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

Jackrabbits %>%
  filter(SurveyNight == "1")



#############################################################################







plot(NCAgrid300m)
plot(st_geometry(Jackrabbits), add=TRUE)
 class(Jackrabbits)










BTJR_When.df <- site %>%
  dplyr::select(Survey_ID, Date, Crew_name, Site, Start_temp.F., 
                Start_wind.km.h., Spotlight_Start.Time, 
                Spotlight_End.Time, Night_number)







# Cleaning Rab.df ---------------------------------------------------------

##Selecting columns of interest from original df:
Rab.df<-Jackrabbits %>%
  select(RabID, lat,lon, Species=Rab.Obv, Rab.name=name, Date,MST.time, 
         Hour,DayOfYr)

#view
head(Rab.df )
#fix date to lubridate
Rab.df$MST.time <-lubridate::mdy_hm( Rab.df$MST.time, tz = "MST" )






################################################################
















# Checking Geometries: ----------------------------------------------------

#Changing Jackrabbit df from dataframe to sf object :  -----------
sf::st_as_sf(Jackrabbits$geometry)
##########################################################################3
#SOMETHING IS WRONG WITH THE JACKRABBITS DF BECASUE WHEN I WROTE THE CLEAN AUG CSV IT LOOSES THE GEOMETRY COLUMN 

##############################################################################













##Checking if geometry of spatial objects are valid :  -----------
sf::st_is_valid()


#Checking CRS of all objects :  -----------
























# Saving Data -------------------------------------------------------------
## Save csv's:   -----------

# Save cleaned csv:
write.csv( x = Jackrabbits , file = "Jackrabbits.Aug.csv" )





## Save work space:   -----------
# saving all data to the path
save.image("300mgrid_Jackrabbit.RData")




# loading the workspace
load("300mgrid_Jackrabbit.RData")









