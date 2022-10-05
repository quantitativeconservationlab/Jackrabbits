# Setup -------------------------------------------------------------------

## Set up your work space and load relevant packages -----------

## Clean your work space to reset your R environment. 
rm( list = ls() )




## Load packages relevant to this script:  -------------

library( tidyverse ) #package for easy data manipulation
#install.packages("tidyverse") 
library(ggplot2)








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





# Importing Data: ---------------------------------------------------------

## Import Rabbit Data:  -----------

site<- read.csv(paste0(datapath, "Site_Aug2022.csv"))

Jackrabbits<-read.csv(paste0(datapath, "BTJR.obs_AUG_Clean.csv"))

BigRabdf_extended<-read.csv(paste0(datapath, "BigRabdf_extended.csv"))


#view:
View(site)
View(Jackrabbits)
View(BigRabdf_extended)


#Making Histograms of Weather Variables:

hist(BigRabdf_extended$Start_temp.F.)
hist(BigRabdf_extended$Start_wind.km.h.)

ggplot(data=BigRabdf_extended, aes(x=Start_temp.F.))+
  geom_histogram(binwidth = 2, color="black", fill="seagreen")+
  theme_bw()+
  labs(x= "Start Temp. (F)", 
       y= "Counts", 
       title = "Histogram of Starting Tempatures (deg. F) 
       for August 2022 Spotlight Surveys")
#This is so much uglier then the base hist()



ggplot(data=BigRabdf_extended, aes(x=Start_wind.km.h.))+
  geom_histogram(binwidth = 2, color="black", fill="seagreen")+
  theme_bw()+
  labs(x= "Start Wind. (Km/hr)", 
       y= "Counts", 
       title = "Histogram of Starting Wind (Km/hr) 
       for August 2022 Spotlight Surveys")









#Cleaning dfs 

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
