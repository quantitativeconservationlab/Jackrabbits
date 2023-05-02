#Created Mar.1,2023 by Leticia Camacho to quickly visualize some of the data 
# that has been cleaned in the last 3 spotlight optimazation scripts







# Setup -------------------------------------------------------------------

## Set up your work space and load relevant packages -----------

## Clean your work space to reset your R environment. 
rm( list = ls() )


## Load packages relevant to this script:  -------------

library( tidyverse ) #package for easy data manipulation
# set option to see all columns and more than 10 rows
options( dplyr.width = Inf, dplyr.print_min = 100 )
library(oce)# used for moon phase information
library(ggplot2)


# Setting Working Directory -----------------------------------------------


# Set working directory. This is the path to your Rstudio folder for this 
# project. If you are in your correct Rstudio project then it should be:

getwd()#"C:/Users/leticiacamacho/Documents/BTJR_MSProject/Rcode/Spotlights_Hab_2022/Jackrabbits"

# if so then:
workdir <- getwd() 
#creating working directory as an object so you can call it easier 
#     - without having to type it out or re-run each time.



# Load and Create Data: ---------------------------------------------------

# Set data paths:  -----------

#Creating pathway to call the BTJR data from the common drive
datapath <- "Z:/Common/Jackrabbits/Data/Spotlights/" 

#Calling Rab Loc data:  
Abtjr <- read.csv( file = paste0(datapath, "Aug22/Arab.csv"))
Jbtjr <- read.csv( file = paste0(datapath,"June22/J_btjr.csv"))

#Calling site data:
JRoute<-read.csv( file = paste0(datapath, "June22/Jroutes.csv"))
ARoute<-read.csv( file = paste0(datapath, "Aug22/Aroutes.csv"))




#cleaning cottontails and unknowns out of J_btjr becasue it did not save for some reason right now
Jbtjr<- Jbtjr %>% filter(Rab.Obv=="Jackrab")
#check:
unique(Jbtjr$Rab.Obv)#worked 



# Visualizations: ---------------------------------------------------------


#duration per route:

#amount of btjr per survey night:


