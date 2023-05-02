# Jackrabbits

This repository is used to store scripts from Leticia Camacho’s MS jackrabbit project. 
It includes old scripts that were developed prior to Spring2023 to assess the spotlight survey data collected in Summer 2022.
These scripts were developed at the beginning learning stage in Leticia’s coding career and jackrabbit project. 
So, they were moved to the Old Scripts folder after the usable code was collected from them and streamlined into new updated scripts in 2023. 

The Andrew Baker folder was created to hold our project’s 2022 undergraduate scripts for his summer project. 
This method and scripts are no longer relevant to the current 2023 project analysis goals. 

The Spotlight Optimization folder will house all the current usable code for working towards analyzing 2022 jackrabbit spotlight survey data 
to determine the optimal spotlight methodology. 


# Spotlight Optimization Project : Data Description 
---------------------------------------------
These scripts waere developed to clean and visualize black-tailed jackrabbit 
count data collected using spotlight surveys conducted at dusk (10pm-2am) 
and dawn(2am-6am) in the Morley Nelson Birds of Prey NCA. 
Spotlight surveys were conducted by two teams of two trained technicians each
(1 lead and 1 undergrad. pairs) during 8 day period in June and August 2022. 
There are 4 total sites surveyed in June2022. All 4 sites were surveyed each night
either by the dawn or dusk crew (road/weather conditions permitting). 
Survey crews coordinated to randomize the order,start time, and start locations
of each night and site that was surveyed (planning was done before hand @ 
beginning of season).
In August2022 we switched from focusing on site based data collected to route based collection 
Where we still surveyed the same sites as in June but we also rigourously collected any 
rabbit observations in between these sites, which we named: North Route and South Route 
In Aug2023 we will be adding a third route that encompasses Golden Eagle territories. 


# Aug2022 Spotlight Survey Details: 
-------------------------------------------------
The first two night of surveys were done as a team of 4 - with all technician in the 
same truck, visiting all four sites together to standardize training measures
and increase confidence in Identification skills in the field.
along each site specific route the technicians drove no faster that 10mph 
while surveying in bewteen sites (along the route) incidental rabbit observations,
while technicians were driving no faster than 35mph, were also recorded on the GPS


# June2022 Survey Details : 
-------------------------------------------------
We relied on tablets rather than GPS units to gather GPS points of rabbits 
observed along the routes. This led to a lack of data for this survey period
- specifically the exact time that the GPS point was taken during the surveys



# Following is individual breakdowns/background information for each script currently found within the SpotlightOptimization folder:
---------------------------------------------
# Arab: -------------------------------------------------
A csv file containing the cleaned jackrabbit observations data from August 2022 spotlight surveys collected on GPS units in the field for each individual jackrabbit observation. 
Includes: Rab.Obv (all =Jackrab, because the total number of jackrabbits and cottontails observations from Aug22 was filtered to only include jackrabbits), name (this is the name of the observation inputted in the GPS in the field. Ja=Jackrabbit Adult, Jj= jackrabbit juvenile, Junk= jackrabbit unknown age class, Ca=Cottontail adult , Cj= cottontail juvenile, Cunkn=Cottontail  unknown age class, XXXX i = An (i) is added at the end of a species/age class when it was an incidental (meaning it was not not on one of the individual sites used in the beginning of 2022 - we switched to a North and South Routes method instead in late 2022), XXXX i e = (e) is added at the end of a GPS code if it was an incidental observed on the way out of the field or at the end of the route surveys, XXXX i b = (b) is added at the end of a GPS code if it was an incidental observed on the way into the field or at the beginning of the route surveys), Date, MST.time (time of GPS point taken in the field of the individual jackrabbit observation, converted to Mountain Standard Time), Hour (hour of night extracted from MST.time), lat, lon, geometry, SurveyNight (night number of when observation was taken - this was done because there is a 2 survey crews [dusk (10pm-2am) and dawn(2am-6am)] operating out in the field around the same time, and the dusk crew’s survey time overlapped 2 dates because they surveyed 10pm-2am. So, I developed the SurveyNight to make it easier to determine what day within the survey period the GPS points were made. 

# Jrab:-------------------------------------------------
A csv file containing the cleaned jackrabbit observations data from June 2022 spotlight surveys taken on tablets for each individual jackrabbit observation in the field. 
Includes: Date, Crew_name (dawn or dusk), Rab.Obs (same as described in Arab except incidentals were labeled with and IO, and in june we did not know that we would need an (e or b) label to show us if we were beginning or ending a survey route, so there is no labels that show this in this data set), lat, lon, Night-number (same as SurveyNight in Arab in which each observation is given a night number of when observation was taken - this was done because there is a 2 survey crews [dusk (10pm-2am) and dawn(2am-6am)] operating out in the field around the same time, and the dusk crew’s survey time overlapped 2 dates because they surveyed 10pm-2am. So, I developed the SurveyNight to make it easier to determine what day within the survey period the GPS points were made, RouteID (either S.Route = southern route, or N.Route=Northern route) DayofYr. 

Because we used tablets to collect the location data of each rabbit observation in June22 we were not able to extract the exact time of the observation but we were able to write code to determine which route the points were from with the date. 


# Jrab_sf  & Arab_sf: ------------------------------------------
These files are the exact same as Arab and Jrab (described above) but they were altered to be sf objects and then saved in that form to be called in later scripts if needed. 

# SpotlightOpt_RabLoc : ----------------------------------------
Pulls data from dusk and dawn GPS’s, datasets,  and shape files instead of altered CSv’s from other previous scripts. 
Other scripts may be irrelevant prior to this because of this data pulling direct method. 
Aug22 and June22 site level data and rabbit observation have been proofed, formatted and combined together with each season's data. Into 2 separate DFs for each month’s spotlight surveys. Jackrabbits have been filtered out from cottontails. 

Save cleaned csv's:   -----------
Rab.locations:
write.csv( x = AJrab, file = paste0(datapath, "Aug22/AJrab.csv" ) )
write.csv( x = JJrab, file = paste0(datapath, "June22/JJrab.csv" ) )

Site info:
write.csv( x = JRoutes, file = paste0(datapath, "June22/Jroutes.csv" ) )
write.csv( x = ARoutes, file = paste0(datapath, "Aug22/Aroutes.csv" ) )

Rabloc & Site combined info:
write.csv( x = A, file = paste0(datapath, "Aug22/AJrabRoutes.csv" ) )
write.csv( x = J, file = paste0(datapath, "June22/JJrabRoutes.csv" ) )

Save work space:   -----------
save.image("Opt_RabLoc")


# SpotlightOpt_Raster :---------------------------------------
Importing: Aug22  and June22 spotlight jackrabbit observation location GPS and site level data, North and South Route shape files, NCA boundary line and 300m raster files (to be overlaid with by spotlight survey jackrabbit location/site data).  

Background --------
This script in the 3rd of 3 spotlight methodology optimization scripts 
This script's purpose:
        - Develop spatial objects from the jackrabbit location (rabloc) GPS data to be used in next steps (Spatial analysis) with correct geometries/CRS’s as 300m NCA raster file
       - configuration of Jrab and Arab csv's to sf objects
       - combine rabloc to NCA 300m raster  
       - plot routes 
       - calculate duration and sampling efforts by assigning raster cellID to  RabLoc data for 2022 spotlight surveys

Saving Data --------
write csv /sf object dataframe for NCAb_rast, Jrabsite_rast, Arabsite_rast
(Jrabsite_rast = combined site level and jackrabbit location data into one csv that was converted to the correct format to match the 300m NCA raster. So they can be used in the SpotlightOpt_Raster2.R script next 

Nroute_rast, Sroute_rast and NCA_rast:--------
st_write(NCAb_rast, 
         dsn = paste0(datapath, "Spotlights/Spatial.Data/NCAb_rast.shp"))

st_write(Nroute_rast, 
         dsn = paste0(datapath, "Spotlights/Spatial.Data/Nroute_rast.shp"))

st_write(Sroute_rast, 
         dsn = paste0(datapath, "Spotlights/Spatial.Data/Sroute_rast.shp"))

June and Aug22 site level data:--------
st_write(Jrabsite_rast, 
         dsn = paste0(datapath, "Spotlights/Spatial.Data/Jrabsite_rast.shp"))

st_write(Arabsite_rast, 
         dsn = paste0(datapath, "Spotlights/Spatial.Data/Arabsite_rast.shp"))

Save work space:   -----------
save.image("SpotlightOpt_Raster.RData")


# SpotlightOpt_Raster2 : -------------------------------------------------
This script works off the files that were created and saved in SpotlightOpt_Raster.R script. 

Background -----------
This script in the 4th of 3 spotlight methodology optimization scripts 
This script's purpose:
 - create empty raster that matched specs of original raster (300m rast). Ref. chapter 6 for notes and steps:https://r.geocompx.org/raster-vector.html#rasterization
 - add route shape files (polygons) to the blank raster (~"overlap")
 - calc how much of the route lines fall within each grid cell (terra::extract)(rasterization)



# ----------- Ended off here ----------- (March2023) -----------










 

# Github Code Source: -------------------------------------------------
#Using the quantitativeconservationlab / Jackrabbits GitHub Repository 
#to store scripts. URL:https://github.com/quantitativeconservationlab/Jackrabbits.git
