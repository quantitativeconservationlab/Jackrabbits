############################################################################
#### code used to try and attemp to convert routes to lines from points
### the code that works and is simple is in the BTJRDataPrep_300mGrids script




# Convert Route and BTJR Points to Raster CRS -----------------------------

##Northern Route:  -----------

routeN_coord<-route_N # THIS WORKED !BUT WANT TO SEE IF WILL WORK WITH LINE TOO =======  IT DOES NOT
#replacing route_N , with Nroute_line does not work ===== Error in UseMethod("st_as_sf") :  no applicable method for 'st_as_sf' applied to an object of class "c('XY', 'MULTILINESTRING', 'sfg')"

#routeN_coord<-Nroute # also doesnt work : gives same error message:======Error in UseMethod("st_as_sf") : no applicable method for 'st_as_sf' applied to an object of class "list"

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




## Southern route:  ----------- 
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




###############################################################################################

# Changing Routes to Lines While Keeping CRS: -----------------------------

## Checking current class of spatial object:  -----------
class(Scoordinates_aes)#[1] "sf"         "data.frame"
#Scoordinates = also "sf" dataframe
class(Scoordinates_aes$geometry)#[1] "sfc_POINT" "sfc" 

## Changing Points to lines:
S_coord_line<-sf:: st_cast(Scoordinates_aes$geometry, "LINESTRING")
class(S_coord_line)#[1] "sfc_LINESTRING" "sfc"
terra::crs(S_coord_line)#WGS84

#View:
plot(NCAgrid300m)

plot(st_geometry(S_coord_line), add=TRUE)#DOES NOT WORK - Error in CPL_geos_is_empty(st_geometry(x)) : Evaluation error: IllegalArgumentException: point array must contain 0 or >1 elements.


#####################################################################################################


############################################################################
## PAST WAY: CHANGING ROUTE POINTS TO LINES - BUT LOOSE CRS
# Manipulating Data: ------------------------------------------------------


##Convert geometry from points to a line for routes:  -----------


#southern route:  -----------

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
class(Sroute_line)
#[1] "XY"              "MULTILINESTRING" "sfg" 
crs(Sroute_line)
# Error in h(simpleError(msg, call)) : 
#   error in evaluating the argument 'x' in selecting a method for function 'crs': object 'Sroute_line' not found

##SAVED IMAGE IN COMMON > JACKRABBITS > AUG SPOTLIGHT SURVEYS > R PLOTS > SouthernRoute(multiline string)


#############################################################################
####### MATT CLARKS CODE #############

route_N$lon<-as.numeric(route_N$lon)
route_N$lat<-as.numeric(route_N$lat)

pointz_Nrroute<-cbind(x=route_N$lon, y=route_N$lat)


TEST_crs<-"+proj=longlat +datum=WGS84"

TEST_Nroute_v<-terra::vect(pointz_Nrroute, crs=TEST_crs, type="line")#THIS PLOT WORKED BUT NOT FULL LINESTRING - STILL LOOKS LIKE POINTS 

y<-project(TEST_Nroute_v, "+proj=longlat +datum=WGS84")# ERROR : XY NOT NUMERIC


plot(TEST_Nroute_v)# is in WGS84


class(TEST_Nroute_v)#NOW A SPATRASTER/TERRA OBJECT 
# [1] "SpatVector"
# attr(,"package")
# [1] "terra"


plot(NCAgrid300m)
plot(TEST_Nroute_v, add=TRUE)#WONT PLOT on top of raster still 

lonlat<-geom(y)[,c("x", "y")]#Y NOT AN OBJECT BECASUE ABOVE ERROR 

###########################################################################

#northern route:  -----------

#checking current geometry classification
class(st_geometry(route_N)) 
#"sfc_POINT" "sfc"

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

#combining all the line strings together to create the continuous route 
Nroute_line <- st_multilinestring(do.call("rbind",Nroute))

#view new line
plot(Nroute_line)
class(Nroute_line)
#[1] "XY"              "MULTILINESTRING" "sfg" 

##SAVED IMAGE IN COMMON > JACKRABBITS > AUG SPOTLIGHT SURVEYS > R PLOTS > NorthernRoute(multiline string)





#######################JC NOTES FROM MEETING: ########################
Nroute_2 <- do.call( what = sf:::rbind.sf,
                     args = Nroute )

#### IT LOOKS LIKE THIS IS WHERE WE LOOSE THE CRS OF THE LINE 
#### WHEN WE CHANGE FROM N_ROUTE TO NROUTE_LINE
head( Nroute)


### NEED TO FIX THIS HERE SO THAT THE CRS IS THE SAME AS IT WAS BEFORE CONVERTING
# TO MULTILINE STRINGS  

### NEED ASK MATT HOW TO CONVERT CRS OF  A MULTI LINE STRING TO WGS 84

#################################################################




