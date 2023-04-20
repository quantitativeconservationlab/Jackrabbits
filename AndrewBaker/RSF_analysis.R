##################################################################
# Script developed by Jen Cruz to estimate habitat selection     #
#  by jackrabbits at the NCA using jackrabbit detections and    #
# non detections over an 8-day period in June 2020.             #
# Data were prepared in PilotDataPrep.R and HabPrep.R          #
# Analysis is done using glmmTMB following Muff et al.2019     #
# go here: https://conservancy.umn.edu/handle/11299/204737      #
# for detailed code discussed in the manuscript                 #
###################################################################

################## prep workspace ###############################

# Clean your workspace to reset your R environment. #
rm( list = ls() )
# we will be using new packages:
#install.packages( "glmmTMB" )
# load packages relevant to this script:
library( tidyverse ) #easy data manipulation
library( sf )
library( rasterVis ) #for raster visualization (of raster:: objects)
library( glmmTMB ) # for analysis
library(MuMIn) #for model validation

 ####################################################################
 ## end of package load ###############
 
 ###################################################################
 #### Load or create data -----------------------------------------
 
alldf <- sf::st_read( "allpoints_30m.shp" ) 

######## preparing data ###############################################

# Scale predictors to optimize model convergence and for easy 
# comparison of fixed coefficients:
alldf$shrub_sc <- scale( alldf$shrub_vals )
alldf$inv_sc <- scale( alldf$inv_vals)  
#convert use to 1 and 0
alldf <- alldf %>% 
  mutate( use = ifelse( use == "yes", 1, 0 ) )
# we also assign weights to available points to be much greater than #
# used points
alldf$weight <- 1000 ^( 1 - as.integer(alldf$use ) )
#check
head( alldf );tail(alldf)
#########################################################################
# Population-level RSF

# we start with a full model with a site random intercept
m1 <- glmmTMB( formula = use ~ shrub_sc + inv_sc +  
                 #random effects:
                    (1 | Site ),
               #what distribution:
                   family = binomial(), 
               data = alldf,  weights = weight ) 

summary( m1 )
# What does the variance for the site random intercept tell us?

# check for possible differences in slopes among sites:
mslps <- glmmTMB( use ~ shrub_sc + inv_sc +  
            (1 | Site ) + ( 0 + shrub_sc+ inv_sc | Site ),
            family = binomial(), 
            data = alldf,  weights = weight ) 

summary( mslps )
#Are random slopes supported? How can we tell?

#what about model fit?
# We compare against a null model without fixed predictors
m0 <- glmmTMB( use ~ 1 +
                 (1 | Site ),
               family = binomial(), 
               data = alldf,  weights = weight ) 

#compare models using AIC
anova( m1, m0, mslps )
anova( m1, mslps )
#calculate marginal R^2 for random effects only (R2m) and the whole model
# including fixed and random (R2c)
MuMIn::r.squaredGLMM( mslps )
#What do these values tell us

# Interpreting results ###
# extract relative selection strength of our habitat 
# variables averaged across all sites
exp( glmmTMB::fixef( mslps )$cond[2] )
exp( glmmTMB::fixef( mslps )$cond[3] )
# interpretation is how many more times a jackrabbit is 
# using each particular habitat
# Since we know random slopes are important. These estimates#
# are actually not that meaningful. Instead let's look at #
# what site-level relationships tell us:

#To do this we could estimate the change in the average probability of #
#selection as we change one of the habitat covariates #
# while averaging over other habitat covariates according#
# to their availability as per Avgar et al. 2017. #

# however, let's start simple by looking at partial prediction#
#plots for our sites. 

#First start by extracting random effect estimates from top model:
redf <- as.data.frame(ranef(mslps, condVar = FALSE))
#view
redf
#add fixed effects
redf$effects <- redf$condval + fixef(mslps)$cond[1] 
#replace apppropriate site values
redf$effects[which(redf$term == "shrub_sc" )] <-  
  redf$condval[which(redf$term == "shrub_sc" )] + 
    fixef(mslps)$cond[2] 
redf$effects[which(redf$term == "inv_sc" )] <-  
  redf$condval[which(redf$term == "inv_sc" )] + 
    fixef(mslps)$cond[3] 

#check
redf
#add nice habitat names
redf <- redf %>% 
  dplyr::mutate( habitat = ifelse( term == "shrub_sc",
      "Shrubs", "Invasive annuals" ) )
#check
redf

#view
ggplot( redf[5:12,], aes( x = grp, y = exp(effects) ) ) +
  theme_bw( base_size = 17 ) + 
  labs( x = "Sites", y = "Relative selection strength" )+
  geom_point() +
  facet_wrap( ~habitat )


#what range of habitat values do we have
min( alldf$shrub_vals, na.rm = TRUE ); max( alldf$shrub_vals, na.rm = TRUE  )
min( alldf$inv_vals, na.rm = TRUE  ); max( alldf$inv_vals, na.rm = TRUE  )

#plot partial predictions
sl <- 50
int <- rep( 1, sl )
shrb <- seq( min( alldf$shrub_vals, na.rm = TRUE  ), max( alldf$shrub_vals, na.rm = TRUE  ),
             length.out = sl )
shrb_sc <- scale(shrb )
invs <- seq( min( alldf$inv_vals, na.rm = TRUE  ), max( alldf$inv_vals, na.rm = TRUE  ),
             length.out = sl )
invs_sc <- scale( invs )
#estimate prob of use for shrub
shrb_resp <- plogis( cbind( redf$effects[1:4], redf$effects[5:8]) %*%
                    t(cbind( int, shrb_sc ) ) )
# shrb_resp <- plogis( cbind( 1, redf$effects[5:8]) %*%
#                     t(cbind( int, shrb_sc ) ) )

#for invasives
invs_resp <- plogis( cbind( redf$effects[1:4], redf$effects[9:12]) %*%
                    t(cbind( int, invs_sc ) ) )
# invs_resp <- plogis( cbind( 1, redf$effects[9:12]) %*%
#                     t(cbind( int, invs_sc ) ) )

#create dataframe to store predicted results
resultsdf <- data.frame( t( shrb_resp ), t(invs_resp),
  shrb, shrb_sc,  invs,invs_sc )
     
#add site names
colnames( resultsdf)[1:4] <- paste( redf$grp[1:4], "shrub", sep = "_" )
colnames( resultsdf)[5:8] <- paste( redf$grp[5:8], "invasives", sep = "_" )

#view
tail(resultsdf );dim(resultsdf )
#convert to long format for plotting
rsdf <- pivot_longer( data = resultsdf, cols = 1:8, 
            names_to = c( "Site", "Habitat" ),
            names_sep = "_",
            values_to = "Use" )

rsdf

#plot
ggplot( rsdf ) +
  theme_classic( base_size = 17 ) +
  geom_line( aes( x = shrb, y = Use, color = Site ),
             size = 2 ) +
  #geom_line( aes( x = shrb, y = Use, color = Site ) ) +
  facet_wrap( ~Habitat )

#what variability in sites
alldf %>% group_by(Site ) %>% 
  summarise( min_shrub = min(shrub_vals, na.rm = TRUE ),
             max_shrub = max(shrub_vals, na.rm = TRUE ), 
             min_inv = min(inv_vals, na.rm = TRUE ),
             max_inv = max(inv_vals, na.rm = TRUE ))

##########################################################
### Save desired results                                  #
# we can save the movement model results
#save workspace if in progress
save.image( 'RSFresults_30m.RData' )

############# end of script  ##################################
