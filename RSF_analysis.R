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
 
alldf <- sf::st_read( "allpoints.shp" ) 

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

# estimate 95% CIs for fixed effects of top model
confint( m1 ) #uses the Wald method

#calculate marginal R^2 for random effects only (R2m) and the whole model
# including fixed and random (R2c)
MuMIn::r.squaredGLMM( m1 )
#What do these values tell us

# Interpreting results ###

# 
# since we only have one predictor we don't have to account for
# others in the model and can just exponentiate it to compare the #
# relative intensity or rate of use of two locations that differ by 1 
# SD unit of the explanatory variable but are otherwise equivalent - i.e. #
# they are equally accessible and have identical values for all #
# other explanatory variables. # 
exp( glmmTMB::fixef( mslps )$cond[2] )
exp( glmmTMB::fixef( mslps )$cond[3] )
# this reflects the relative selection strength 


##########################################################
### Save desired results                                  #
# we can save the movement model results
#save workspace if in progress
save.image( 'RSFresults.RData' )
############# end of script  ##################################
