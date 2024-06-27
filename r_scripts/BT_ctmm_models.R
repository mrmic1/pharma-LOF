# Lake BT 
# Run CTMM models for each species 

###  LIBRARIES ###

library(data.table)
library(tidyverse)
library(ctmm)
library(sf)
#for parallel processing
library(parallel)
library(foreach)
library(doParallel)

#set time zones
Sys.setenv(TZ = 'Europe/Stockholm')

#create data path for transmitter data
data_filter_path = "./data/tracks_filtered/"
save_ctmm_path = "./data/ctmm_fits/"
save_telem_path = "./data/telem_obj/"

#-------------------------------------------------------------------------------#

#### Northern pike #####

#Load in the datasets
lake_BT_sub <- readRDS(paste0(data_filter_path, 'lake_BT_sub.rds'))

#isolate pike
pike_lake_BT <- lake_BT_sub %>% 
  dplyr::filter(Species == 'Northern Pike'| individual_ID == 'Reference')

#Eric - movebank method. Provide as.telemetry with columns names it works better with
#No need to coerce it into a move object first.
pike_movebank <- with(pike_lake_BT, data.frame("timestamp" = timestamp, "location.long" = Long,
                                               "location.lat" = Lat, "GPS.HDOP" = HPE,
                                               "individual-local-identifier" = Fish_ID,
                                               "treatment" = Treatment,
                                               "date" = date,
                                               "week" = week,
                                               "individual_day" = individual_day))

# pike_lake_BT_tel_utm <- as.telemetry(pike_movebank, 
#                                        timezone = "Europe/Stockholm", 
#                                        timeformat="%Y-%m-%d %H:%M:%S", 
#                                        projection= "+init=epsg:32634",
#                                        datum="WGS84",
#                                        keep = c("treatment", "date", 
#                                                 "week", "individual_day"))

pike_lake_BT_tel_tpeqd <- as.telemetry(pike_movebank, 
                                       timezone = "Europe/Stockholm", 
                                       timeformat="%Y-%m-%d %H:%M:%S", 
                                       projection= NULL,
                                       datum="WGS84",
                                       keep = c("treatment", "date", 
                                                "week", "individual_day")
)


#check some dataset parameters

head(pike_lake_BT_tel_tpeqd$F59886)
#columns x and y are very different from 1. I think it has something to do with the projection
ctmm::projection(pike_lake_BT_tel_tpeqd$F59886)
#tpeqd projection
tz(pike_lake_BT_tel_tpeqd$F59886$timestamp)
#"Europe/Stockholm"

#--------------------------------------------------------------------------------#

names(pike_lake_BT_tel_tpeqd)

#Center the projection on the geometric median of the data
ctmm::projection(pike_lake_BT_tel_tpeqd) <- ctmm::median(pike_lake_BT_tel_tpeqd)

### INCORPORATING LOCATION ERROR
# fit error parameters to calibration data
#UERE_utm <- uere.fit(pike_lake_BT_tel_utm$FReference)
UERE_tpeqd <- uere.fit(pike_lake_BT_tel_tpeqd$FReference)
# do not run uere.fit on tracking data

#summary(UERE_utm)
summary(UERE_tpeqd)
#both are similar

# apply error model to data
#uere(pike_lake_BT_tel_utm) <- UERE_utm
uere(pike_lake_BT_tel_tpeqd) <- UERE_tpeqd
#new column now called VAR.xy
head(pike_lake_BT_tel_tpeqd$F59886)
names(pike_lake_BT_tel_tpeqd)

#remove reference list
pike_lake_BT_tel <- pike_lake_BT_tel_tpeqd[1:6]
names(pike_lake_BT_tel)

#remove outliers based on speed
out_pike <- outlie(pike_lake_BT_tel, plot = FALSE)
head(out_pike[[1]])
sum(sapply(out_pike, function(x) sum(x$speed > 0.823)))
#Function to get range in speeds (m/s) for each individual. 
#11661 are > than 0.823 m/s 
#Ucrit speeds taken from 
#KEY FACTORS EXPLAINING CRITICAL SWIMMING SPEED IN FRESHWATER FISH:  
#A REVIEW AND STATISTICAL ANALYSIS USING IBERIAN SPECIES), 

#Need to filter out unrealistic speeds
#Making a logical vector
which_lowSp <- lapply(out_pike, function(x) x$speed <= 0.823)
#Combining the lists and removing observations for which the logical vector was false
pike_lake_BT_tel <- Map(function(x,y) x[y,], pike_lake_BT_tel,which_lowSp)

#save telemetry object
#saveRDS(pike_lake_BT_tel , paste0(save_telem_path, "pike_lake_BT_tel.rds")) 

#load object
#pike_lake_BT_tel <- readRDS(paste0(save_telem_path, "pike_lake_BT_tel.rds"))

cl <- makeCluster(6)
doParallel::registerDoParallel(cl)
lake_BT_pike_select_fits <-  
  foreach(i = 1:length(pike_lake_BT_tel), .packages = 'ctmm') %dopar% {
    lake_BT_pike_guess <- ctmm.guess(pike_lake_BT_tel[[i]], CTMM=ctmm(error=TRUE), interactive = FALSE)
    model_fit <- ctmm.select(pike_lake_BT_tel[[i]], lake_BT_pike_guess, verbose = TRUE)
    saveRDS(model_fit, file = paste0(save_ctmm_path, "lake_BT_pike_fits/", names(pike_lake_BT_tel)[i], ".rds"))
    model_fit
  }

stopCluster(cl)

###NEED TO LOOK INTO INDIVIDUAL F59889 ###
#has 30,000 more positions than the next hightest individual.
#I might subsample this pike in order to get the ctmm model to actually finish
#first I will isolate the pike
pike_F59889 <- pike_movebank %>% 
  dplyr::filter(individual.local.identifier == 'F59889')
library(move2)
pike_F59889_mv <- mt_as_move2(pike_F59889, 
                          coords = c("location.long","location.lat"),
                          crs = "WGS84",
                          time_column = "timestamp",
                          track_id_column = "individual.local.identifier",
                          na.fail = F) # allows or not empty coordinates

#Arrange by individual and datetime
pike_F59889_mv <- pike_F59889_mv %>% 
  dplyr::arrange(individual.local.identifier, timestamp)

pike_F59889_sub <- pike_F59889_mv %>% mt_filter_per_interval(unit = "3 seconds",  criterion="first")

pike_F59889_sub <- to_move(pike_F59889_sub)

pike_F59889_tel <- as.telemetry(pike_F59889_sub, 
                                       timezone = "Europe/Stockholm", 
                                       timeformat="%Y-%m-%d %H:%M:%S", 
                                       projection= NULL,
                                       datum="WGS84",
                                       keep = c("treatment", "date", 
                                                "week", "individual_day")
)


ctmm::projection(pike_F59889_tel)
#tpeqd projection
tz(pike_F59889_tel$timestamp)
#"Europe/Stockholm"

#Center the projection on the geometric median of the data
ctmm::projection(pike_F59889_tel) <- ctmm::median(pike_F59889_tel)

#summary(UERE_utm)
summary(UERE_tpeqd)
#both are similar

# apply error model to data
#uere(pike_lake_BT_tel_utm) <- UERE_utm
uere(pike_F59889_tel) <- UERE_tpeqd
#new column now called VAR.xy
head(pike_F59889_tel)

#remove outliers based on speed
out_pike <- outlie(pike_F59889_tel, plot = FALSE)
head(out_pike[[1]])
sum(out_pike$speed > 0.823)

#filter out speeds
pike_F59889_tel <- pike_F59889_tel[out_pike$speed < 0.823, ]

#save telemetry object
#saveRDS(pike_lake_BT_tel , paste0(save_telem_path, "pike_lake_BT_tel.rds")) 

#load object
#pike_lake_BT_tel <- readRDS(paste0(save_telem_path, "pike_lake_BT_tel.rds"))

lake_BT_pike_guess <- ctmm.guess(pike_F59889_tel, CTMM=ctmm(error=TRUE), interactive = FALSE)
F59889 <- ctmm.select(pike_F59889_tel, lake_BT_pike_guess, verbose = TRUE)
saveRDS(F59889, file = paste0(save_ctmm_path, "lake_BT_pike_fits/", "F59889.rds"))
 
#---------------------------------------------------------------------------------#

#### Perch ####




