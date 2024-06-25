# Muddyfoot 
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

#-------------------------------------------------------------------------------#

#### Northern pike #####

#Load in the datasets
muddyfoot_sub <- readRDS(paste0(data_filter_path, 'muddyfoot_sub.rds'))

#isolate pike
pike_muddyfoot <- muddyfoot_sub %>% 
  dplyr::filter(Species == 'Northern Pike'| individual_ID == 'Reference')


#Eric - movebank method. Provide as.telemetry with columns names it works better with
#No need to coerce it into a move object first.
pike_movebank <- with(pike_muddyfoot, data.frame("timestamp" = timestamp, "location.long" = Long,
                                                 "location.lat" = Lat, "GPS.HDOP" = HPE,
                                                 "individual-local-identifier" = Fish_ID,
                                                 "treatment" = Treatment,
                                                 "date" = date,
                                                 "week" = week,
                                                 "individual_day" = individual_day))

# pike_muddyfoot_tel_utm <- as.telemetry(pike_movebank, 
#                                        timezone = "Europe/Stockholm", 
#                                        timeformat="%Y-%m-%d %H:%M:%S", 
#                                        projection= "+init=epsg:32634",
#                                        datum="WGS84",
#                                        keep = c("treatment", "date", 
#                                                 "week", "individual_day"))

pike_muddyfoot_tel_tpeqd <- as.telemetry(pike_movebank, 
                                         timezone = "Europe/Stockholm", 
                                         timeformat="%Y-%m-%d %H:%M:%S", 
                                         projection= NULL,
                                         datum="WGS84",
                                         keep = c("treatment", "date", 
                                                  "week", "individual_day")
)


#Check some of the parameters

head(pike_muddyfoot_tel_tpeqd$F59880)
#columns x and y are very different from 1. I think it has something to do with the projection
ctmm::projection(pike_muddyfoot_tel_tpeqd$F59880)
#tpeqd projection
tz(pike_muddyfoot_tel_tpeqd$F59880$timestamp)
#"Europe/Stockholm"

#--------------------------------------------------------------------------------#

names(pike_muddyfoot_tel_tpeqd)

#Center the projection on the geometric median of the data
ctmm::projection(pike_muddyfoot_tel_tpeqd) <- ctmm::median(pike_muddyfoot_tel_tpeqd)

### INCORPORATING LOCATION ERROR
# fit error parameters to calibration data
#UERE_utm <- uere.fit(pike_muddyfoot_tel_utm$FReference)
UERE_tpeqd <- uere.fit(pike_muddyfoot_tel_tpeqd$FReference)
# do not run uere.fit on tracking data

#summary(UERE_utm)
summary(UERE_tpeqd)
#both are similar

# apply error model to data
#uere(pike_muddyfoot_tel_utm) <- UERE_utm
uere(pike_muddyfoot_tel_tpeqd) <- UERE_tpeqd
#new column now called VAR.xy
head(pike_muddyfoot_tel_tpeqd$F59880)

#remove reference list
pike_muddyfoot_tel <- pike_muddyfoot_tel_tpeqd[1:6]

#remove outliers based on speed
out_pike <- outlie(pike_muddyfoot_tel, plot = FALSE)
head(out_pike[[1]])
sum(sapply(out_pike, function(x) sum(x$speed > 0.823)))
#Function to get range in speeds (m/s) for each individual. 
#891 are > than 0.823 m/s 
#Ucrit speeds taken from 
#KEY FACTORS EXPLAINING CRITICAL SWIMMING SPEED IN FRESHWATER FISH:  
#A REVIEW AND STATISTICAL ANALYSIS USING IBERIAN SPECIES), 

#Need to filter out unrealistic speeds
#Making a logical vector
which_lowSp <- lapply(out_pike, function(x) x$speed <= 0.823)
#Combining the lists and removing observations for which the logical vector was false
pike_muddyfoot_tel <- Map(function(x,y) x[y,], pike_muddyfoot_tel,which_lowSp)

#Run ctmm models for six pike in muddyfoot
cl <- makeCluster(6)
doParallel::registerDoParallel(cl)
muddyfoot_pike_select_fits <-  
  foreach(i = 1:length(pike_muddyfoot_tel), .packages = 'ctmm') %dopar% {
    lake_BT_pike_guess <- ctmm.guess(pike_muddyfoot_tel[[i]], CTMM=ctmm(error=TRUE), interactive = FALSE)
    model_fit <- ctmm.select(pike_muddyfoot_tel[[i]], lake_BT_pike_guess, verbose = TRUE)
    saveRDS(model_fit, file = paste0(save_ctmm_path, "muddyfoot_pike_fits/", names(pike_muddyfoot_tel)[i], ".rds"))
    model_fit
  }

stopCluster(cl)

#add ID to lists
names(muddyfoot_pike_select_fits ) <- names(pike_muddyfoot_tel)

#save fits
#saveRDS(muddyfoot_pike_select_fits , paste0(save_ctmm_path, "muddyfoot_pike_select_fits .rds")) 
muddyfoot_pike_select_fits <- readRDS(paste0(save_ctmm_path, "muddyfoot_pike_ctmm_fits.rds"))

#Check model outputs
for (i in 1:length(muddyfoot_pike_select_fits)) {
  element_name <- names(muddyfoot_pike_select_fits)[i]
  cat("Summary for", element_name, ":\n")
  print(summary(muddyfoot_pike_select_fits[[i]]))
  cat("\n")
}

