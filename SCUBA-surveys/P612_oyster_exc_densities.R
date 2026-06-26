################################################################################
# SCRIPT NAME     : Oyster SCUBA Survey (P612) Density estimates (extractions) #
# DESCRIPTION &   : Calculate the size class densities for OS SCUBA surveys on #
#                 : extracted materials. Observed densities can be estimated   #
#                 : in another script/workspace. If samples have been sub-     #
#                 : sampled, then the resampling script should be run first.   # 
#                 : A separate but similar script may be used for DORA         #
#                 : density calculations.                                      #
# INSTRUCTIONS    : Import csv file with individual oyster measurements from   #
#                 : Oyster Sanctuary SCUBA monitoring efforts                  #
#                 :                                                            #
################################################################################
# PROGRAMMER      : Bennett Paradis                                            #
# DATE WRITTEN    : 01/27/2026                                                 #
# CURRENT CONTACT : Bennett Paradis                                            #
# CONTACT INFO    : bennett.paradis@deq.nc.gov                                 #
################################################################################
# INPUT FILES     : csv of individual oyster measurements from annual survey   #
# OUTOUT FILES    : csv of size class density estimates for each sampled dive  #
#                 : site, each sanctuary by material type, and sanctuary wide  #
#                 :                                                            #
################################################################################
# MODIFICATIONS   :                                                            #
#---------------- :                                                            #
# DATE            :                                                            #
# CHANGE #        :                                                            #
# PROGRAMMER      :                                                            #
# DESCRIPTION     :                                                            #
################################################################################

library(plyr)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(tidyr)
library(data.table)

#set evaluation and year for P612 workflow 
#!THESE ARE THE ONLY THINGS TO CHANGE EACH YEAR!
#evaluation <- 'OS'
evaluation <- 'DORA'
survey_year <- 2025


#if/else controls directories for data upload and outputs based on evaluation & year specified
if (evaluation == 'OS') {
  
  #OS - Oyster Sanctuary - set up
  #set folder directories with the year for upload
  
  data_dir <- paste0("S:/8. Oyster Sanctuaries/3. Monitoring and Data/1. Oyster Sanctuary (OS)/5. Analysis/", survey_year, " R")
  df_name <- paste0(survey_year," R Data.csv")
  
} else {
  
  #DORA - set up
  #set folder directories with the year for upload
  data_dir <- paste0("S:/14. DORAs/SCUBA monitoring/", survey_year, "/Analysis")
  df_name <- paste0("DORA ", survey_year, " R Data.csv")
  
}


#upload data
setwd(data_dir)
df <- read.csv(df_name)

#clean spaces/symbols from column headers
names(df) <- make.names(names(df))

#format dataframe
OS_import<- dplyr::rename(df, Collection_Method=Collection.Method, Sample_Method=Sample.Method, Material_Age=Material.Age,Oyster_Cover=Oyster.Cover,Mussel_Cover=Mussel.Cover,Boring_Sponge=Boring.Sponge, Sample_Depth=Sample.Depth)

OS_import$Material <- factor(OS_import$Material)
OS_import$LVL<-as.numeric(OS_import$LVL)

# OS_import<-OS_import %>% #cut down dataset to only include what is needed
#   dplyr::select(Year,OS_ID,OS_Name,Material,Collection_Method,Sample_Method,Site_ID,Latitude,Longitude,SID,Deployment.Year, Deployment.Month,Material_Age,LVL,Oyster_Cover,Mussel_Cover,Sedimentation,Boring_Sponge,Sample_Depth,OS.Depth, Relief, S.Do, B.Do, S.Sal, B.Sal, S.Temp, B.Temp)
# 
# OS_import2 <- OS_import%>%
#   dplyr::select(-UID, -Quadrate.Size,)

# ISOLATE ENVIRONMENTAL DATA
# Create a dataframe with all the environmental data and Sample ID (SID will be the key for a join)
environ_data <- OS_import %>%
  select(SID,Collection_Method,Sample_Method, Latitude, Longitude, Deployment.Year, Deployment.Month, Material_Age,Oyster_Cover,Mussel_Cover,Sedimentation,Boring_Sponge,Sample_Depth,OS.Depth, Relief, S.Do, B.Do, S.Sal, B.Sal, S.Temp, B.Temp)

# removes duplicate rows, only unique SID remains; dataframe should have the same number of rows as dens_total dataframe
environ_data <- environ_data[!duplicated(environ_data$SID),]


# SPLIT OYSTER DATA
# Separate sample data by method used - extraction or observation
OS_extract<-subset(OS_import, OS_import$Collection_Method == "Extraction") #isolate extraction data rows
OS_extract$LVL <- as.numeric((OS_extract$LVL))

OS_obs <- subset(OS_import, OS_import$Collection_Method == "Observation") #isolate observation data rows

######################################################################
########       ESTIMATING DENSITIES OF EXCAVATED SAMPLES      ########    
######################################################################

# EXTRACTED DENSITIES - work with extracted dataframe to calculate densities
# Break up the oysters into subclasses; creates DF object for all oyster measurements for legal, sublegal, and spat size classes
oysters_legal <- filter(OS_extract, OS_extract$LVL >75) #exclusive of 75
oysters_sublegal <- filter(OS_extract, between(LVL, 26, 75)) #inclusive of 26 and 75
oysters_spat <- filter(OS_extract, LVL <=25) #inclusive of 25

# Function to calculate oyster density 
oyster_density <- function(x) {
  (x)%>%
    group_by(Year, OS_ID, OS_Name, Site_ID,SID, Material)%>%
    dplyr::summarise(oyst_density =4*n ())
}  

#Run the various size class data(all, legal, sublegal, spat) through density function, then join to the environmental data.  
#Each of these dataframes now treats each sampled site as an individual point, with the associated environmental data. 
#These can now be used in models to explore relationships between variables that might influence oyster density.

#total density across all size classes, join environmental data to each unique sample
dens_total <- oyster_density(OS_extract)
dens_total <- left_join(dens_total,environ_data,by="SID")
#write.csv(dens_total,"S:/8. Oyster Sanctuaries/3. Monitoring and Data/1. Oyster Sanctuary (OS)/5. Analysis/2025 R/excavated densities/dens_total.csv", row.names=FALSE)

#legal oyster density, merge to dens_total master dataframe
dens_legal <- oyster_density(oysters_legal)
dens_total <- merge(dens_total,dens_legal[, c("SID", "oyst_density")], by="SID", all.x=TRUE)
names(dens_total)[names(dens_total) == "oyst_density.y"] <- "legal_density"
dens_total$legal_density[is.na(dens_total$legal_density)]<-0

#sublegal oyster density, join to master dataframe
dens_sublegal <- oyster_density(oysters_sublegal)
dens_total <- merge(dens_total,dens_sublegal[, c("SID", "oyst_density")], by="SID", all.x=TRUE)
names(dens_total)[names(dens_total) == "oyst_density"] <- "sublegal_density"
dens_total$sublegal_density[is.na(dens_total$sublegal_density)]<-0

#spat oyster density, join environmental data to each unique sample
dens_spat <- oyster_density(oysters_spat)
dens_total <- merge(dens_total,dens_spat[, c("SID", "oyst_density")], by="SID", all.x=TRUE)
names(dens_total)[names(dens_total) == "oyst_density"] <- "spat_density"
names(dens_total)[names(dens_total) == "oyst_density.x"] <- "total_density"
dens_total$spat_density[is.na(dens_total$spat_density)]<-0

extract_density <- dens_total%>%
  mutate(non_spat = legal_density + sublegal_density)

#insert NA placeholders for density estimates where materials were observed and not excavated
OS_obs_clean <- OS_obs %>%
  distinct(SID, .keep_all = TRUE) %>%
  mutate(
    total_density    = NA_real_,
    legal_density    = NA_real_,
    sublegal_density = NA_real_,
    spat_density     = NA_real_,
    non_spat         = NA_real_
  )

#remove unecessary rows for a density dataframe
OS_obs_clean <- OS_obs_clean %>% select(-Quadrat.Size, -Month, -Algae.Cover, -Day, -Oyster, -Size_Class, -Total.Oyster.Count, -Spat.Y.N, -Sub.Legal.Y.N, -Legal.Y.N,-X,-UID, -LVL)

#merge density dataframes together
all_density<- rbind(OS_obs_clean, extract_density)

#clean up variables in the workspace/global environment
rm(dens_legal,dens_spat,dens_sublegal,dens_total,oysters_legal,oysters_spat,oysters_sublegal)

#create a csv file with site specific density estimates (each quadrat/dive has an associated series of size class densities)
write.csv(all_density, "2025_OS_densities")

#calculate mean densities for each material type on each sanctuary & output a csv file
matOSdens <- all_density%>%
  group_by(OS_ID, Material)%>%
  summarise(n = n(), 
            avg_total_dens = mean(total_density, na.rm = TRUE),
            sd_total_dens = sd(total_density, na.rm = TRUE),
            avg_legal_dens = mean(legal_density, na.rm = TRUE),
            sd_legal_dens = sd(legal_density, na.rm = TRUE),
            avg_sublegal_dens = mean(sublegal_density, na.rm = TRUE),
            sd_sublegal_dens = sd(sublegal_density, na.rm = TRUE),
            avg_spat_dens = mean(spat_density, na.rm = TRUE),
            sd_spat_dens = sd(spat_density, na.rm = TRUE)
  )
write.csv(matOSdens, "matOS_avgs2025.csv")

#calculate mean densities for each sanctuary & output a csv file
OSdens <- all_density%>%
  group_by(OS_ID)%>%
  summarise(n = n(), 
            avg_total_dens = mean(total_density, na.rm = TRUE),
            sd_total_dens = sd(total_density, na.rm = TRUE),
            avg_legal_dens = mean(legal_density, na.rm = TRUE),
            sd_legal_dens = sd(legal_density, na.rm = TRUE),
            avg_sublegal_dens = mean(sublegal_density, na.rm = TRUE),
            sd_sublegal_dens = sd(sublegal_density, na.rm = TRUE),
            avg_spat_dens = mean(spat_density, na.rm = TRUE),
            sd_spat_dens = sd(spat_density, na.rm = TRUE)
  )
write.csv(OSdens, "OS_avgs2025.csv")

#calculate the size class density averages for the entire oyster sanctuary network
density_OSavg2025 <- all_density%>%
  summarise(n = n(), 
            avg_total_dens = mean(total_density, na.rm = TRUE),
            sd_total_dens = sd(total_density, na.rm = TRUE),
            avg_legal_dens = mean(legal_density, na.rm = TRUE),
            sd_legal_dens = sd(legal_density, na.rm = TRUE),
            avg_sublegal_dens = mean(sublegal_density, na.rm = TRUE),
            sd_sublegal_dens = sd(sublegal_density, na.rm = TRUE),
            avg_spat_dens = mean(spat_density, na.rm = TRUE),
            sd_spat_dens = sd(spat_density, na.rm = TRUE))

print(density_OSavg2025)
