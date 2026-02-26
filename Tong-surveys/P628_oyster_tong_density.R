################################################################################
# SCRIPT NAME     : Oyster Tong Survey (P628) Density calculations             #
# DESCRIPTION &   : For estimating oyster size class densities                 #
# INSTRUCTIONS    : Import csv file with individual oyster measurements. If    #
#                 : sub-sampling occurred, run resampling script first.        #
#                 :                                                            #
################################################################################
# PROGRAMMER      : Bennett Paradis                                            #
# DATE WRITTEN    : 01/16/2026                                                 #
# CURRENT CONTACT : Bennett Paradis                                            #
# CONTACT INFO    : bennett.paradis@deq.nc.gov                                 #
################################################################################
# INPUT FILES     : csv of individual oyster measurements                      #
# OUTOUT FILES    : csv of oyster density estimates by size class              #
################################################################################
# MODIFICATIONS   :                                                            #
#---------------- :                                                            #
# DATE            :                                                            #
# CHANGE #        :                                                            #
# PROGRAMMER      :                                                            #
# DESCRIPTION     :                                                            #
################################################################################

library(dplyr)
library(grid)
library(gridExtra)
library(ggplot2)
library(tidyr)
library(data.table)
library(stringr)

# update R & RStudio without IT help!
# library(installr)
# updateR()

#set working directory to import csv data
setwd("S:/14. DORAs/FMP Monitoring Protocols/P628 Tong Surveys")
df <- read.csv("P628_test_data.csv")

#get a list of unique sites that were sampled
#easy way to check number of sites
SID_list <- unique(df$SID)
SIDs <- as.data.frame(SID_list)
#write.csv(SIDs, "2025_SIDs.csv")

col_names <- names(df)

# Create a dataframe with all the environmental data and Sample ID (SID will be the key for a join)
environ_data <- df %>%
  select(SID,Station,Management.Area,Latitude, Longitude,Material,B.Do,B.Temp,B.Sal, S.Sal)

# removes duplicate rows, only unique SID remains; dataframe should have the same number of rows as dens_total dataframe
environ_data <- environ_data[!duplicated(environ_data$SID),]

#create oyster dataframes - one for total, and one for each size class (legal, sublegal, spat)
oyster_data <- df%>%
  select(SID, Year, LVL, Size.Class, Total.Oyster.Count)

#sampling effort - total area sampled, based on tong length and number of grabs
tong_l = 0.876
n_grabs = 3
effort = n_grabs * (tong_l**2)

density_data <- oyster_data%>%
  group_by(Year, SID) %>%
  mutate(
    total_density = Total.Oyster.Count / effort,
    legal_density = sum(Size.Class == 'Legal', na.rm = TRUE)/ effort,
    sublegal_density = sum(Size.Class == 'Sub-Legal', na.rm = TRUE)/ effort,
    spat_density = sum(Size.Class == 'Spat', na.rm = TRUE)/ effort
  )%>%
  select(SID, Year, total_density, legal_density, sublegal_density, spat_density)

density_data <- density_data[!duplicated(density_data$SID),]

df2 <- left_join(density_data, environ_data, by = "SID")

#create a csv with density calculations for each site that was sampled
write.csv(df2, "tong_densities_2025.csv")

#optional for later analysis when comparing several cultch sites
# create a summary function that allows you to specify the level to which you want to average across (Station, Year Deployed, Region)
#useful for cultch evaluations - comparing effects between years or regions for instance
summarize_avgs <- function(...) {
  df2%>%
    group_by(...)%>%
    summarise(n = n(), 
              #min, max, avg total density 
              max_total_dens = max(total_density, na.rm=TRUE),
              min_total_dens = min(total_density, na.rm=TRUE),
              avg_total_dens = mean(total_density, na.rm = TRUE),
              sd_total_dens = sd(total_density, na.rm = TRUE),
              
              #min, max, avg legal density 
              max_legal_dens = max(legal_density, na.rm=TRUE),
              min_legal_dens = min(legal_density, na.rm=TRUE),
              avg_legal_dens = mean(legal_density, na.rm = TRUE),
              sd_legal_dens = sd(legal_density, na.rm = TRUE),
              
              #min, max, avg sublegal density 
              max_sublegal_dens = max(sublegal_density, na.rm=TRUE),
              min_sublegal_dens = min(sublegal_density, na.rm=TRUE),
              avg_sublegal_dens = mean(sublegal_density, na.rm = TRUE),
              sd_sublegal_dens = sd(sublegal_density, na.rm = TRUE),
              
              #min, max, avg spat density 
              max_spat_dens = max(spat_density, na.rm=TRUE),
              min_spat_dens = min(spat_density, na.rm=TRUE),
              avg_spat_dens = mean(spat_density, na.rm = TRUE),
              sd_spat_dens = sd(spat_density, na.rm = TRUE),
              .groups = 'drop')
}  

#different function calls specifying the level at which sites are compared
station_summary <- summarize_avgs(Station)
deployment_year_summary <- summarize_avgs(Deployment.Year)
region_summary <- summarize_avgs(Region)
region_year_summary <- summarize_avgs(Region, Deployment.Year)

#write the summarized stats as a csv file
write.csv(station_summary, "station_avgs_2025.csv")
# write.csv(deployment_year_summary, "deployment_year_avgs_2025.csv")
# write.csv(region_summary, "region_avgs_2025.csv")
# write.csv(region_year_summary, 'region_year_avgs_2025.csv')