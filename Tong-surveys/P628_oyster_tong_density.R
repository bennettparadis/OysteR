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

#!THESE ARE THE ONLY THINGS TO CHANGE EACH YEAR!
#evaluation <- 'cultch'
evaluation <- 'trigger'
trigger_timing <- 'midseason' #or midseason
survey_year <- 2025



#if/else controls directories for data upload and outputs based on evaluation & year specified
if (evaluation == 'cultch') {
  
  #cultch set up
  #set folder directories with the year for upload and output
  analysis_dir <- paste0("S:/7. Cultch Planting/6. Monitoring and Data/p628 monitoring/5. Analysis/", survey_year)
  
  df_name <- paste0("P628_cultch_", survey_year,"data_R.csv")
  
} else {
  
  #trigger set up
  #set folder directories with the year for upload and output
  analysis_dir <- paste0("S:/16. Trigger Sampling/Data/", survey_year,"/",trigger_timing,"/Analysis")
  
  df_name <- paste0("P628_trigger_", trigger_timing, survey_year, "data_R.csv")
  
}


#upload data
setwd(analysis_dir)
df <- read.csv(df_name)

# Station code formatting w/ leading zeroes 
df <- df %>%
  mutate(
    Station = as.character(Station),
    Station = case_when(
      Station != "NATOR" ~ sprintf("%05d", as.numeric(Station)),
      .default = Station
    )
  )


#get a list of unique sites that were sampled
#easy way to check number of sites
SID_list <- unique(df$SID)
SIDs <- as.data.frame(SID_list)
#write.csv(SIDs, "2025_SIDs.csv")

col_names <- names(df)

# Create a dataframe with all the environmental data and Sample ID (SID will be the key for a join)
environ_data <- df %>%
  select(SID,
         #Management.Area,
         Management.Area, Latitude, Longitude,Deployment.Year,Material.Age,B.Do,B.Temp,B.Sal, S.Sal)

# removes duplicate rows, only unique SID remains; dataframe should have the same number of rows as dens_total dataframe
environ_data <- environ_data[!duplicated(environ_data$SID),]

#create oyster dataframes - one for total, and one for each size class (legal, sublegal, spat)
oyster_data <- df%>%
  select(SID, Year, Station, Evaluation, Sample, LVL, Size.Class, Total.Oyster.Count)

# Prepare sampling effort for normalization/correction in density estimates

# Total area sampled per tong (m²)
tong_length <- 0.876
if (evaluation == 'trigger') {
  n_grabs <- 3 
    } else {
  n_grabs <- 1
  }

tong_area <- n_grabs * (tong_length^2)

#correction factor (per Harris et al. 2026)
#https://academic.oup.com/najfm/article-abstract/46/3/598/8670038?redirectedFrom=fulltext
#compared scuba quadrat sampling to hydraulic patenet tongs in Galveston TX oyster reefs
#found that efficiency ran between 43-73% 
#this is higher than the efficiency determined by Marylands gear comparison study
#the correction factor below is based off the assumption of a 73% efficiency per tong grab (reciprocal)
cf = 1.37


density_data <- oyster_data%>%
  group_by(Year, Station, Sample, SID) %>%
  mutate(
    total_density = cf*(Total.Oyster.Count / tong_area),
    legal_density = cf*(sum(Size.Class == 'Legal', na.rm = TRUE)/tong_area),
    sublegal_density = cf*(sum(Size.Class == 'Sub-Legal', na.rm = TRUE)/tong_area),
    spat_density = cf*(sum(Size.Class == 'Spat', na.rm = TRUE)/tong_area)
  )%>%
  select(SID, Year, Station, Sample, total_density, legal_density, sublegal_density, spat_density)


density_data <- density_data[!duplicated(density_data$SID),]

df2 <- left_join(density_data, environ_data, by = "SID")

sitedensity_output_name <- if (evaluation == 'trigger') {
  paste0("P628_trigger", trigger_timing, survey_year, "densities.csv")
} else {
  paste0("P628_cultch", survey_year, "densities.csv")
}

#create a csv with density calculations for each site that was sampled
write.csv(df2, sitedensity_output_name)

#optional for later analysis when comparing several cultch sites
# create a summary function that allows you to specify the level to which you want to average across (Station, Year Deployed, Region)
#useful for cultch evaluations - comparing effects between years or regions for instance

summarize_avgs <- function(...) {
  df2%>%
    group_by(...)%>%
    summarise(
        n = n(),
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
material_age_summary <- summarize_avgs(Material.Age)
region_summary <- summarize_avgs(Management.Area)
region_year_summary <- summarize_avgs(Management.Area, Material.Age)

#write the summarized stats as a csv file
write.csv(station_summary, paste0("P628_", evaluation,"station_avgs_", survey_year ,".csv"))
write.csv(material_age_summary, paste0("P628_", evaluation,"deployment_year_avgs_", survey_year ,".csv"))
write.csv(region_summary, paste0("P628_", evaluation,"region_avgs_", survey_year ,".csv"))
write.csv(region_year_summary , paste0("P628_", evaluation, "region_year_avgs_", survey_year , ".csv"))
