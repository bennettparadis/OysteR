################################################################################
# SCRIPT NAME     : Oyster Tong Survey (P612) Data Validation                  # 
# DESCRIPTION &   : Script to check data structure and format following data   #
#                 : entry of oyster processing resulting from tong survey      #
#                 : efforts including cultch sites or trigger surveys.         #
#                 :                                                            #  
# INSTRUCTIONS    : Import csv file with individual oyster measurements.       #
#                 :                                                            #
#                 :                                                            #
################################################################################
# PROGRAMMER      : Bennett Paradis                                            #
# DATE WRITTEN    : 01/16/2026                                                 #
# CURRENT CONTACT : Bennett Paradis                                            #
# CONTACT INFO    : bennett.paradis@deq.nc.gov                                 #
################################################################################
# INPUT FILES     : csv of individual oyster measurements                      #
# OUTOUT FILES    : A table of Site Identification Numbers (SIDs) for          #
#                 : error checking                                             #
################################################################################
# MODIFICATIONS   :                                                            #
#---------------- :                                                            #
# DATE            :                                                            #
# CHANGE #        :                                                            #
# PROGRAMMER      :                                                            #
# DESCRIPTION     :                                                            #
################################################################################

library(dplyr)
library(ggplot2)
library(tidyr)
library(data.table)
library(stringr)

# update R & RStudio without IT help!
# library(installr)
# updateR()


#set evaluation and year for P612 workflow 
#!THESE ARE THE ONLY THINGS TO CHANGE EACH YEAR!
#evaluation <- 'OS'
evaluation <- 'DORA'
survey_year <- 2025


#if/else controls directories for data upload and outputs based on evaluation & year specified
if (evaluation == 'OS') {
  
  #OS - Oyster Sanctuary - set up
  #set folder directories with the year for upload
  data_dir <- paste0("S:/8. Oyster Sanctuaries/3. Monitoring and Data/1. Oyster Sanctuary (OS)/4. Data/", survey_year, " Data")
  df_name <- paste0(survey_year," Data.csv")
  
} else {
  
  #DORA - set up
  #set folder directories with the year for upload
  data_dir <- paste0("S:/14. DORAs/SCUBA monitoring/", survey_year, "/Data")
  df_name <- paste0("DORA_monitoring_", survey_year, ".csv")
  
}


#upload data
setwd(data_dir)
df <- read.csv(df_name)

#clean spaces/symbols from column headers
names(df) <- make.names(names(df))


#focus on extraction sites, more data entry errors 
df <- df%>%
  filter(Collection.Method == "Extraction")

#clean spaces/symbols from column headers
names(df) <- make.names(names(df))


#get list of unique sites, validate number of sites matches sampling effort
SID_list <- unique(df$SID)
SIDs <- as.data.frame(SID_list)
#write.csv(SIDs, "test_SIDs_2025.csv")


#check sample site data consistency 
#list of variables that should be consistent among rows with the same SID
SID_vars <- c("Collection.Method", "Sample.Method", "Latitude", "Longitude", "Total.Oyster.Count", "Sample.Depth", "B.Do", "B.Sal", "B.Temp")

#check for site-specific variables and consistency by grouped locations
sid_consistency <- df %>%
  group_by(SID) %>%
  summarise(
    across(all_of(SID_vars), ~ n_distinct(.), .names = "n_unique_{col}")
  ) %>%
  pivot_longer(
    cols = starts_with("n_unique_"),
    names_to = "variable",
    names_prefix = "n_unique_",
    values_to = "n_unique"
  ) %>%
  filter(n_unique > 1) %>%
  mutate(issue = paste0("Inconsistent: ", variable)) %>%
  select(SID, issue)

#check for any extremes in LVL
lvl_issues <- df %>%
  group_by(SID) %>%
  summarise(max_LVL = max(LVL, na.rm = TRUE)) %>%
  filter(max_LVL > 200) %>%
  mutate(issue = paste0("LVL exceeds 200: ", max_LVL)) %>%
  select(SID, issue)

#Total.Oyster.Count and Sample.Method validation
sample_total_issues <- df %>%
  group_by(SID) %>%
  summarise(
    max_oyster = max(Oyster, na.rm = TRUE),
    total_count = unique(Total.Oyster.Count),
    sample_method = unique(Sample.Method)
  ) %>%
  mutate(issue = case_when(
    # Total count too low (max oyster > total) → Total count error
    max_oyster > total_count ~ paste0("Total.Oyster.Count inconsistent (max oyster = ", max_oyster, ")"),
    
    #if max oyster and total count equal, sample method should be census
    max_oyster == total_count & sample_method != "Census" ~ "Sample.Method should be 'Census'",
    
    #checks for instances where SID was incorrectly labeled as a census 
    max_oyster < total_count & max_oyster <= 400 & sample_method != "Subsample" ~ "Sample.Method should be 'Subsample'",
    
    # Otherwise no issue
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(issue)) %>%
  select(SID, issue)

#Combine all validation issues
master_validation <- bind_rows(
  sid_consistency,
  lvl_issues,
  sample_total_issues
) %>%
  arrange(SID)

master_validation
