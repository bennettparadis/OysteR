################################################################################
# SCRIPT NAME     : Oyster SCUBA Survey (P612) Resampling with replacement     #
# DESCRIPTION &   : If subsampling occurred during data processing of oysters  #
#                 : this script will resample the measured oysters to generate #
#                 : a simulated sample that matches the total number of        #
#                 : oysters counted within the sample.                         #  
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
# OUTOUT FILES    : csv of individual oyster measurements augmented with the   #
#                 : 'complete'/simulated measurements for sites that get       #
#                 : resampled.                                                 #
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

#set working directory to import csv data
setwd("S:/8. Oyster Sanctuaries/3. Monitoring and Data/1. Oyster Sanctuary (OS)/5. Analysis/2025 R")
df <- read.csv("2025 Data.csv")

#clean spaces/symbols from column headers
names(df) <- make.names(names(df))

#set working directory for output files
#setwd("S:/7. Cultch Planting/6. Monitoring and Data/p610 monitoring/5. Analysis/2024")


#get list of unique sites, validate number of sites matches sampling effort
SID_list <- unique(df$SID)
SIDs <- as.data.frame(SID_list)
#write.csv(SIDs, "2025_SIDs.csv")

#Split datafrane into what was subsampled and samples that were "censused" or where only observation data was collected
sub_data <- subset(df, df$Sample.Method == 'Subsample')
part_data <- subset(df, df$Sample.Method != 'Subsample')

#Create a empty dataframe that matches the structure of the original imported dataframe
#this will store measurements for the sites that will be resampled with replacement
resampled_data <- df[0,]


#loop through each site using unique SID
for (sid in unique(sub_data$SID)) {
  #subset the data for each SID iteration
  subsample <- subset(sub_data, SID == sid)
  
  # Confirm how many oysters were measured
  measured_n <- nrow(subsample)  
  
  # get total count from sample
  total_count <- max(subsample$Total.Oyster.Count, na.rm = TRUE)
  
  # Calculate how many resampled rows are needed
  n_resample <- total_count - measured_n
  
  #check to ensure resampling is needed for site
  if (n_resample < 0) {
    warning(paste("Total.count is smaller than actual measured count at SID:", sid))
    n_resample <- 0
  }
  
  if (n_resample > 0) {
    # Sample with replacement
    resampled_rows <- subsample[
      sample(seq_len(measured_n), n_resample, replace = TRUE), 
      ]
  }
  else {
    resampled_rows <- subsample[0,]
  }
  
  # Combine original measured + resampled rows for this site
  site_data <- rbind(subsample, resampled_rows)
  
  # Bind to overall resampled_data
  resampled_data <- rbind(resampled_data, site_data)
}


#check to see if number of resampled rows equals the sum of total oysters counted among subsampled sites
check_subsample <- df %>%
  filter(Sample.Method == "Subsample") %>%
  group_by(SID) %>%
  summarise(site_total = max(Total.Oyster.Count, na.rm = TRUE)) %>%
  summarise(total = sum(site_total))

counted_n   <- check_subsample$total
generated_n <- nrow(resampled_data)

if (counted_n != generated_n) {
  warning(
    paste(
      "Mismatch for subsampled sites:",
      counted_n, "expected vs", generated_n, "generated"
    )
  )
} else {
  message("Subsample resampling matches expected totals")
}


#Organize resampled data frame before joining with part_data 
resampled_data <- resampled_data%>%
  select(-Oyster)%>%
  group_by(SID) %>% 
  mutate(Oyster = row_number()) %>% #recreates a column that assigns a sample number for each oyster at that site
  ungroup() %>%
  relocate(Oyster, .before = LVL) %>% #places the Sample column before SH_mm -- like in original dataset
  arrange(SID, Oyster) #properly sorts samples

#combine the resampled data with the census data to get a full frame ready to generate length frequency plots
final_df <- rbind(resampled_data, part_data)


#final check to see if number of rows equals the sum of total oysters counted among all sites
true_n <- df %>%
  filter(Collection.Method == "Extraction",
         !is.na(Total.Oyster.Count)) %>%   # <-- critical filter
  group_by(SID) %>%
  summarise(site_total = max(Total.Oyster.Count)) %>%
  summarise(total = sum(site_total)) %>%
  pull(total)

final_n <- final_df %>%
  filter(Collection.Method == "Extraction",
         !is.na(Total.Oyster.Count)) %>%  # match the same sites
  nrow()

if (true_n != final_n) {
  warning(
    paste(
      "Mismatch for extraction sites with totals:",
      true_n, "expected vs", final_n, "in final_df"
    )
  )
} else {
  message("Final dataset matches extraction totals for sites with counts")
  write.csv(final_df, "2025_hist_data.csv")
}

site_compare <- df %>%
  filter(Collection.Method == "Extraction",
         !is.na(Total.Oyster.Count)) %>%
  group_by(SID) %>%
  summarise(expected = max(Total.Oyster.Count)) %>%
  mutate(generated = sapply(SID, function(s) sum(final_df$SID == s)))

problem_sites <- site_compare %>%
  filter(expected != generated)

print(site_compare)
print(problem_sites)