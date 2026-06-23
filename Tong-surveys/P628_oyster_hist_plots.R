################################################################################
# SCRIPT NAME     : Oyster population structure visualization w/ histograms    #
#                 : To be use for data collected by both the Oyster SCUBA      #
#                 : Survey (P612) & Tong Survey (P628) efforts. This script    #
# DESCRIPTION &   : generates histograms from a series of left valve           #
#                 : measurements from various sites. Individual plots can be   #
#                 : created for a specific reef/sanctuary or a facet plot can  #
#                 : be put together by toggling on/off lines in the for loop.  #
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
# OUTOUT FILES    : inidividual tiff files of histograms for a sanctuary site  #
#                 : or a facet plot for the entire sanctuary network that was  #
#                 : monitored in a single year                                 #
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
library(scales)
library(devtools)
library(car)
library(carData)

#### Length Frequency Histograms ####
# This script generates length-frequency histograms for oyster data after resampling.
# Resampling is applied at excavated sites where subsampling was used.
# The script expects the processed 'R' dataset CSV as input.


#!THESE ARE THE ONLY THINGS TO CHANGE EACH YEAR!
#evaluation <- 'cultch'
evaluation <- 'trigger'
trigger_timing <- 'preseason' #or midseason
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

# assign each sampled site the correct number of tong grabs (changes by evaluation effort)
if (evaluation == 'cultch') {
  Num_grabs <- df %>%
    group_by(Station) %>%
    summarise(n_grabs = n_distinct(SID), .groups = "drop")
  
  
  # Create a loop index to safely iterate over stations
  Num_grabs$vector <- seq_len(nrow(Num_grabs))
  
  # Join grab counts back to the main dataset
  datum <- left_join(df, Num_grabs, by = "Station")
  
} else {
  
  #control flow for trigger/in-season management tong survey
  
  Num_grabs <- df %>%
    group_by(Station) %>%
    summarise(n_grabs = n_distinct(SID) * 3, .groups = "drop")
  
  # Create a loop index to safely iterate over stations
  Num_grabs$vector <- seq_len(nrow(Num_grabs))
  
  # Join grab counts back to the main dataset
  datum <- left_join(df, Num_grabs, by = "Station")

}


# Prepare sampling effort for normalization/correction in density estimates

# Total area sampled per tong (m²)
tong_length <- 0.876
tong_area <- tong_length^2

#correction factor (per Harris et al. 2026)
#https://academic.oup.com/najfm/article-abstract/46/3/598/8670038?redirectedFrom=fulltext
#compared scuba quadrat sampling to hydraulic patent tongs in Galveston TX oyster reefs
#found that efficiency ran between 43-73% 
#this is higher than the efficiency determined by Marylands gear comparison study
#the correction factor below is based off the assumption of a 73% efficiency per tong grab (reciprocal)
cf = 1.37

# --- Histogram bin setup ---------------------------------------------------

br <- seq(0, 180, by = 5)  # Bin edges
ranges <- seq(5, 180, by = 5)  # Bin midpoints
plist <- list()  # Store plots

loop.vector <- unique(datum$Station)  # Loop index for stations

# Text grobs (if needed for grid or multi-panel plots)
y.grob <- textGrob(expression("Frequency (Oysters per m"^2*")"), rot = 90,
                   gp = gpar(fontsize = 24, fontface = "plain"))
x.grob <- textGrob("LVL (mm)", gp = gpar(fontsize = 24, fontface = "plain"))
title <- textGrob("Oyster Size Frequencies", gp = gpar(fontsize = 20, fontface = "plain"))

# --- Generate and save individual station plots ----------------------------
for (idx in seq_along(loop.vector)) {
  i <- loop.vector[idx]
  tong <- datum %>% filter(Station == i)
  
  mat_age <- unique(tong$Material.Age)[1]
  n_grabs <- unique(tong$n_grabs)[1]
  effort <- n_grabs * tong_area * cf
  
  freq <- hist(tong$LVL, breaks = br, include.lowest = TRUE, plot = FALSE)
  Count <- data.frame(
    range = ranges,
    count = freq$counts / effort
  )
  
  y_limit <- 50
  max_bin <- max(Count$count, na.rm = TRUE)
  y_limit_plot <- if (max_bin > y_limit) ceiling(max_bin / 50) * 50 else y_limit
  
  # Build label here, before ggplot
  plot_label <- paste0(
    tong$Month[1], "-", tong$Day[1], "-", tong$Year[1],
    "\nn = ", n_grabs,
    "\nReef Age = ", mat_age, " years"
  )
  
  plist[[idx]] <- ggplot(Count, aes(x = range, y = count)) +
    geom_bar(stat = "identity", color = "black", fill = "grey") +
    geom_vline(xintercept = 75, linetype = "dashed", size = 0.5) +
    geom_vline(xintercept = 25, linetype = "dotdash", size = 0.5) +
    coord_cartesian(ylim = c(0, y_limit_plot)) +
    theme_classic() +
    geom_text(
      x = 100,
      y = 0.75 * y_limit_plot,
      label = plot_label,
      hjust = 0
    ) +
    labs(x = "LVL (mm)", y = expression("Frequency (Oysters/m"^2*")")) +
    ggtitle(tong$Station[1]) +
    theme(plot.title = element_text(size = 20, hjust = 0.5))
  
  ggsave(file = paste0(tong$Station[1], "_", survey_year, "_hist.tiff"),
         width = 5, height = 4, dpi = 300, units = "in")
}


stations <- df%>%
  select(Station)%>%
  distinct(Station)
