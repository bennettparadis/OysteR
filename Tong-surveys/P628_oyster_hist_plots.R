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
# The script expects the processed 2025 dataset CSV as input.

# --- Setup ------------------------------------------------------------------

# Set working directory and load CSV
setwd("S:/14. DORAs/FMP Monitoring Protocols/P628 Tong Surveys")
df <- read.csv("P628_test_data.csv")

# Ensure Station column is numeric
df$Station <- as.numeric(df$Station)

# Subset and arrange the dataset to only include required columns
datum <- df %>%
  select(Year, Month, Day, Station, SID, LVL) %>%
  arrange(Station)

# Total area sampled per tong (m²)
tong_area <- 0.876^2

# --- Standardize sampling effort -------------------------------------------

# Compute number of tong grabs per station
Num_grabs <- datum %>%
  group_by(Station) %>%
  summarise(n_grabs = n_distinct(SID), .groups = "drop")

# Create a loop index to safely iterate over stations
Num_grabs$vector <- seq_len(nrow(Num_grabs))

# Join grab counts back to the main dataset
datum <- left_join(datum, Num_grabs, by = "Station")
datum <- na.omit(datum)

# --- Histogram bin setup ---------------------------------------------------

br <- seq(0, 180, by = 5)  # Bin edges
ranges <- seq(5, 180, by = 5)  # Bin midpoints
plist <- list()  # Store plots

loop.vector <- seq_len(nrow(Num_grabs))  # Loop index for stations

# Text grobs (if needed for grid or multi-panel plots)
y.grob <- textGrob(expression("Frequency (Oysters per m"^2*")"), rot = 90,
                   gp = gpar(fontsize = 24, fontface = "plain"))
x.grob <- textGrob("LVL (mm)", gp = gpar(fontsize = 24, fontface = "plain"))
title <- textGrob("2025 Size Frequencies", gp = gpar(fontsize = 20, fontface = "plain"))

# --- Generate and save individual station plots ----------------------------

for (i in loop.vector) {
  
  # Subset data for current station
  tong <- datum %>% filter(vector == i)
  
  # Calculate total sampling effort for this station
  n_grabs <- unique(tong$n_grabs)
  effort <- n_grabs * tong_area
  
  # Compute histogram counts (number of oysters per bin standardized by effort)
  freq <- hist(tong$LVL, breaks = br, include.lowest = TRUE, plot = FALSE)
  Count <- data.frame(
    range = ranges,
    count = freq$counts / effort
  )
  
  # Set base y-axis limit and adjust for outliers
  y_limit <- 50  # Typical maximum for most sites
  max_bin <- max(Count$count, na.rm = TRUE)
  
  if (max_bin > y_limit) {
    y_limit_plot <- ceiling(max_bin / 50) * 50  # Expand axis for extreme sites
    clipped <- TRUE
  } else {
    y_limit_plot <- y_limit
    clipped <- FALSE
  }
  
  # Generate histogram plot
  plist[[i]] <- ggplot(Count, aes(x = range, y = count)) +
    geom_bar(stat = "identity", color = "black", fill = "grey") +
    geom_vline(xintercept = 75, linetype = "dashed", size = 0.5) +  # Reference lines
    geom_vline(xintercept = 25, linetype = "dotdash", size = 0.5) +
    coord_cartesian(ylim = c(0, y_limit_plot)) +  # Prevent data clipping
    theme_classic() +
    geom_text(
      data = Num_grabs %>% filter(vector == i),
      aes(
        x = 100,
        y = 0.95 * y_limit_plot,
        label = paste0(tong$Month[1], "-", tong$Day[1], "-", tong$Year[1],
                       "\n", "n = ", n_grabs,
                       if (clipped) "\nscaled axis" else "")
      ),
      hjust = 0
    ) +
    labs(x = "LVL (mm)", y = expression("Frequency (Oysters/m"^2*")")) +
    ggtitle(tong$Station[1]) +
    theme(plot.title = element_text(size = 20, hjust = 0.5))
  
  # Optional: print plot to console
  plist[i]
  
  # Save plot to TIFF
  ggsave(file = paste0("2025_histogram", tong$Station[1], ".tiff"),
         width = 5, height = 4, dpi = 300, units = "in")
}
