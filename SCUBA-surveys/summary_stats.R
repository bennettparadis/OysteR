library(dplyr)
library(grid)
library(gridExtra)
library(ggplot2)
library(tidyr)
library(data.table)
library(stringr)

setwd("S:/8. Oyster Sanctuaries/3. Monitoring and Data/1. Oyster Sanctuary (OS)/5. Analysis/2025 R")
df <- read.csv("RF_predictions_2025.csv")

#calculate mean densities for each material type on each sanctuary & output a csv file
matOSdens <- df%>%
  group_by(OS_Name, Material)%>%
  summarise(n = n(), 
            avg_mat_age = mean(Material_Age, na.rm = TRUE),
            avg_total_dens = mean(total, na.rm = TRUE),
            sd_total_dens = sd(total, na.rm = TRUE),
            avg_legal_dens = mean(legal, na.rm = TRUE),
            sd_legal_dens = sd(legal, na.rm = TRUE),
            avg_sublegal_dens = mean(sublegal, na.rm = TRUE),
            sd_sublegal_dens = sd(sublegal, na.rm = TRUE),
            avg_spat_dens = mean(spat, na.rm = TRUE),
            sd_spat_dens = sd(spat, na.rm = TRUE)
  )
write.csv(matOSdens, "matOS_avgs2025_2.csv")

#calculate mean densities for each sanctuary & output a csv file
OSdens <- df%>%
  group_by(OS_Name)%>%
  summarise(n = n(), 
            avg_total_dens = mean(total, na.rm = TRUE),
            sd_total_dens = sd(total, na.rm = TRUE),
            avg_legal_dens = mean(legal, na.rm = TRUE),
            sd_legal_dens = sd(legal, na.rm = TRUE),
            avg_sublegal_dens = mean(sublegal, na.rm = TRUE),
            sd_sublegal_dens = sd(sublegal, na.rm = TRUE),
            avg_spat_dens = mean(spat, na.rm = TRUE),
            sd_spat_dens = sd(spat, na.rm = TRUE)
  )
write.csv(OSdens, "OS_avgs2025.csv")

#calculate the size class density averages for the entire oyster sanctuary network
density_OSavg2025 <- df%>%
  summarise(n = n(), 
            avg_total_dens = mean(total, na.rm = TRUE),
            sd_total_dens = sd(total, na.rm = TRUE),
            avg_legal_dens = mean(legal, na.rm = TRUE),
            sd_legal_dens = sd(legal, na.rm = TRUE),
            avg_sublegal_dens = mean(sublegal, na.rm = TRUE),
            sd_sublegal_dens = sd(sublegal, na.rm = TRUE),
            avg_spat_dens = mean(spat, na.rm = TRUE),
            sd_spat_dens = sd(spat, na.rm = TRUE))

print(density_OSavg2025)



