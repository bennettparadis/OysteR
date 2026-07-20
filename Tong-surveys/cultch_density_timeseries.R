library(dplyr)
library(grid)
library(gridExtra)
library(ggplot2)
library(tidyr)
library(data.table)
library(stringr)

# #update R without IT help!
#library(installr)
#updateR()


#IMPORT SPATFALL DENSITY DATASETS
setwd("S:/7. Cultch Planting/6. Monitoring and Data/p628 monitoring/5. Analysis/2022")
df1 <- read.csv("P628_cultch2022densities.csv")
setwd("S:/7. Cultch Planting/6. Monitoring and Data/p628 monitoring/5. Analysis/2023")
df2 <- read.csv("P628_cultch2023densities.csv")
setwd("S:/7. Cultch Planting/6. Monitoring and Data/p628 monitoring/5. Analysis/2024")
df3 <- read.csv("P628_cultch2024densities.csv")
setwd("S:/7. Cultch Planting/6. Monitoring and Data/p628 monitoring/5. Analysis/2025")
df4 <- read.csv("P628_cultch2025densities.csv")

#combine into one spatfall df
spf_df <- rbind(df1,df2,df3,df4)
rm(df1,df2,df3,df4)

#Get statistics for stations
avgs <- spf_df %>%
  group_by(Year, Station) %>%
  summarise(
    n = n(),
    
    avg_total_dens = mean(total_density, na.rm = TRUE),
    sd_total_dens = sd(total_density, na.rm = TRUE),
    
    avg_legal_dens = mean(legal_density, na.rm = TRUE),
    sd_legal_dens = sd(legal_density, na.rm = TRUE),
    
    avg_sublegal_dens = mean(sublegal_density, na.rm = TRUE),
    sd_sublegal_dens = sd(sublegal_density, na.rm = TRUE),
    
    avg_spat_dens = mean(spat_density, na.rm = TRUE),
    sd_spat_dens = sd(spat_density, na.rm = TRUE),
    
    .groups = 'drop'
  )

station_info <- spf_df %>%
  select(Year, Station, Evaluation, Management.Area, Latitude, Longitude, Material.Age, B.Do, B.Temp, B.Sal) %>%
  distinct(Year, Station, .keep_all = TRUE)

spf_df <- left_join(avgs, station_info, by = c("Year", "Station"))
spf_df[['Station']] <-as.character(spf_df[["Station"]])


#IMPORT TRIGGER SAMPLING DENSITY DATASETS
setwd("S:/16. Trigger Sampling/Data/2025/preseason/Analysis")
df1 <- read.csv("P628_triggerpreseason2025densities.csv")
setwd("S:/16. Trigger Sampling/Data/2025/midseason/Analysis")
df2 <- read.csv("P628_triggermidseason2025densities.csv")

trg_df <- rbind(df1,df2)
rm(df1,df2)

trg_df <- trg_df %>%
  select(-SID, -Deployment.Year, -S.Sal, -X)%>%
  rename( #newname = oldname; averages already calculated for trigger collection sites
    n = Sample,
    avg_total_dens = total_density,
    avg_legal_dens = legal_density,
    avg_sublegal_dens = sublegal_density,
    avg_spat_dens = spat_density
  )%>%
  mutate(#create numeric columns full of NAs since averages & n already established, sd not calculated 
    sd_total_dens = NA_real_,
    sd_legal_dens = NA_real_,
    sd_sublegal_dens = NA_real_,
    sd_spat_dens = NA_real_
  )
  
# Stacks rows and matches columns perfectly by name
timeseries <- bind_rows(spf_df, trg_df)

######################################
#FORMAT TO GO INTO ARCGIS & DASHBOARD#
######################################

#function to convert coordinate notation from DDM to DD
ddm_to_dd <- function(ddm) {
  parts <- strsplit(trimws(ddm), "\\s+")[[1]]
  degrees <- as.numeric(parts[1])
  minutes <- as.numeric(parts[2])
  degrees + (minutes / 60)
}


GIS <- timeseries%>%
  rename( #rename cols
    Survey_Year = Year,
    LatDDM = Latitude,
    LonDDM = Longitude,
    Total_density = avg_total_dens,
    Legal_density = avg_legal_dens,
    Sublegal_density = avg_sublegal_dens,
    Spat_density = avg_spat_dens,
    Reef_Age = Material.Age,
    Management_Area = Management.Area)%>%
  # apply coordinate conversion function to make new cols
    mutate(
      Latitude = sapply(LatDDM, ddm_to_dd),
      Longitude = -(sapply(LonDDM, ddm_to_dd))
    )%>%
  select(-sd_total_dens, -sd_legal_dens, -sd_sublegal_dens, -sd_spat_dens)%>%
  #set Recent status based on year and evaluation to reflect most recent survey effort
  group_by(Station) %>%
  mutate(Recent = ifelse(Survey_Year == max(Survey_Year), "Y", "N")) %>%
  group_by(Station, Survey_Year) %>%
  mutate(Recent = ifelse(Recent == "Y" & any(Evaluation == "mid-season"), 
                         ifelse(Evaluation == "mid-season", "Y", "N"), 
                         Recent)) %>%
  ungroup()

#assign RHM status based on stations
RHM_stations <- c('20002', '20003', '20004', '21001', '21004', '22001', '22002', '22003', '23001','23002','23004','24004', '24006', '25001', '25003' , '25004', '25005', '25006')
GIS <- GIS %>%
  group_by(Station)%>%
  mutate(Rotational = ifelse(Station %in% RHM_stations, "Y", "N"))%>%
  #put in order for dashboard readability
  select(Station, Survey_Year, Evaluation, Recent, Reef_Age, n, Total_density, Legal_density, Sublegal_density, Spat_density, Management_Area, Rotational, Latitude, Longitude, B.Do, B.Sal, B.Temp)


#output 
setwd("S:/7. Cultch Planting/6. Monitoring and Data/p628 monitoring/5. Analysis/timeseries")
write.csv(GIS, "tong_dash_dataset2.csv")


