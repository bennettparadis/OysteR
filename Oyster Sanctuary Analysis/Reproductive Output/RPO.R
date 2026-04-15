setwd("S:/8. Oyster Sanctuaries/3. Monitoring and Data/1. Oyster Sanctuary (OS)/5. Analysis/2019-current R")

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
#library(reshape2)
#library(rcompanion)
#library(FSA)
#library(multcompView)
#library(multcomp)
#library(Rcmdr)

# Annual Data Import
OS_import <- read.csv("All 19-22 R Data.csv") #Read in ANNUAL Dataset
data.frame(OS_import)

#remove NA rows
OS_import <- OS_import[complete.cases(OS_import$Year),]

#remove non-extracted rows, Cedar Island
OS_import<-subset(OS_import, OS_import$Collection.Method != "Non-Extracted")
OS_import <- subset(OS_import, OS_import$OS.Name != "Cedar Island")

data<- dplyr::rename(OS_import,OS_ID=OS.ID,OS_Name=OS.Name,Site_ID=Site..,SH_mm=SH..mm., Size_Class=Size.Class)

data<-data %>% #cut down dataset to only include what is needed
  select(Year,OS_ID,OS_Name,Site_ID,SID,SH_mm)


####Format
data$OS_ID<-as.numeric(gsub("OS-","",data$OS_ID)) #remove OS-" from rows under OS_ID, change to numeric variables
data$SH_mm<-as.numeric(data$SH_mm)
data$OS_ID<-as.character(data$OS_ID)
data$Year<-as.character(data$Year)



# Assign size classes to each oyster/row
data1 <- data%>%
  mutate(Size_Class = case_when(
    between(SH_mm, 0, 15) ~ 'SC1',
    between(SH_mm, 16, 30) ~ 'SC2',
    between(SH_mm, 31, 45) ~ 'SC3',
    between(SH_mm, 46, 60) ~ 'SC4',
    between(SH_mm, 61, 75) ~ 'SC5',
    between(SH_mm, 76, 300) ~ 'SC6',
  ))

#edit Size classes to remove individuals under a certain size so as to not create biased results favoring high densities of small oysters
#data1 <- subset(data1, data1$Size_Class !='SC1')


# Calculate fecundity for each oyster. Input equation coefficients here. y = a * e^(b*x), where x is LVL and y is eggs per oyster, following Mroch et al. 2012
# used coefficients that were averaged from Mroch equations (excluding Hatteras and Ocracoke Sanctuaries)
August_data <- data1%>%
  mutate(per_cap_fecundity =  42.62857 * exp(0.041429 * SH_mm))

#Calculate the mean per-capita fecundity of oysters in a size class at a single site at a sanctuary during a year; also calculate the standard deviation within that grouping; 
August_data <- August_data%>%
  group_by(Year, OS_Name, Site_ID, Size_Class)%>%
  mutate(Fsc = mean(per_cap_fecundity), 
         SDsc = sd(per_cap_fecundity))%>%
  mutate(SDsc= ifelse(is.na(SDsc), 0, SDsc))%>%
  ungroup()



##Generates a random coefficient for each row to be used in the summation formula to estimate reproductive potential per square meter; calculates a simulated RPO value using
# the equation from Mroch et al 2012, where Fsc is mean per capita fecundity in a size class at a sanctuary, and SDsc is the correspondng standard deviation
August_data <- August_data%>%
  rowwise()%>%
  mutate(R = runif(1, min = -1, max =1))%>%
  mutate(sim_fecundity = Fsc + (SDsc * R))

#Any negative simulated fecundity values are replaced with 0s to represent males
August_data$sim_fecundity[August_data$sim_fecundity < 0] <- 0


#sums the simulated fecundity for each size class within a site at a sanctuary for a given year, THEN
#sums the size classes to generate a per square meter reproductive potential output; sum is multiplied by 4 since samples were collected on 1/4 meter quadrats
Aug_SC_Fecund <- August_data%>%
  group_by(Year, OS_Name, Site_ID, SID, Size_Class)%>%
  summarize(SC_RPO = sum(sim_fecundity))%>%
  ungroup()%>%
  group_by(Year, OS_Name, Site_ID, SID)%>%
  summarize(Site_RPO_sq_meter = 4*sum(SC_RPO))%>%
  ungroup()


#extract environmental variables from original dataset
environ_data <-OS_import%>%
  distinct(SID, .keep_all = TRUE)%>%
  select(SID, Material,Latitude, Longitude,Material.Age,Oyster.Cover,Mussel.Cover,Algae.Cover,Sedimentation,Boring.Sponge,Strata,Sample.Depth,OS.Depth,Relief,S.DO,B.DO,S.Sal,B.Sal,S.Temp,B.Temp)


#join August and Environmental dataframes 
August_Site_RPO <- merge(Aug_SC_Fecund, environ_data, by = "SID")%>%
  select(-SID, -Latitude, -Longitude, -Mussel.Cover, -Algae.Cover, -Sedimentation, -Boring.Sponge, -Strata, -Sample.Depth, -OS.Depth, -Relief, -S.DO, -B.DO, -S.Sal, -B.Sal, -S.Temp,-B.Temp)


#average the RPOs per sq meter for each material type on a sanctuary in a given year
August_Avg_RPO <- August_Site_RPO%>%
  group_by(OS_Name,Material)%>%
  mutate(avg_RPO_sq_meter = mean(Site_RPO_sq_meter))%>%
  ungroup()%>%
  select(OS_Name, avg_RPO_sq_meter, Material)%>%
  distinct(OS_Name, Material, .keep_all = T)



#import sanctuary area estimates, clean up environment
setwd("S:/8. Oyster Sanctuaries/3. Monitoring and Data/1. Oyster Sanctuary (OS)/5. Analysis/2019-current R/RPO")
sanctuaries <- read.csv("Sanctuaries.csv")
rm(August_data, Aug_SC_Fecund, August_Site_RPO,OS_import)


#determine RPOs per sq meter for materials, including reef balls & consolidated concrete
#merge dataframes
Aug_OS_RPO <- merge(August_Avg_RPO, sanctuaries, by = c("OS_Name", "Material"), all = TRUE)%>%
  select(-OS, -Acres, -Sample.Method)


#remove Cedar Island & Long Shoal
Aug_OS_RPO <- subset(Aug_OS_RPO , Aug_OS_RPO$OS_Name != "Cedar Island")
Aug_OS_RPO <- subset(Aug_OS_RPO , Aug_OS_RPO$OS_Name != "Long Shoal")



#Calculate an average for observed materials by assuming it's the average density of the material types found in the sanctuary
Aug_OS_RPO <-Aug_OS_RPO%>%
  group_by(OS_Name)%>%
  mutate(mean_RPO = mean(avg_RPO_sq_meter, na.rm=TRUE),
         avg_RPO_sq_meter = ifelse(is.na(avg_RPO_sq_meter), mean_RPO, avg_RPO_sq_meter))%>%
  select(-mean_RPO)%>%
  ungroup()


#Creates a weighted average of the RPO per sq meter for each material type based on the Area percentage; then adds up by sanctuary to get a weighted average RPO 
August_RPO_sq_m_Rank <- Aug_OS_RPO%>%
  group_by(OS_Name)%>%
  mutate(wt_RPO = avg_RPO_sq_meter * Area.percentage)%>%
  summarize(August_RPO_sq_m = sum(wt_RPO))%>%
  ungroup()


#Using the RPO per sq m, estimates the entire Sanctuary's RPO by multiplying it by the total material footprint (area)
Aug_Total_RPO <- Aug_OS_RPO%>%
  mutate(material_RPO = avg_RPO_sq_meter * Footprint..sq.meters.)%>%
  group_by(OS_Name)%>%
  summarise(Aug_Total_RPO = sum(material_RPO))


###################################
###################################
#Repeat for May
###################################
###################################

May_data <- data1%>%
  mutate(per_cap_fecundity =  243.975 * exp(0.05625 * SH_mm))


#Calculate the mean per-capita fecundity of oysters in a size class at a single site at a sanctuary during a year; also calculate the standard deviation within that grouping; 
May_data <- May_data%>%
  group_by(Year, OS_Name, Site_ID, Size_Class)%>%
  mutate(Fsc = mean(per_cap_fecundity), 
         SDsc = sd(per_cap_fecundity))%>%
  mutate(SDsc= ifelse(is.na(SDsc), 0, SDsc))%>%
  ungroup()



##Generates a random coefficient for each row to be used in the summation formula to estimate reproductive potential per square meter; calculates a simulated RPO value using
# the equation from Mroch et al 2012, where Fsc is mean per capita fecundity in a size class at a sanctuary, and SDsc is the correspondng standard deviation
May_data <- May_data%>%
  rowwise()%>%
  mutate(R = runif(1, min = -1, max =1))%>%
  mutate(sim_fecundity = Fsc + (SDsc * R))

#Any negative simulated fecundity values are replaced with 0s to represent males
May_data$sim_fecundity[May_data$sim_fecundity < 0] <- 0


#sums the simulated fecundity for each size class within a site at a sanctuary for a given year, THEN
#sums the size classes to generate a per square meter reproductive potential output; sum is multiplied by 4 since samples were collected on 1/4 meter quadrats
May_SC_Fecund <- May_data%>%
  group_by(Year, OS_Name, Site_ID, SID, Size_Class)%>%
  summarize(SC_RPO = sum(sim_fecundity))%>%
  ungroup()%>%
  group_by(Year, OS_Name, Site_ID, SID)%>%
  summarize(Site_RPO_sq_meter = 4*sum(SC_RPO))%>%
  ungroup()



#join May and environmental dataframes 
May_Site_RPO <- merge(May_SC_Fecund, environ_data, by = "SID")%>%
  select(-SID, -Latitude, -Longitude, -Mussel.Cover, -Algae.Cover, -Sedimentation, -Boring.Sponge, -Strata, -Sample.Depth, -OS.Depth, -Relief, -S.DO, -B.DO, -S.Sal, -B.Sal, -S.Temp,-B.Temp)


#average the RPOs per sq meter for each material type on a sanctuary in a given year
May_Avg_RPO <- May_Site_RPO%>%
  group_by(OS_Name,Material)%>%
  mutate(avg_RPO_sq_meter = mean(Site_RPO_sq_meter))%>%
  ungroup()%>%
  select(OS_Name, avg_RPO_sq_meter, Material)%>%
  distinct(OS_Name, Material, .keep_all = T)



#import sanctuary area estimates, clean up environment
setwd("S:/8. Oyster Sanctuaries/3. Monitoring and Data/1. Oyster Sanctuary (OS)/5. Analysis/2019-current R/RPO")
sanctuaries <- read.csv("Sanctuaries.csv")
rm(May_data, May_SC_Fecund, May_Site_RPO)


#determine RPOs per sq meter for materials, including reef balls & consolidated concrete
#merge dataframes
May_OS_RPO <- merge(May_Avg_RPO, sanctuaries, by = c("OS_Name", "Material"), all = TRUE)%>%
  select(-OS, -Acres, -Sample.Method)


#remove Cedar Island & Long Shoal
May_OS_RPO <- subset(May_OS_RPO , May_OS_RPO$OS_Name != "Cedar Island")
May_OS_RPO <- subset(May_OS_RPO , May_OS_RPO$OS_Name != "Long Shoal")



#Calculate an average for observed materials by assuming it's the average density of the material types found in the sanctuary
May_OS_RPO <-May_OS_RPO%>%
  group_by(OS_Name)%>%
  mutate(mean_RPO = mean(avg_RPO_sq_meter, na.rm=TRUE),
         avg_RPO_sq_meter = ifelse(is.na(avg_RPO_sq_meter), mean_RPO, avg_RPO_sq_meter))%>%
  select(-mean_RPO)%>%
  ungroup()


#Creates a weighted average of the RPO per sq meter for each material type based on the Area percentage; then adds up by sanctuary to get a weighted average RPO 
May_RPO_sq_m_Rank <- May_OS_RPO%>%
  group_by(OS_Name)%>%
  mutate(wt_RPO = avg_RPO_sq_meter * Area.percentage)%>%
  summarize(May_RPO_sq_m = sum(wt_RPO))%>%
  ungroup()


#Using the RPO per sq m, estimates the entire Sanctuary's RPO by multiplying it by the total material footprint (area)
May_Total_RPO <- May_OS_RPO%>%
  mutate(material_RPO = avg_RPO_sq_meter * Footprint..sq.meters.)%>%
  group_by(OS_Name)%>%
  summarise(May_Total_RPO = sum(material_RPO))



###################################
###################################
##Combine Aug & May for csv files##
###################################
###################################

#Combine datasets for ranking normalized sanctuary RPOs per square meter & write csv file

OS_RPO_sq_m <- merge(May_RPO_sq_m_Rank, August_RPO_sq_m_Rank, by = "OS_Name", all.x = T)

write.csv(OS_RPO_sq_m, "OS_RPO_normalized.csv", row.names = FALSE)



#Combine datasets for ranking TOTAL sanctuary RPO & write csv file

OS_RPO_total <- merge(Aug_Total_RPO, May_Total_RPO, by = "OS_Name", all.x = T)

write.csv(OS_RPO_total, "OS_RPO_total.csv", row.names = FALSE)
