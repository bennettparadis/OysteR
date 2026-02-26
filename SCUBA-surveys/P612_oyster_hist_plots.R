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


#### Length Frequency Plots - use this script after resampling has been done ####
# resampling is conducted on excavated sites where subsampling was utilized #
# the resampling script will output the 2025 R Data csv file #
setwd("S:/8. Oyster Sanctuaries/3. Monitoring and Data/1. Oyster Sanctuary (OS)/5. Analysis/2025 R")
df <- read.csv("2025_hist_data.csv")

datum<-subset(df, df$Collection.Method == "Extraction")
check <- subset(df, df$Collection.Method == "Observation") #check to make sure correct sample sizes

datum$OS.ID<-as.numeric(datum$OS.ID)

datum <- datum %>%#cut down dataset to only include what is needed, renaming condensed dataset as "data"
  select(Year, Month, Day, OS.ID,OS.Name,Site_ID,LVL) %>%
  arrange(OS.ID)

Num_quads<-datum %>% #Calculate number of quadrats taken for each site (will be used later to standardize sampling) 
  group_by(OS.ID)%>%
  dplyr::mutate(count=n_distinct(Site_ID))%>%
  rename(Quad_Count = "count")%>%
  distinct(OS.ID,Quad_Count)

Num_quads$vector <- rownames(Num_quads) # Loops don't like skipped values for iterations (sanctuaries no longer in OSP, such as OS.ID 4, 6, or for sanctuaries we don't end up sampling), so create a list of 1:n to then add as a column for loop to reference & follow along
datum<-left_join(datum,Num_quads,by="OS.ID") #R requires a long data format, so join "Num_quad" with "data"; adds vector column to "data" dataframe
datum<-na.omit(datum) #removes any instances of "na"

br = seq(0,165,by=5) #creates a sequence; start, end, step
ranges <- c(5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100,105,110,115,120,125,130,135,140,145,150,155,160,165)
#ranges <- c(10,20,30,40,50,60,70,80,90,100,110,120,130,140)
plist <- list()

loop.vector <-1:(nrow(Num_quads)) #create vector for loop to cycle through (set as total # sanctuaries sampled that year)
y.grob<-textGrob(expression("Frequency (Oysters per m"^2*")"),rot=90,gp=gpar(fontsize=24,fontface="plain"))
x.grob<-textGrob("LVL (mm)",gp=gpar(fontsize=24,fontface="plain"))
title=textGrob("2025 Size Frequencies",gp=gpar(fontsize=20,fontface="plain"))


# GENERATE AND SAVE ALL INDIVIDUAL SANCTUARY PLOTS
for (i in loop.vector) { # Loop over loop.vector to get multi-panel plots
  Sanctuary<-datum %>%
    filter(vector==i)
  freq <- hist(Sanctuary$LVL, breaks=br, include.lowest=TRUE, plot=FALSE)
  Count<-data.frame(range = ranges, count = (freq$counts/unique(Sanctuary$Quad_Count))*4)
  
  
  # Set base y-axis limit and adjust for outliers
  y_limit_base <- 250 # Typical maximum for most sites
  max_bin <- max(Count$count, na.rm = TRUE)
  
  if (max_bin > y_limit_base) {
    y_limit <- ceiling(max_bin / 250)  * 250 # Expand axis for extreme sites
    clipped <- TRUE
  } else {
    y_limit <- y_limit_base
    clipped <- FALSE
  }
  
  plist[[i]]<-ggplot(Count, aes(x=range, y=count)) +
    geom_bar(stat="identity",color="black", fill="grey") +
    geom_vline((aes(xintercept=75)), color="black", linetype="dashed", linewidth=.5)+
    geom_vline((aes(xintercept=25)), color="black", linetype="dotdash", linewidth=.5)+
    ylim(0,y_limit)+ #can toggle off the y_limit line and replace y_limit here with values - 400, 600, etc
    theme_classic()+
    geom_text(data = Num_quads %>%
                filter(vector == i),
              mapping = aes(x=130, y=150, 
                            label=paste0(Sanctuary$Month[1],"-", Sanctuary$Day[1],"-",Sanctuary$Year[1], "\n", "n =",Quad_Count)
                            ), 
              hjust=0) +
    #theme(plot.title = element_text(hjust=0.5), #toggle these three lines off/on for individual/grid plots
    #axis.title.x=element_blank(),
    #axis.title.y=element_blank())+
    labs(x="LVH (mm)", y = expression("Frequency (Oysters/m"^2*")"))+  #toggle this line on/off for individual/grid plots
    ggtitle(Sanctuary$OS.Name)+
    theme(plot.title = element_text(size=20,hjust = 0.5))
  plist[i]
  ggsave(file = paste0("2025_histogram", Sanctuary$OS.Name[1], ".tiff"),width = 5, height = 4, dpi = 300, units = "in",) #toggle this line off for grid plot
}


# #PLOT A SPECIFIC SANCTUARY
# x<-1 #replace "i" with whatever sanctuary (1-14) you want to generate
# plist[14]

##############################
#PLOT ALL SANCTUARIES IN GRID#
##############################
for (i in loop.vector) { # Loop over loop.vector to get multi-panel plots
  Sanctuary<-datum %>%
    filter(vector==i)
  freq <- hist(Sanctuary$LVL, breaks=br, include.lowest=TRUE, plot=FALSE)
  Count<-data.frame(range = ranges, count = (freq$counts/unique(Sanctuary$Quad_Count))*4)
  
  label_info <- Sanctuary%>%
    summarise(
      Month = first(Month),
      Day = first(Day),
      Year = first(Year),
      Quad_Count = unique(Quad_Count)
    )
  
  # Set base y-axis limit and adjust for outliers
  y_limit_base <- 250 # Typical maximum for most sites
  max_bin <- max(Count$count, na.rm = TRUE)
  
  if (max_bin > y_limit_base) {
    y_limit <- ceiling(max_bin / 250)  * 250 # Expand axis for extreme sites
    clipped <- TRUE
  } else {
    y_limit <- y_limit_base
    clipped <- FALSE
  }
  
  plist[[i]]<-ggplot(Count, aes(x=range, y=count)) +
    geom_bar(stat="identity",color="black", fill="grey") +
    geom_vline((aes(xintercept=75)), color="black", linetype="dashed", linewidth=.5)+
    geom_vline((aes(xintercept=25)), color="black", linetype="dotdash", linewidth=.5)+
    ylim(0,y_limit)+ #can toggle off the y_limit line and replace y_limit here with values - 400, 600, etc
    theme_classic()+
    geom_text(data = label_info,
              mapping = aes(x=130, y=150, 
                            label=paste0(Month,"-", Day,"-",Year, "\n", "n =",Quad_Count)
              ), 
              hjust=0) +
    #theme(plot.title = element_text(hjust=0.5), #toggle these three lines off/on for individual/grid plots
    #axis.title.x=element_blank(),
    #axis.title.y=element_blank())+
    labs(x="LVH (mm)", y = expression("Frequency (Oysters/m"^2*")"))+  #toggle this line on/off for individual/grid plots
    ggtitle(Sanctuary$OS.Name)+
    theme(plot.title = element_text(size=20,hjust = 0.5))
  plist[i]
  ggsave(file = paste0("2025_histogram", Sanctuary$OS.Name[1], ".tiff"),width = 5, height = 4, dpi = 300, units = "in",) #toggle this line off for grid plot
}

for (i in seq(1,length(plist), 15)) { #loop for plotting all sanctuaries on grid (3 columns) with labels, Change "(plist), x)" to total # sanctuaries sampled with excavated material (Long Shoal does not count!!)
  g=grid.arrange(grobs=plist[i:(i+14)], #change "i+x" to i+ (# sanctuaries sampled -1)
                 ncol=3,bottom=x.grob, left=y.grob, top=title)
  ggsave(file = paste0("2025 Oyster Sanctuary Demographics.tiff"), width = 12, height = 10, dpi = 300, units = "in", g)
}

