################################################################################
# SCRIPT NAME     : Oyster Tong Survey (P628) Percent legal calculations       #
# DESCRIPTION &   : Determine percentage of legal oyster from sites visited    #
#                 : during trigger/in-season management survey of Pamlico Sound#
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
# OUTOUT FILES    : csv of percent legal calculation for each site sampled     #
################################################################################
# MODIFICATIONS   :                                                            #
#---------------- :                                                            #
# DATE            :                                                            #
# CHANGE #        :                                                            #
# PROGRAMMER      :                                                            #
# DESCRIPTION     :                                                            #
################################################################################

# update R & RStudio without IT help!
# library(installr)
# updateR()

library(dplyr)
library(tidyr)

#set working directory & upload csv
setwd("S:/14. DORAs/FMP Monitoring Protocols/P628 Tong Surveys")
df <- read.csv("P628_test_data.csv")

area_inf <- df%>%
  select(Management.Area, Limit.Area, SID, Total.Oyster.Count)%>%
  distinct(Management.Area, Limit.Area, SID, Total.Oyster.Count)

legal_ratios <- df %>%
  # drop spat and NA row(s)
  filter(Size.Class != "Spat", !is.na(SID)) %>%
  group_by(SID, Size.Class) %>%
  summarise(count = dplyr::n(), .groups = "drop") %>%
  pivot_wider(names_from = Size.Class,
              values_from = count,
              values_fill = 0) %>%
  mutate(non_spat_count = rowSums(across(where(is.numeric))),
         percent_legal = if_else(non_spat_count > 0, (coalesce(Legal, 0) / non_spat_count), NA_real_))

legal_ratios <- left_join(legal_ratios, area_inf, by = 'SID')

write.csv(legal_ratios, "test_legal_ratios.csv")


