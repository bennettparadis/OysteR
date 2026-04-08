library(stats)
library(dplyr)
library(ggplot2)


setwd("S:/8. Oyster Sanctuaries/3. Monitoring and Data/1. Oyster Sanctuary (OS)/5. Analysis/2019-current R")

OS_data <- read.csv("All 19-24 R Data.csv")
#truth <- read.csv("1700_truth.csv")
OS_data <- OS_data%>%
  select(SID, SH..mm.)%>%
  rename(SH = SH..mm.)

#select a particular quadrat of interest using the SID
 truth <- OS_data%>%
   filter(SID == '2020-6-26-OS-12-5')%>%
   select(SH)


#create into a numeric list
truth <- as.numeric(truth$SH)


#number of observations, used to dictate the number of rows in a bootstrap dataframe
n <- length(truth)

sub_size = 400
#create a subsample of 400
subsample <- sample(x=truth, size = sub_size)


#create a bootstrap dataset that can be used for LFPs--single use for histogram
booted <- sample(subsample, size = n, rep=T)


#for validating that this sample size is adequate in simulating a similar distribution as seen in truth, create a dataframe with bootstrap iterations
#specify how many bootstrap iterations you want & create an empty dataframe with that many columns
num_boots <- 5000
boot_data <- data.frame(matrix(ncol = num_boots, nrow=n))
column_names <- paste0("B",1:num_boots)
colnames(boot_data) <- column_names

# for loop populates the dataframe of bootstrap iterations
for (i in 1:num_boots) {
  subsample <- sample(x=truth, size = sub_size)
  booted <- sample(subsample, size = n, rep = T)
  boot_data [, i] <- booted
}


#check histograms for rough idea of distributions; can specify any random iteration of bootstrap
hist(subsample)
hist(truth)
hist(booted)



#run Kolmogorov-Smirnov test comparing truth and subsample
ks_test_1 <- ks.test(truth, subsample)

print(ks_test_1)
#A p-value greater than 0.05 suggests that there is not enough evidence to reject the null hypothesis 
#that the distributions of the truth data and the subsample are significantly different. 
#In other words, if the p-value is relatively high, its indicates that the observed difference between the two distributions could be due to random sampling 
#variability rather than a true difference in their underlying distributions.

#compare subsample, and boot
ks_test_2 <- ks.test(subsample, booted)
print(ks_test_2)

ks_test_3 <- ks.test(truth, booted)
print(ks_test_3)

#compare the bootstrap data to truth -- create a dataframe with n*1000 iterations, see how many p-values return >0.05.

p_values <- vector("numeric", length(boot_data))  # Initialize a numeric vector to store the p-values
ks_results <- list()

count_above_threshold <- 0  # Initialize a count variable

for (i in 1:num_boots) {
  ks_results[[i]] <- ks.test(truth, boot_data[[i]])
  if (ks_results[[i]]$p.value > 0.05) {
    count_above_threshold <- count_above_threshold + 1
  }
}

#will spit out 50+ warning messages -- that's just the automatic warning message that comes from running a KS test

#reports how many of the iterations had distributions that were NOT statistically different than the truth dataset (aim is for 950+ for appropriate confidence level)
print(count_above_threshold/5000)

