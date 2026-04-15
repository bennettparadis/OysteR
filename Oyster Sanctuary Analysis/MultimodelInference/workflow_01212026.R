setwd("S:/8. Oyster Sanctuaries/3. Monitoring and Data/1. Oyster Sanctuary (OS)/5. Analysis/2007-2025/pub/results_reduced/01212026")

library(dplyr)
library(grid)
library(gridExtra)
library(ggplot2)
library(tidyr)
library(data.table)
library(stringr)
library(nlme)
library(lme4)
library(car)
library(MuMIn)
library(glmmTMB)
library(olsrr)
library(DHARMa)
library(splines)
library(emmeans)
library(mgcv)

# #update R without IT help!
# library(installr)
# updateR()

#############################################
###  SHAPE DATAFRAME TO BE USED IN MODELS ###
#############################################
data <- read.csv("RF_predictions_reduced_10202025.csv")

df <- data%>%
  select(SID, Year, OS_ID, Season, Material, Material_Age, Sedimentation, Boring_Sponge, Sample_Depth, Relief, B.DO, B.Sal, total, legal, spat)%>%
  as.data.frame()

#make site-year variable to test for random effect interaction term
df$site_year <- interaction(df$OS_ID, df$Year, drop = TRUE)

#Make categorical variables into factors for next steps in model testing
df2 <- df%>%
  mutate_at(c('OS_ID','Material', 'Season' , 'Year', 'Sedimentation', 'Boring_Sponge', 'site_year'), as.factor)%>%
  as.data.frame()


# Dataframe info for 1363 rows
# Collection method: extraction - 999, observation - 364
# Season: 118 - winter, 81 spring, 197 fall, 967 summer
# Material: n (age range)
#   marl: 780 (0.17 - 29.17)
#   granite: 102 (1 - 8.92)
#   crushed con: 98 (1.1 - 11.33)
#   consol con: 78 (1.92 - 11.17)
#   reef ball: 243 (3.4 - 13.5)
#   basalt: 28 (2.92 - 9.25)
#   shell: 34 (16.25 - 19.33)

#trim dataset to only include those variables and complete rows
#n = 1174
model_vars <- c("total","legal", "spat", "Year", "OS_ID", "Season", "Boring_Sponge","Material_Age", "Material", "Relief", "Sample_Depth", "B.Sal", "B.DO", "Sedimentation", "site_year")
complete_df3 <- df2[complete.cases(df2[,model_vars]),]

#remove extreme outliers from Brant Island 2025 (total = 7136, 6376) & Swan Island 2024 (total = 6776)
#improves the residual diagnostics
outliers <- c("2025-7-15-19-5", "2025-7-15-19-7", "2024-7-29-15-15")

#complete rows minus the three outliers -> 1171
df <- complete_df3[!complete_df3$SID %in% outliers, ]

#relevel material so that marl is the reference
df$Material <- relevel(df$Material, ref = "Marl")
#re-label categorical vairables
df$Sedimentation <- factor(df$Sedimentation,
                           levels = c(1,2,3,4),
                           labels = c("None", "Low", "Medium", "High"))
df$Boring_Sponge <- factor(df$Boring_Sponge,
                           levels = c(0, 1),
                           labels = c("Absent", "Present"))


# #Preliminary diagnostics of the independent parameters
#check for collinearity among the independent parameters, set as numerical values - cor() reports Pearson's correlation coefficients (r-values)
collin <- cor(df %>% select(Material_Age, Relief, Sample_Depth, B.DO, B.Sal), use = "complete.obs")
collin <- as.data.frame(collin)
collin
#write.csv(collin, "results.csv")

#Check for collinearity among variables
#If any VIF > 5 (or 10 in some fields), consider removing or combining variables.
vif_results <- vif(lm(total ~ Material_Age + Material + Season + Boring_Sponge + Sample_Depth + Relief + Sedimentation + B.DO + B.Sal, data = df))
vif_results <- as.data.frame(vif_results)
vif_results
#write.csv(vif_results, "vif.csv")


# #distribution of response - oyster density
hist(df$total)
hist(df$legal)
hist(df$spat)


#mean and variance relationship --> to determine nbinom1 or nbinom2
#nbinom1 --> mean-variance relationship is linear
# overdispersion is mild, var is only slightly larger

#nbinom2 --> mean-variance is quadratic
# overdispersion is severe; var >> mean

# Compute overall mean and variance
mean_total <- mean(df$total, na.rm = TRUE)
var_total <- var(df$total, na.rm = TRUE)

mean_legal <- mean(df$legal, na.rm = TRUE)
var_legal <- var(df$legal, na.rm = TRUE)

mean_spat <- mean(df$spat, na.rm = TRUE)
var_spat <- var(df$spat, na.rm = TRUE)

#the mean and variance differ significantly, by orders of magnitude
# this led to dramatic issues with overdispersion and when running residuals diagnostics
# without transforming the y-responses.

# Overall, the workflow of running and all-subsets analysis followed by multimodel inference
# hit bumps with all possible options (nbinom1, nbinom2, tweedie) 
# While one might have addressed problems with the residuals, it would fail to run or 
# generate model weights with dredge(). family=nbinom2 ran without issue, but exhibited
# problems with the residual diagnostic tests.




#### EXPLORATORY ANAYLSIS ####

# categorical variables
# Material type
plot(total~Material, data=df)
plot(legal~Material, data=df)
plot(spat~Material, data = df)

# Boring Sponge
plot(total~Boring_Sponge, data=df)
plot(legal~Boring_Sponge, data=df)
plot(spat~Boring_Sponge, data = df)

# Sedimentation
plot(total~Sedimentation, data=df)
plot(legal~Sedimentation, data=df)
plot(spat~Sedimentation, data = df)

# Season
plot(total~Season, data=df)
plot(legal~Season, data=df)
plot(spat~Season, data = df)


#continuous variables
# Material Age
plot(total~Material_Age, data=df)
plot(legal~Material_Age, data=df)
plot(spat~Material_Age, data = df)

# Sample Depth
plot(total~Sample_Depth, data=df)
plot(legal~Sample_Depth, data=df)
plot(spat~Sample_Depth, data = df)

# Relief
plot(total~Relief, data=df)
plot(legal~Relief, data=df)
plot(spat~Relief, data = df)

# Dissolved Oxygen
plot(total~B.DO, data=df)
plot(legal~B.DO, data=df)
plot(spat~B.DO, data = df)

# Salinity
plot(total~B.Sal, data=df)
plot(legal~B.Sal, data=df)
plot(spat~B.Sal, data = df)



##########################################
########## Y-TRANSFORMATIONS #############
##########################################

response_vars <- c("total", "legal", "spat")

for (y in response_vars) {
  df[[paste0(y, "_log")]] <- log(df[[y]]+1)
  df[[paste0(y, "_log10")]] <- log10(df[[y]]+1)
  df[[paste0(y, "_sqrt")]] <- sqrt(df[[y]])
}

# Compute overall mean and variance for each to determine family/distribution model
y_vars <- c("total", "legal", "spat", "total_log", "legal_log", "spat_log", "total_log10", "legal_log10", "spat_log10", "total_sqrt", "legal_sqrt", "spat_sqrt")

# Initialize an empty data frame for storing the results
mean_var <- data.frame(Variable = character(), Mean = numeric(), Variance = numeric(), stringsAsFactors = FALSE)

# Loop through each variable in y_vars
for (var in y_vars) {
  # Calculate mean and variance
  mean_value <- mean(df[[var]], na.rm = TRUE)
  var_value <- var(df[[var]], na.rm = TRUE)
  
  # Append the results to the mean_var data frame
  mean_var <- rbind(mean_var, data.frame(Variable = var, Mean = mean_value, Variance = var_value))
}

#write.csv(mean_var, "mean_var_table.csv")

############################################################################
################# TAKE AWAY ON TRANSFORMATIONS AND FAMILIES ################
############################################################################

#stick with count models (nbinom1, nbinom2, poisson, and genpois) if NOT transforming y
#interpretation is relatively straightforward

#if wanting to stabilize varaince and improve assumptions, then transformations would be appropriate
#would use a gaussian distribution 

#non-transformed data --> use nbinom2

#Log10 - mean greater than variance (2-4x)
#NatLog - mean ~ variance 
#Sqrt - variance > mean (4-8x) 
#transformed data would need gaussian distribution as assumption, but with mean-variance relationships
#and interpretability, probably best to stick with not transforming the y


#######################################################
######### FIXED EFFECTS STRUCTURE #####################
#######################################################
# Function to create the GLMM model
create_glmm <- function(y_var, params, family_type, data) {
  # Create formula dynamically
  formula <- as.formula(
    paste(y_var, "~", paste(params, collapse = " + "), "+ (1|OS_ID/Year)"))
  
  # Run the model with the specified formula and family
  model <- glmmTMB(formula, family = family_type, data = data)
  
  # Return the model object (don't assign it globally)
  return(model)
}

# Define the variables to pass
# complex with interaction & quadratic terms
params_full <- c("Material_Age", "Material", 
                 "Relief", "Sample_Depth", 
                 "B.DO", "I(B.DO^2)", 
                 "B.Sal", "I(B.Sal^2)", 
                 "Sedimentation", "Boring_Sponge",
                 "Material_Age*Material")

# simplified parameters, no interaction nor quadratics
params_simp <- c("Material_Age", "Material", 
                 "Relief", "Sample_Depth", 
                 "B.DO", "B.Sal",  
                 "Sedimentation", "Boring_Sponge")

# parameters with quadratics, no interaction
params_quad <- c("Material_Age", "Material", 
                 "Relief", "Sample_Depth", 
                 "B.DO", "I(B.DO^2)", 
                 "B.Sal", "I(B.Sal^2)", 
                 "Sedimentation", "Boring_Sponge")

# parameters with interaction, no quadratics
params_int <- c("Material_Age", "Material", 
                 "Relief", "Sample_Depth", 
                 "B.DO", "B.Sal", 
                 "Sedimentation", "Boring_Sponge",
                 "Material_Age*Material")

# AIC function to compare various FE structures
FE_AIC <- function(y_var, family, data, fixed_sets, fixed_names) {
  
  model_list <- list()
  
  for (i in seq_along(fixed_sets)) {
    fe <- fixed_sets[[i]]
    model <- create_glmm(y_var, fe, family, data)
    
    model_name <- paste(y_var, fixed_names[i], sep = "_")
    model_list[[model_name]] <- model
  }
  
  aicc_table <- sapply(model_list, function(model) {
    k <- attr(logLik(model), "df")
    n <- nobs(model)
    aic <- AIC(model)
    aicc <- aic + (2 * k * (k + 1)) / (n - k - 1)
    c(AICc = aicc, df = k)
  })
  
  aicc_table <- t(aicc_table)
  aicc_table <- aicc_table[order(aicc_table[, "AICc"]), ]
  
  return(aicc_table)
}

fixed_sets <- list(params_simp, params_quad, params_int, params_full)
fixed_names <- c("simp", "quad", "int", "full")


##################################################################
###### TEST DENSITY MODELS FOR RE STRUCTURE ######
##############################################
#Total
total_FE_AIC <- FE_AIC("total", nbinom2, df, fixed_sets, fixed_names)
print(total_FE_AIC)
#best fitting model is full random effects structure with the interaction & no quadratic terms

#check the residuals of global before testing fixed effects structure
FE_total<- glmmTMB(total ~ Material_Age + Material+ Relief + Sample_Depth + 
                        Sedimentation + Boring_Sponge +
                        B.DO + B.Sal + 
                        Material_Age*Material + 
                        (1|OS_ID/Year), 
                        family = nbinom2, data=df, na.action=na.fail)

#DHARMa tests --> fails all tests
sim_res <- simulateResiduals(fittedModel = FE_total, plot = TRUE)


#Legal
legal_AIC <- FE_AIC("legal", nbinom2, df, fixed_sets, fixed_names)
print(legal_AIC)
#best fitting model is full random effects structure with all terms present

#check the residuals of global before testing fixed effects structure
FE_legal<- glmmTMB(legal ~ Material_Age + Material+ Relief + Sample_Depth + 
                        B.DO + I(B.DO^2) + B.Sal + I(B.Sal^2) +
                        Boring_Sponge + Sedimentation + Material_Age*Material +
                        (1|OS_ID/Year), 
                      family = nbinom2, data=df, na.action=na.fail)

#DHARMa tests --> fails all tests
sim_res <- simulateResiduals(fittedModel = FE_legal, plot = TRUE)



#Spat
spat_AIC <- FE_AIC("spat", nbinom2, df, fixed_sets, fixed_names)
print(spat_AIC)
#best fitting model is with interaction term and no quadratics

#check the residuals of global before testing fixed effects structure
FE_spat<- glmmTMB(spat ~ Material_Age + Material+ Relief + Sample_Depth + 
                        B.DO + B.Sal + Boring_Sponge + 
                        Sedimentation + Material_Age*Material +
                        (1|OS_ID/Year), 
                      family = nbinom2, data=df, na.action=na.fail)

#DHARMa tests --> fails all tests
sim_res <- simulateResiduals(fittedModel = FE_spat, plot = TRUE)


########################################################################################
### All subsets analysis and multimodel inferences with normal, non-transformed data ### 
########################################################################################

#setwd("S:/8. Oyster Sanctuaries/3. Monitoring and Data/1. Oyster Sanctuary (OS)/5. Analysis/2007-2025/pub/results_june")

###################################
###TOTAL DENSITy MODEL SELECTION###
###################################

#all subsets analysis with best model from parabolic testing 
norm_total2<- glmmTMB(total ~ Material_Age + Material+ Relief + Sedimentation + Boring_Sponge +
                       Sample_Depth + B.DO + I(B.DO^2) + B.Sal + I(B.Sal^2) + Material_Age*Material +
                       (1|OS_ID) +(1|Year), 
                     family = nbinom2, data=df, na.action=na.fail)

#check residuals of full model
sim_res <- simulateResiduals(fittedModel = norm_total2, plot = TRUE)



norm_total_AIC <- dredge(norm_total, rank = AICc)
norm_total_BIC <- dredge(norm_total, rank = BIC)

#warning message about cond((Int)) and disp((Int)) can be ignored

#calculate evidence ratios
weights <- norm_total_AIC$weight
best_weight <- max(weights)
evidence_ratios <- best_weight/weights

#add evidence ratios column to the dredge table
norm_total_AIC$ER <- round(evidence_ratios, 2)

#save results into a csv file
norm_total_resultsA <- as.data.frame(norm_total_AIC)
write.csv(norm_total_resultsA,"norm_total_resultsA.csv")
norm_total_resultsB <- as.data.frame(norm_total_BIC)
write.csv(norm_total_resultsB,"norm_total_resultsB.csv")

#if the weight of the best model is < 0.85, best to run a multi-model inference 
#this then averages the weights of variables from all models

mmin_norm_total = model.avg(get.models(norm_total_AIC, subset=TRUE))
summary(mmin_norm_total)

# repeats AIC table
# get variable weights --> probability that variable is in the true best model among those considered
# importance(test) --> outdated, now it's sw(test)
vw_norm_total <- as.data.frame(sw(mmin_norm_total))
write.csv(vw_norm_total,"vw_norm_total.csv")

# don't use the conditional table for variable coefficients; ignore z + p values (don't report with AIC?)

conf_ints_norm_total <- confint(mmin_norm_total)
coeffs_norm_total <- as.data.frame(coef(mmin_norm_total, full = TRUE))

coefficients_norm_total <- cbind(coeffs_norm_total, conf_ints_norm_total)
write.csv(coefficients_norm_total, "coeffs_norm_total.csv")




###################################
###LEGAL DENSITy MODEL SELECTION###
###################################

#with n=957 dataset (does not include boring sponge)

#all subsets analysis with best model from parabolic testing 
norm_legal <- glmmTMB(legal ~ Material_Age + Material+ Relief + Sedimentation +
                        Sample_Depth + B.DO + I(B.DO^2) + B.Sal + I(B.Sal^2) + Material_Age*Material +
                        (1|OS_ID) +(1|Year) + (1|OS_ID:Year), 
                      family = nbinom2, data=df, na.action=na.fail)

#check residuals of full model
sim_res <- simulateResiduals(fittedModel = norm_legal, plot = TRUE)

#Model selection with all subsets analysis
norm_legal_AIC <- dredge(norm_legal, rank = AICc)
norm_legal_BIC <- dredge(norm_legal, rank = BIC)

#warning message about cond((Int)) and disp((Int)) can be ignored

#calculate evidence ratios
weights <- norm_legal_AIC$weight
best_weight <- max(weights)
evidence_ratios <- best_weight/weights

#add evidence ratios column to the dredge table
norm_legal_AIC$ER <- round(evidence_ratios, 2)

#save results into a csv file
norm_legal_resultsA <- as.data.frame(norm_legal_AIC)
write.csv(norm_legal_resultsA,"norm_legal_resultsA.csv")
norm_legal_resultsB <- as.data.frame(norm_legal_BIC)
write.csv(norm_legal_resultsB,"norm_legal_resultsB.csv")

#if the weight of the best model is < 0.85, best to run a multi-model inference 
#this then averages the weights of variables from all models

mmin_norm_legal = model.avg(get.models(norm_legal_AIC, subset=TRUE))
summary(mmin_norm_legal)

# repeats AIC table
# get variable weights --> probability that variable is in the true best model among those considered
# importance(test) --> outdated, now it's sw(test)
vw_norm_legal <- as.data.frame(sw(mmin_norm_legal))
write.csv(vw_norm_legal,"vw_norm_legal.csv")

# don't use the conditional table for variable coefficients; ignore z + p values (don't report with AIC?)

conf_ints_norm_legal <- confint(mmin_norm_legal)
coeffs_norm_legal <- as.data.frame(coef(mmin_norm_legal, full = TRUE))

coefficients_norm_legal <- cbind(coeffs_norm_legal, conf_ints_norm_legal)
write.csv(coefficients_norm_legal, "coeffs_norm_legal.csv")


###################################
### SPAT DENSITy MODEL SELECTION###
###################################

#with n=958 dataset (does not include boring sponge)

#all subsets analysis with best model from parabolic testing 
norm_spat <- glmmTMB(spat ~ Material_Age + Material+ Relief + Sedimentation +
                        Sample_Depth + B.DO + I(B.DO^2) + B.Sal + I(B.Sal^2) + Material_Age*Material +
                        (1|OS_ID) +(1|Year) + (1|OS_ID:Year), 
                      family = nbinom2, data=df, na.action=na.fail)

#check residuals of full model
sim_res <- simulateResiduals(fittedModel = norm_spat, plot = TRUE)

#Model selection with all subsets analysis
norm_spat_AIC <- dredge(norm_spat, rank = AICc)
norm_spat_BIC <- dredge(norm_spat, rank = BIC)

#warning message about cond((Int)) and disp((Int)) can be ignored

#calculate evidence ratios
weights <- norm_spat_AIC$weight
best_weight <- max(weights)
evidence_ratios <- best_weight/weights

#add evidence ratios column to the dredge table
norm_spat_AIC$ER <- round(evidence_ratios, 2)

#save results into a csv file
norm_spat_resultsA <- as.data.frame(norm_spat_AIC)
write.csv(norm_spat_resultsA,"norm_spat_resultsA.csv")
norm_spat_resultsB <- as.data.frame(norm_spat_BIC)
write.csv(norm_spat_resultsB,"norm_spat_resultsB.csv")

#if the weight of the best model is < 0.85, best to run a multi-model inference 
#this then averages the weights of variables from all models

mmin_norm_spat = model.avg(get.models(norm_spat_AIC, subset=TRUE))
summary(mmin_norm_spat)

# repeats AIC table
# get variable weights --> probability that variable is in the true best model among those considered
# importance(test) --> outdated, now it's sw(test)
vw_norm_spat <- as.data.frame(sw(mmin_norm_spat))
write.csv(vw_norm_spat,"vw_norm_spat.csv")

# don't use the conditional table for variable coefficients; ignore z + p values (don't report with AIC?)

conf_ints_norm_spat <- confint(mmin_norm_spat)
coeffs_norm_spat <- as.data.frame(coef(mmin_norm_spat, full = TRUE))

coefficients_norm_spat <- cbind(coeffs_norm_spat, conf_ints_norm_spat)
write.csv(coefficients_norm_spat, "coeffs_norm_spat.csv")


############################################
### TESTING RESIDUALS OF TOP TWO MODELS  ### 
############################################

##### TOTAL DENSITY MODELS######
#check the best model for total density 
best1_total<- glmmTMB(total ~ Material_Age + Material+ Relief + Sample_Depth + 
                        Sedimentation + Material_Age*Material +
                   (1|OS_ID) +(1|Year) + (1|OS_ID:Year), 
                 family = nbinom2, data=df, na.action=na.fail)

#DHARMa tests
sim_res <- simulateResiduals(fittedModel = best1_total, plot = TRUE)
#fails all residual diagnostic tests

#check the second best model for total density 
best2_total<- glmmTMB(total ~ Material_Age + Material+ Relief + Sample_Depth + I(B.DO^2) + 
                        Sedimentation + Material_Age*Material +
                        (1|OS_ID) +(1|Year) + (1|OS_ID:Year), 
                      family = nbinom2, data=df, na.action=na.fail)

#DHARMa tests
sim_res <- simulateResiduals(fittedModel = best2_total, plot = TRUE)
#fails all residual diagnostic tests

#check the third best model for total density 
best3_total<- glmmTMB(total ~ Material_Age + Material+ Relief + Sample_Depth + I(B.DO^2) + 
                        Sedimentation + Material_Age*Material +
                        (1|OS_ID) +(1|Year) + (1|OS_ID:Year), 
                      family = nbinom2, data=df, na.action=na.fail)

#DHARMa tests
sim_res <- simulateResiduals(fittedModel = best2_total, plot = TRUE)
#fails all residual diagnostic tests



##### LEGAL DENSITY MODELS######
#check the best model for legal density 
best1_legal<- glmmTMB(legal ~ Material_Age + Material+ Relief + I(B.DO^2) 
                      + Material_Age*Material +
                   (1|OS_ID) +(1|Year) + (1|OS_ID:Year), 
                 family = nbinom2, data=df, na.action=na.fail)

#DHARMa tests
sim_res <- simulateResiduals(fittedModel = best1_legal, plot = TRUE)
#fails KS and dispersion test; passes outlier test


#check the second best model for total density 
best2_legal<- glmmTMB(legal ~ Material_Age + Material+ Relief + Sample_Depth + I(B.DO^2) + 
                   Material_Age*Material +
                   (1|OS_ID) +(1|Year) + (1|OS_ID:Year), 
                 family = nbinom2, data=df, na.action=na.fail)

#DHARMa tests
sim_res <- simulateResiduals(fittedModel = best2_legal, plot = TRUE)
#fails KS and dispersion test; passes outlier test


#check the best model for spat density 
best1_spat<- glmmTMB(spat ~ Material_Age + Material+ Relief + 
                       Sedimentation + Material_Age*Material +
                   (1|OS_ID) +(1|Year) + (1|OS_ID:Year), 
                 family = nbinom2, data=df, na.action=na.fail)

#DHARMa tests
sim_res <- simulateResiduals(fittedModel = best1_spat, plot = TRUE)
#fails all residual diagnostic tests


#check the best model for spat density 
best2_spat<- glmmTMB(spat ~ Material_Age + Material+ Relief +  
                       Sedimentation + I(B.Sal^2) + Material_Age*Material +
                       (1|OS_ID) +(1|Year) + (1|OS_ID:Year), 
                     family = nbinom2, data=df, na.action=na.fail)

#DHARMa tests
sim_res <- simulateResiduals(fittedModel = best2_spat, plot = TRUE)
#fails all residual diagnostic tests

################################################################################


##############################################################################
################# WORKFLOW FOR DREDGE & INF w/ SQRT TRANSFORMED DATA #########
##############################################################################

#prediction vs inference --> determine how to go about with model selection
#if prediction is the goal, then issues of convergence should be addressed so that the best fitting model can
#be determined. in this case, systematically removing fixed variables would be the route
# can remove salinity to fix widespread convergence issues and re-test RE structure
#lowest AIC is the full RE structure

#if inference or knowing the importance of fixed effects is the goal, then stick w/ whichever random effect
# structure converges and has the lowest AIC instead of removing fixed effects
#here we went with inference and moved past the convergence issues

#TOTAL
total_sqrt_FE_AIC <- FE_AIC("total_sqrt", gaussian, df, fixed_sets, fixed_names)
print(total_sqrt_FE_AIC)

# total_sqrt_full 8349.731 27 barely fails KS & quantile tests
# total_sqrt_int  8351.559 25 barely fails outlier test and quantile test
# total_sqrt_simp 8358.210 19
# total_sqrt_quad       NA 21 --> selecting for dredge()

# lowest AICcs were full fixed effects & only interaction; test residuals 

#check residual tests of each model to find the 'most stable'
total_sqrt1<- glmmTMB(total_sqrt ~ Material_Age + Material+ Relief + 
                       Boring_Sponge + Sample_Depth + 
                       B.DO + I(B.DO^2) + B.Sal + I(B.Sal^2) +
                       Sedimentation + 
                        Material_Age*Material +
                       (1|OS_ID/Year),   
                     family = gaussian, data=df, na.action=na.fail)

sim_res <- simulateResiduals(fittedModel = total_sqrt1, plot = TRUE)
#with quadratic terms and site*year, only barely fails KS test

total_sqrt2<- glmmTMB(total_sqrt ~ Material_Age + Material+ Relief + 
                        Boring_Sponge + Sample_Depth + 
                        B.DO + B.Sal +
                        Sedimentation + Material_Age*Material +
                        (1|OS_ID/Year),   
                      family = gaussian, data=df, na.action=na.fail)

sim_res <- simulateResiduals(fittedModel = total_sqrt2, plot = TRUE)
#without quadratic terms, fails outlier test, passes other tests
#with quads, fails KS and passes other tests

AIC(total_sqrt1, total_sqrt2)


#LEGAL
legal_sqrt_FE_AIC <- FE_AIC("legal_sqrt", gaussian, df, fixed_sets, fixed_names)
print(legal_sqrt_FE_AIC)


#best AICc with full fixed effects structure, interaction, and quadratics
# legal_sqrt_full 6493.956 27  
# legal_sqrt_int  6532.450 25
# legal_sqrt_simp 6555.495 19
# legal_sqrt_quad       NA 21

#no convergence problem with all variables; best RE structure is with full parameters (AICc = 6485.15)
legal_sqrt1<- glmmTMB(legal_sqrt ~ Material_Age + Material+ 
                        Relief + Sample_Depth + Boring_Sponge + Sedimentation +
                        B.DO+ I(B.DO^2) + 
                        B.Sal+ I(B.Sal^2) + 
                        Material_Age*Material +
                        (1|OS_ID/Year), 
                        family = gaussian, data=df, na.action=na.fail)

sim_res <- simulateResiduals(fittedModel = legal_sqrt1, plot = TRUE)

legal_sqrt2<- glmmTMB(legal_sqrt ~ Material_Age + Material+ 
                        Relief + Sample_Depth + Boring_Sponge + Sedimentation +
                        B.DO + 
                        B.Sal +  
                        Material_Age*Material +
                        (1|OS_ID/Year), 
                      family = gaussian, data=df, na.action=na.fail)

sim_res <- simulateResiduals(fittedModel = legal_sqrt2, plot = TRUE)

#including quad terms results in failing KS and adjusted quantile test; removing quad terms
# results in failing of those two and the outlier test (p=0.045)


#SPAT
spat_sqrt_FE_AIC <- FE_AIC("spat_sqrt", gaussian, df, fixed_sets, fixed_names)
print(spat_sqrt_FE_AIC)

#lowest AIC is with only interaction & no quadratic terms (AICc = 7616.6)
# spat_sqrt_int  7616.603 25  -- fails KS test & adj quantile test
# spat_sqrt_simp 7631.105 19
# spat_sqrt_quad       NA 21
# spat_sqrt_full       NA 27

#check residuals one by one for valid model to select as global before dredge()
spat_sqrt1<- glmmTMB(legal_sqrt ~ Material_Age + Material+ 
                        Relief + Sample_Depth + Boring_Sponge + Sedimentation +
                        B.DO+ I(B.DO^2) + 
                        B.Sal+ I(B.Sal^2) + 
                        Material_Age*Material +
                        (1|OS_ID/Year), 
                      family = gaussian, data=df, na.action=na.fail)

sim_res <- simulateResiduals(fittedModel = spat_sqrt1, plot = TRUE)

spat_sqrt2<- glmmTMB(spat_sqrt ~ Material_Age + Material+ Relief + 
                      Boring_Sponge + Sample_Depth + 
                       B.DO+ B.Sal+ 
                      Sedimentation + 
                      Material_Age*Material +
                     (1|OS_ID/Year),  
                    family = gaussian, data=df, na.action=na.fail)

#fails KS test, quatile test
sim_res <- simulateResiduals(fittedModel = spat_sqrt2, plot = TRUE)


# summary of residual diagnostics for fixed effects
# total --> include interaction, quadratics
# legal --> include quadratics & interaction
# spat --> include interaction, no quad terms

##########################################################
### RERUN ALL SUBSETS ANALYSIS & MUMMIn ON SQRT MODELS ### 
##########################################################

setwd("S:/8. Oyster Sanctuaries/3. Monitoring and Data/1. Oyster Sanctuary (OS)/5. Analysis/2007-2025/pub/results_reduced/01212026/02042026")


sqrt_total<- glmmTMB(total_sqrt ~ Material_Age + Material+ Relief +
                               Boring_Sponge + Sample_Depth +
                               B.DO + B.Sal +
                               #I(B.DO^2) + I(B.Sal^2) +
                               Sedimentation + Material_Age*Material +
                               (1|OS_ID/Year),
                             family = gaussian, data=df, na.action=na.fail)

sqrt_legal<- glmmTMB(legal_sqrt ~ Material_Age + Material+ Relief +
                       Boring_Sponge + Sample_Depth +
                       B.DO + B.Sal +
                       #I(B.DO^2) + I(B.Sal^2) +
                       Sedimentation +
                       Material_Age*Material +
                       (1|OS_ID/Year),
                     family = gaussian, data=df, na.action=na.fail)

sqrt_spat<- glmmTMB(spat_sqrt ~ Material_Age + Material+ Relief +
                      Boring_Sponge + Sample_Depth +
                      B.DO + B.Sal +
                      #I(B.DO^2) + I(B.Sal^2) +
                      Sedimentation + Material_Age*Material +
                      (1|OS_ID/Year),
                    family = gaussian, data=df, na.action=na.fail)

###################################
###TOTAL DENSITy MODEL SELECTION###
###################################

#Model selection with all subsets analysis
sqrt_total_AIC <- dredge(sqrt_total, rank = AICc)
sqrt_total_BIC <- dredge(sqrt_total, rank = BIC)

#warning message about cond((Int)) and disp((Int)) can be ignored

# Calculate weights and evidence ratios
weights <- sqrt_total_AIC$weight
best_weight <- max(weights)
evidence_ratios <- best_weight / weights

# Add evidence ratios column
sqrt_total_AIC$ER <- round(evidence_ratios, 2)

#save results into a csv file
write.csv(as.data.frame(sqrt_total_AIC), "sqrt_total_resultsA.csv")
write.csv(as.data.frame(sqrt_total_BIC), "sqrt_total_resultsB.csv")

#Subset models - exclude those that failed to converge & narrow down to models with ΔAICc < 6
sqrt_total_AIC <- subset(sqrt_total_AIC, !is.na(AICc))
top_models_total <- get.models(sqrt_total_AIC, subset = delta < 6)

#Model averaging over top models (ΔAICc < 6)
total_avg_model <- model.avg(top_models_total, fit = TRUE)

#Variable importance (model-averaged weights); importance() is deprecated, use sw()
vw_sqrt_total <- as.data.frame(sw(total_avg_model))
write.csv(vw_sqrt_total, "vw_sqrt_total.csv")

#Coefficients and confidence intervals
conf_ints_sqrt_total <- confint(total_avg_model)
coeffs_sqrt_total <- as.data.frame(coef(total_avg_model, full = TRUE))
coefficients_sqrt_total <- cbind(coeffs_sqrt_total, conf_ints_sqrt_total)
write.csv(coefficients_sqrt_total, "coeffs_sqrt_total.csv")

#save models to subfolder
saveRDS(total_avg_model, file = "total_avg_model.rds")

total_best_model <- get.models(sqrt_total_AIC, subset = 1)[[1]]
saveRDS(total_best_model, file = "total_best_model.rds")
sim_res <- simulateResiduals(fittedModel = total_best_model, plot = TRUE)

# best2_total_sqrt <- top_models_total[[2]]
# sim_res <- simulateResiduals(fittedModel = best2_total_sqrt, plot = TRUE)

#best3_total_sqrt <- top_models_total[[3]]
#sim_res <- simulateResiduals(fittedModel = best3_total_sqrt, plot = TRUE)
#top 3 models did not exhibit issues with the residuals - one test had a p=0.04 for the KS test...


###################################
###LEGAL DENSITy MODEL SELECTION###
###################################
#Model selection with all subsets analysis
sqrt_legal_AIC <- dredge(sqrt_legal, rank = AICc)
sqrt_legal_BIC <- dredge(sqrt_legal, rank = BIC)

#warning message about cond((Int)) and disp((Int)) can be ignored

# Calculate weights and evidence ratios
weights <- sqrt_legal_AIC$weight
best_weight <- max(weights)
evidence_ratios <- best_weight / weights

# Add evidence ratios column
sqrt_legal_AIC$ER <- round(evidence_ratios, 2)

#save results into a csv file
write.csv(as.data.frame(sqrt_legal_AIC), "sqrt_legal_resultsA.csv")
write.csv(as.data.frame(sqrt_legal_BIC), "sqrt_legal_resultsB.csv")

#Subset models - exclude those that failed to converge & narrow down to models with ΔAICc < 6
sqrt_legal_AIC <- subset(sqrt_legal_AIC, !is.na(AICc))
top_models_legal <- get.models(sqrt_legal_AIC, subset = delta < 6)

#Model averaging over top models (ΔAICc < 10)
legal_avg_model <- model.avg(top_models_legal, fit = TRUE)

#Variable importance (model-averaged weights); importance() is deprecated, use sw()
vw_sqrt_legal <- as.data.frame(sw(legal_avg_model))
write.csv(vw_sqrt_legal, "vw_sqrt_legal.csv")

#Coefficients and confidence intervals
conf_ints_sqrt_legal <- confint(legal_avg_model)
coeffs_sqrt_legal <- as.data.frame(coef(legal_avg_model, full = TRUE))
coefficients_sqrt_legal <- cbind(coeffs_sqrt_legal, conf_ints_sqrt_legal)
write.csv(coefficients_sqrt_legal, "coeffs_sqrt_legal.csv")

#save models
saveRDS(legal_avg_model, file = "legal_avg_model.rds")
legal_best_sqrt <- get.models(sqrt_legal_AIC, subset = 1)[[1]]
saveRDS(legal_best_sqrt, file = "legal_best_model.rds")

#check residuals of top performing models
best1_legal_sqrt <- top_models_legal[[1]]
sim_res <- simulateResiduals(fittedModel = best1_legal_sqrt, plot = TRUE)
#best2_legal_sqrt <- top_models_legal[[2]]
#sim_res <- simulateResiduals(fittedModel = best2_legal_sqrt, plot = TRUE)
#best3_legal_sqrt <- top_models_legal[[3]]
#sim_res <- simulateResiduals(fittedModel = best3_legal_sqrt, plot = TRUE)
#top three models have significant deviation with outlier test...


###################################
### SPAT DENSITY MODEL SELECTION###
###################################
#Model selection with all subsets analysis
sqrt_spat_AIC <- dredge(sqrt_spat, rank = AICc)
sqrt_spat_BIC <- dredge(sqrt_spat, rank = BIC)

#warning message about cond((Int)) and disp((Int)) can be ignored

#calculate evidence ratios
weights <- sqrt_spat_AIC$weight
best_weight <- max(weights)
evidence_ratios <- best_weight/weights

#add evidence ratios column to the dredge table
sqrt_spat_AIC$ER <- round(evidence_ratios, 2)

#save results into a csv file
sqrt_spat_resultsA <- as.data.frame(sqrt_spat_AIC)
write.csv(sqrt_spat_resultsA,"sqrt_spat_resultsA.csv")
sqrt_spat_resultsB <- as.data.frame(sqrt_spat_BIC)
write.csv(sqrt_spat_resultsB,"sqrt_spat_resultsB.csv")

#Subset models - exclude those that failed to converge & narrow down to models with ΔAICc < 6
sqrt_spat_AIC <- subset(sqrt_spat_AIC, !is.na(AICc))
top_models_spat <- get.models(sqrt_spat_AIC, subset = delta < 6)

# get an averaged model
spat_avg_model <- model.avg(top_models_spat, fit = TRUE)

mmin_sqrt_spat = model.avg(get.models(sqrt_spat_AIC, subset=TRUE))
#summary(mmin_sqrt_spat)

# get variable weights --> probability that variable is in the true best model among those considered
# importance(test) --> outdated, now it's sw(test)

vw_sqrt_spat <- as.data.frame(sw(spat_avg_model))
write.csv(vw_sqrt_spat,"vw_sqrt_spat.csv")

# don't use the conditional table for variable coefficients; ignore z + p values (don't report with AIC?)

conf_ints_sqrt_spat <- confint(spat_avg_model)
coeffs_sqrt_spat <- as.data.frame(coef(spat_avg_model, full = TRUE))

coefficients_sqrt_spat <- cbind(coeffs_sqrt_spat, conf_ints_sqrt_spat)
write.csv(coefficients_sqrt_spat, "coeffs_sqrt_spat.csv")


#save models to subfolder
saveRDS(spat_avg_model, file = "spat_avg_model.rds")
top_spat_sqrt <- get.models(sqrt_spat_AIC, subset = 1)[[1]]
saveRDS(top_spat_sqrt, file = "spat_best_model.rds")
#check residuals of top performing models
best1_spat_sqrt <- top_models_spat[[1]]
sim_res <- simulateResiduals(fittedModel = best1_spat_sqrt, plot = TRUE)
#best2_spat_sqrt <- top_models_spat[[2]]
#sim_res <- simulateResiduals(fittedModel = best2_spat_sqrt, plot = TRUE)
#best3_spat_sqrt <- top_models_spat[[3]]
#sim_res <- simulateResiduals(fittedModel = best3_spat_sqrt, plot = TRUE)
#for the most part the residuals look good! top three models on significant with the KS test