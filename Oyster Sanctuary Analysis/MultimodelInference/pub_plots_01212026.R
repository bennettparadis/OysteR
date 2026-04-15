setwd("S:/8. Oyster Sanctuaries/3. Monitoring and Data/1. Oyster Sanctuary (OS)/5. Analysis/2007-2025/pub/results_reduced/01212026")
library(dplyr)
library(grid)
library(gridExtra)
library(ggplot2)
library(cowplot)
library(scales)
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
library(ggpubr)

# #update R without IT help!
# library(installr)
# updateR()

#############################################
###  SHAPE DATAFRAME TO BE USED IN MODELS ###
#############################################
data <- read.csv("RF_predictions_reduced_10202025.csv")

df <- data%>%
  select(SID, Year, Month, OS_ID, Season, Material, Material_Age, Sedimentation, Boring_Sponge, Sample_Depth, Relief, B.DO, B.Sal, total, legal, spat)%>%
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

#more exploratory figures

sed_frequency <- table(df$Sedimentation)
print(sed_frequency)

ggplot(data =df, aes(x=Material_Age, y = Sedimentation)) + geom_hex()
ggplot(data =df, aes(x=Material_Age, y = Sedimentation)) + geom_point(alpha=0.3)

ggplot(data =df, aes(x=Month, y = spat)) + geom_point(alpha=0.3)
ggplot(data =df, aes(x=Month, y = legal)) + geom_point(alpha=0.3)

ggplot(df, aes(x=Material, y=spat, color = Boring_Sponge)) + geom_boxplot()

ggplot(df, aes(x=Sedimentation, y=Relief)) + geom_boxplot()


mat_freq <- table(df$Material)
print(mat_freq)
########## Y-TRANSFORMATIONS #############

response_vars <- c("total", "legal", "spat")

for (y in response_vars) {
  df[[paste0(y, "_log")]] <- log(df[[y]]+1)
  df[[paste0(y, "_log10")]] <- log10(df[[y]]+1)
  df[[paste0(y, "_sqrt")]] <- sqrt(df[[y]])
}

######### IMPORT MODELS FROM ANALYSIS ###########
setwd("S:/8. Oyster Sanctuaries/3. Monitoring and Data/1. Oyster Sanctuary (OS)/5. Analysis/2007-2025/pub/results_reduced/01212026/02042026")

total_avg_model <- readRDS("total_avg_model.rds")
legal_avg_model <- readRDS("legal_avg_model.rds")
spat_avg_model <- readRDS("spat_avg_model.rds")

total_best_model <- readRDS("total_best_model.rds")
legal_best_model <- readRDS("legal_best_model.rds")
spat_best_model <- readRDS("spat_best_model.rds")


###################################
####### BOX PLOT FUNCTION #########
###################################

#theme for box plots
bp_theme <-   theme(plot.margin = margin(t=5, r=5, b=5, l=20),
                    axis.title.x = element_blank(),
                    axis.title.y = element_text(size = 10, margin = margin(r = 5)),  # Add right margin to y-axis label
                    axis.text.x = element_text(size = 8, margin = margin(t = 5)),    # Add top margin to x-axis text
                    axis.text.y = element_text(size = 8, margin = margin(r = 5)),
                    plot.title = element_text(size = 8, hjust = 0.5),
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    plot.background = element_rect(fill = "white"),
                    panel.background = element_rect(fill = "white"),
                    axis.line = element_line(color = "black"))

#labels for y-axes, and x-axes
label_t = expression("Total Oysters (m"^-2*")")
label_l = expression("Market-sized Oysters (m"^-2*")")
label_r = expression("Recruit Oysters (m"^-2*")")

label_mat = "Material Type"
label_sed = "Sedimentation"


#box plot function
make_boxplot <- function(data, yvar, xvar, ylabel) {                 
  ggplot(data, aes(x = .data[[xvar]], y = .data[[yvar]])) +
    stat_boxplot(geom = "errorbar", width = 0.25) +
    geom_boxplot(
      fill = "#56B4E9", color = "black", alpha = 0.5,
      outlier.color = "tomato1", outlier.size = 2
    ) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
    labs(y = ylabel) +
    #stat_compare_means(method = "kruskal.test", label.x = 5, label.y = max(data[[yvar]], na.rm = TRUE) * 0.9) +
    #stat_compare_means(label = "p.signif", method = "t.test", ref.group = ref_level)+
    bp_theme
}

#MATERIAL TYPE VS DENSITY#
total_mattype <- make_boxplot(df, "total", "Material", label_t)
#total_mattype
legal_mattype <- make_boxplot(df, "legal", "Material", label_l)
#legal_mattype
spat_mattype <- make_boxplot(df, "spat", "Material", label_r)
#spat_mattype

mattype_plots <- plot_grid(
  total_mattype, legal_mattype, spat_mattype,
  nrow = 3,
  labels = "AUTO",
  rel_widths = c(0.7, 0.7, 0.7)
)

final_mattype <- ggdraw() +
  draw_plot(mattype_plots, x = 0, y = 0.05, width = 1, height = 0.95) +  # Leave space for x-axis label
  draw_label("Material Type", x = 0.5, y = 0.01, vjust = 0, size = 14, fontface = "bold")  # Centered below

final_mattype


#SEDIMENTATION LVL VS DENSITY
total_sed <- make_boxplot(df, "total", "Sedimentation", label_t)#, "None")
#total_sed

legal_sed <- make_boxplot(df, "legal", "Sedimentation", label_l)#, "None")
#legal_sed

#RECRUIT DENSITY VS SEDIMENTATION LVL
spat_sed <- make_boxplot(df, "spat", "Sedimentation", label_r)#, "None")
#spat_sed

sed_plots <- plot_grid(
  total_sed, legal_sed, spat_sed,
  nrow = 3,
  labels = "AUTO",
  rel_widths = c(0.7, 0.7, 0.7)
)

final_sed <- ggdraw() +
  draw_plot(sed_plots, x = 0, y = 0.05, width = 1, height = 0.95) +  # Leave space for x-axis label
  draw_label("Sedimentation Level", x = 0.5, y = 0.01, vjust = 0, size = 14, fontface = "bold")  # Centered below

final_sed



#############################################################
############ SCATTER PLOTS FOR CONTINUOUS VARIABLES #########
#############################################################


library(ggplot2)
library(patchwork)
library(dplyr)

# Theme
sp_theme <- theme(
  plot.margin = margin(t = 5, r = 5, b = 5, l = 5),
  axis.title.x = element_blank(),
  axis.title.y = element_text(size = 10, margin = margin(r = 5)),
  axis.text.x = element_text(size = 10, margin = margin(t = 5)),
  axis.text.y = element_text(size = 10, margin = margin(r = 5)),
  legend.text = element_text(size = 8),
  legend.title = element_text(size = 8),
  legend.title.align = 0.5,
  legend.background = element_rect(fill = "white", color = "black"),
  legend.box.background = element_rect(fill = "white", color = "black"),
  plot.title = element_text(size = 8, hjust = 0.5),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  plot.background = element_rect(fill = "white"),
  panel.background = element_rect(fill = "white"),
  axis.line = element_line(color = "black")
)

# Scatterplot maker
make_scatter <- function(data, yvar, xvar, ylabel, xlabel, newdata, predx, predy, lower, upper,
                         show_xlab = TRUE, show_ylab = TRUE) {
  ggplot(data, aes(x = .data[[xvar]], y = .data[[yvar]])) +
    geom_point(size = 2, alpha = 0.5) +
    geom_line(data = newdata, aes(x = .data[[predx]], y = .data[[predy]]), color = 'red') +
    geom_ribbon(
      data = newdata,
      aes(x = .data[[predx]], ymin = .data[[lower]], ymax = .data[[upper]]),
      alpha = 0.2,
      color = NA,
      inherit.aes = FALSE
    ) +
    labs(x = if (show_xlab) xlabel else NULL,
         y = if (show_ylab) ylabel else NULL) +
    guides(color = "none", fill = "none") +
    sp_theme +
    theme(
      axis.text.x = if (show_xlab) element_text(size = 10, margin = margin(t = 5)) else element_blank(),
      axis.title.x = if (show_xlab) element_text(size = 10) else element_blank(),
      axis.text.y = if (show_ylab) element_text(size = 10, margin = margin(r = 5)) else element_blank(),
      axis.title.y = if (show_ylab) element_text(size = 10, margin = margin(r = 5)) else element_blank()
    )
}

# Function to create newdata and predictions
make_predictions <- function(model, df, xvar, transform_back = TRUE) {
  
  safe_mean <- function(x) mean(x, na.rm = TRUE)
  
  # fixed effect terms
  terms <- attr(terms(model), "term.labels")
  terms <- terms[!grepl("\\|", terms)]
  main_terms <- unique(unlist(strsplit(terms, ":")))
  main_terms <- gsub("I\\(|\\^2\\)", "", main_terms)
  
  grid <- list()
  
  for (v in main_terms) {
    if (v == xvar) next
    
    if (v %in% names(df)) {
      if (is.numeric(df[[v]])) {
        grid[[v]] <- safe_mean(df[[v]])
      } else {
        grid[[v]] <- factor(levels(df[[v]])[1], levels = levels(df[[v]]))
      }
    }
  }
  
  # random effects placeholders
  grid$OS_ID <- df$OS_ID[1]
  grid$Year  <- df$Year[1]
  
  grid <- as.data.frame(grid)
  
  # ---- HANDLE NUMERIC VS FACTOR XVAR ----
  if (is.numeric(df[[xvar]])) {
    
    grid <- grid[rep(1, 100), ]
    grid[[xvar]] <- seq(min(df[[xvar]], na.rm = TRUE),
                        max(df[[xvar]], na.rm = TRUE),
                        length.out = 100)
    
  } else {
    levs <- levels(df[[xvar]])
    grid <- grid[rep(1, length(levs)), ]
    grid[[xvar]] <- factor(levs, levels = levs)
  }
  
  preds <- predict(
    model,
    newdata = grid,
    type = "response",
    se.fit = TRUE,
    re.form = NA,
    allow.new.levels = TRUE
  )
  
  if (transform_back) {
    grid$fit <- preds$fit^2
    grid$se  <- 2 * preds$fit * preds$se.fit
  } else {
    grid$fit <- preds$fit
    grid$se  <- preds$se.fit
  }
  
  grid$lower <- pmax(0, grid$fit - 1.96 * grid$se)
  grid$upper <- grid$fit + 1.96 * grid$se
  
  return(grid)
}


# Labels
ylabels <- c(
  total = expression("Total (m"^-2*")"),
  legal = expression("Market-sized (m"^-2*")"),
  spat  = expression("Recruit (m"^-2*")")
)

xlabels <- c(
  Relief        = "Relief (m)",
  Sample_Depth  = "Sample Depth (m)",
  B.DO          = "Dissolved Oxygen (mg/L)",
  B.Sal         = "Salinity (psu)",
  Boring_Sponge = "Boring Sponge"
)

# Models
models <- list(
  total = total_best_model,
  legal = legal_best_model,
  spat  = spat_best_model
)

xvars <- names(xlabels)

# Generate all plots with label suppression
plot_matrix <- list()

for (row_i in seq_along(models)) {
  yvar <- names(models)[row_i]
  for (col_i in seq_along(xvars)) {
    xvar <- xvars[col_i]
    newdat <- make_predictions(models[[yvar]], df, xvar)
    p <- make_scatter(
      data = df,
      yvar = yvar,
      xvar = xvar,
      ylabel = ylabels[[yvar]],
      xlabel = xlabels[[xvar]],
      newdata = newdat,
      predx = xvar,
      predy = "fit",
      lower = "lower",
      upper = "upper",
      show_xlab = (row_i == length(models)),  # only bottom row
      show_ylab = (col_i == 1)               # only first col
    )
    plot_matrix[[paste(yvar, xvar, sep = "_")]] <- p
  }
}

# Arrange plots
final_plot <- wrap_plots(
  lapply(seq_along(models), function(row_i) {
    wrap_plots(lapply(seq_along(xvars), function(col_i) {
      plot_matrix[[paste(names(models)[row_i], xvars[col_i], sep = "_")]]
    }), ncol = length(xvars))
  }), ncol = 1
) +
  plot_annotation(theme = theme(plot.title = element_text(size = 16, hjust = 0.5)))

print(final_plot)



####MATERIAL AGE VISUALIZATION#####
plot_material_age <- function(df, response_var, best_model, y_label, transform = "sqrt") {
  
  # Get observed range of ages per material
  age_ranges <- df %>%
    group_by(Material) %>%
    summarize(min_age = min(Material_Age),
              max_age = max(Material_Age),
              .groups = "drop")
  
  # Build new data for predictions
  newdat <- age_ranges %>%
    rowwise() %>%
    do({
      data.frame(
        Material = factor(.$Material, levels = levels(df$Material)),
        Material_Age = seq(.$min_age, .$max_age, length.out = 100),
        Relief = mean(df$Relief),
        B.DO = mean(df$B.DO),
        Sample_Depth = if("Sample_Depth" %in% names(df)) mean(df$Sample_Depth) else NA,
        Sedimentation = if("Sedimentation" %in% names(df)) factor(levels(df$Sedimentation)[1], levels = levels(df$Sedimentation)) else NA,
        Boring_Sponge = factor(levels(df$Boring_Sponge)[1], levels = levels(df$Boring_Sponge))  # default to first level
      )
    }) %>%
    ungroup() %>%
    bind_rows()
  
  # Predict with glmmTMB
  preds <- predict(
    best_model,
    newdata = newdat,
    type = ifelse(transform == "link", "link", "response"),
    se.fit = TRUE,
    re.form = NA,
    allow.new.levels = TRUE
  )
  
  newdat$pred_fit <- preds$fit
  newdat$se <- preds$se.fit
  
  # Back-transform predictions
  if(transform == "sqrt"){
    newdat <- newdat %>%
      mutate(
        pred_original = pred_fit^2,
        lower = (pred_fit - 1.96 * se)^2,
        upper = (pred_fit + 1.96 * se)^2
      )
  } else if(transform == "link"){  # e.g., log-link for spat
    newdat <- newdat %>%
      mutate(
        pred_original = (pred_fit)^2,
        lower = (pred_fit - 1.96*se)^2,
        upper = (pred_fit + 1.96*se)^2
      )
  }
  
  # ggplot
  p <- ggplot(df, aes(x = Material_Age, y = !!sym(response_var), color = Material)) +
    geom_point(size = 2, alpha = 0.5) +
    geom_line(data = newdat, aes(x = Material_Age, y = pred_original, group = Material), color = "black") +
    geom_ribbon(data = newdat,
                aes(x = Material_Age, ymin = lower, ymax = upper, fill = Material, group = Material),
                alpha = 0.2,
                color = NA,
                inherit.aes = FALSE) +
    facet_wrap(~Material, scales = "free", ncol=4) +
    scale_x_continuous(breaks = function(x) {
      rng <- ceiling(x[2]) - floor(x[1])
      by_val <- dplyr::case_when(
        rng <= 8 ~ 1,
        rng <= 10 ~ 2,
        rng <= 15 ~ 3,
        TRUE      ~ 6
      )
      seq(floor(x[1]), ceiling(x[2]), by = by_val)
    }) +
    labs(
      x = "Material Age (years)",
      y = y_label
    ) +
    guides(color = "none", fill = "none") +
    theme(
      axis.title.x = element_text(size = 16, margin = margin(t = 20)),
      axis.title.y = element_text(size = 16, margin = margin(r = 20)),
      axis.text.x = element_text(size = 12, margin = margin(t = 10)),
      axis.text.y = element_text(size = 12, margin = margin(r = 10)),
      plot.title = element_text(size = 20, hjust = 0.5),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "white"),
      panel.background = element_rect(fill = "white"),
      axis.line = element_line(color = "black")
    )
  
  return(p)
}

#----------------------------------------
# Example use for your glmmTMB models
#----------------------------------------
legal_plot <- plot_material_age(df, "legal", legal_best_model, expression("Market-sized Density (m"^-2*")"), transform = "sqrt")
total_plot <- plot_material_age(df, "total", total_best_model, expression("Total Oyster Density (m"^-2*")"), transform = "sqrt")
spat_plot  <- plot_material_age(df, "spat", spat_best_model, expression("Recruit Density (m"^-2*")"), transform = "link")

print(legal_plot)
print(total_plot)
print(spat_plot)









