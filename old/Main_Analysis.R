# Working directory und Packages:
user <- "Max"
if(user == "Elisabeth") {
  setwd("C:/Users/di67mav/LRZ Sync+Share/TourIST (Alexander Bauer)/Daten/Daten_aufbereitet")
}
if(user == "Max") {
  setwd("C:/Users/ri87nix/LRZ Sync+Share/TourIST_neu/TourIST_2021-03-07T0005/Daten/Daten_aufbereitet")
}
if (user == "Juliana") {
  setwd("~/LRZ Sync+Share/TourIST_neu (Alexander Bauer)/TourIST_2021-03-07T0005/Daten/Daten_aufbereitet")
}
library(tidyverse)
library(mgcv)
library(RColorBrewer)
library(purrr)
library(data.table)
library(ggpubr)

# Read data:
dat <- readRDS("210527_dat_ftf.rds")

if(user == "Max") {
  setwd("C:/Users/ri87nix/Documents/Git/TourIST/2_paper/Bedeutungswandel")
}
if(user == "Juliana") {
  setwd("~/Arbeit/tourist/2_paper/Bedeutungswandel")
}
source("Functions.R")

# Preprocessing:

# Anteil Ausgaben am Haushaltseinkommen:
dat_einkommen <- data.table(dat)
dat_einkommen <- dat_einkommen[!is.na(JS_HUR_Ausgaben_AnteilEinkHH)][!is.na(JS_HUR_Ausgaben_gesamt)][JS_HUR_Ausgaben_AnteilEinkHH != -99]
#dat_einkommen[, JS_HUR_Ausgaben_AnteilEinkHH := JS_HUR_Ausgaben_AnteilEinkHH / 12]

# Anteil Ausgaben aller Reisen am Haushaltseinkommen (aufaddiert)
#dat_einkommen[,JS_Gesamt_Ausgaben_AnteilEinkHH := sum(JS_HUR_Ausgaben_AnteilEinkHH[which(JS_HUR_Ausgaben_AnteilEinkHH > 0)],
#                                                      JS_UR2_Ausgaben_AnteilEinkHH[which(JS_UR2_Ausgaben_AnteilEinkHH > 0)],
#                                                      JS_UR3_Ausgaben_AnteilEinkHH[which(JS_UR3_Ausgaben_AnteilEinkHH > 0)]),
#              by = id]
dat_einkommen$JS_Gesamt_Ausgaben_AnteilEinkHH <- dat_einkommen$JS_HUR_Ausgaben_AnteilEinkHH
dat_einkommen[, JS_Gesamt_Ausgaben_AnteilEinkHH := JS_Gesamt_Ausgaben_AnteilEinkHH / 12]
dat_einkommen <- data_preparation(dat_einkommen, target = "JS_Gesamt_Ausgaben_AnteilEinkHH")
dat_einkommen <- preprocessing_model(dat_einkommen, target = "JS_Gesamt_Ausgaben_AnteilEinkHH")

# Einschränken der Reisejahre:
dat_einkommen <- dat_einkommen %>% filter(period > 1980) #%>%
  #filter(is.na(S_Herkunft) | S_Herkunft == "West")
  
# Berechnung Äquivalenzeinkommen:



# Pure --------------------------------------------------------------------
# Modellrechnung:
pure_model <- model_pure(data = dat_einkommen,
                         target = "JS_Gesamt_Ausgaben_AnteilEinkHH",
                         method = "bam")

# Heatmap:
plot_heatmap(model = pure_model, data = dat_einkommen, type = "pure")


# APC plots:
partial_APC_plots(model = pure_model, data = dat_einkommen,
                  variable = "period", title = TRUE)

partial_APC_plots(model = pure_model, data = dat_einkommen,
                  variable = "age", title = TRUE)

partial_APC_plots(model = pure_model, data = dat_einkommen,
                  variable = "cohort", title = TRUE)


# Covariates --------------------------------------------------------------
# Modellrechnung:
covariate_model <- model_covariate(data = dat_einkommen,
                                   target = "JS_Gesamt_Ausgaben_AnteilEinkHH",
                                   method = "bam",
                                   variables_num = c("S_Einkommen_HH"),
                                   variables_cat = c("JS_HUR_Reisedauer",
                                                     "S_Haushaltsgroesse"))

# Heatmap:
plot_heatmap(model = covariate_model, data = dat_einkommen, type = "covariate",
             variables_num = c("S_Einkommen_HH"),
             variables_cat = c("JS_HUR_Reisedauer", "S_Haushaltsgroesse"))

# APC plots:
partial_APC_plots(model = model_covariate, data = dat_einkommen,
                  type = "covariate", variable = "period", title = TRUE,
                  variables_num = c("S_Einkommen_HH"),
                  variables_cat = c("JS_HUR_Reisedauer", "S_Haushaltsgroesse"))

partial_APC_plots(model = model_covariate, data = dat_einkommen, type = "covariate",
                  variable = "age", title = TRUE,
                  variables_num = c("S_Einkommen_HH"),
                  variables_cat = c("JS_HUR_Reisedauer", "S_Haushaltsgroesse"))

partial_APC_plots(model = model_covariate, data = dat_einkommen, type = "covariate",
                  variable = "cohort", title = TRUE,
                  variables_num = c("S_Einkommen_HH"),
                  variables_cat = c("JS_HUR_Reisedauer", "S_Haushaltsgroesse"))


# Covariates plots
plot_covariates(model = model_covariate, data = dat_einkommen,
                variables_num = c("S_Einkommen_HH"),
                variables_cat = c("JS_HUR_Reisedauer", "S_Haushaltsgroesse"))


# Comparison of marginal effects between pure and covariate APC models:
compare_pure_covariate(model_pure = model_pure,
                       model_covariate = model_covariate,
                       data = dat_einkommen,
                       variables_num = c("S_Einkommen_HH"),
                       variables_cat = c("JS_HUR_Reisedauer",
                                         "S_Haushaltsgroesse"))

























