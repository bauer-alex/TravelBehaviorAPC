# Working directory und Packages:
user <- "Max"
if(user == "Elisabeth") {
  setwd("C:/Users/di67mav/LRZ Sync+Share/TourIST (Alexander Bauer)/Daten/Daten_aufbereitet")
}
if(user == "Max") {
  setwd("C:/Users/ri87nix/LRZ Sync+Share/TourIST_neu/TourIST_2021-03-07T0005/Daten/Daten_aufbereitet")
}
library(tidyverse)
library(mgcv)

# Read data:
dat <- readRDS("210527_dat_ftf.rds")

if(user == "Max") {
  setwd("C:/Users/ri87nix/Documents/Git/TourIST/2_paper/Bedeutungswandel")
}
source("Functions.R")

# Preprocessing:


# Anteil Ausgaben am Haushaltseinkommen:
dat_model <- dat %>%
  filter(!is.na(JS_HUR_Ausgaben_AnteilEinkHH)) %>%
  filter(JS_HUR_Ausgaben_AnteilEinkHH != -99) %>%
  filter(travel_year >= 1981) %>%
  mutate(JS_HUR_Ausgaben_AnteilEinkHH = JS_HUR_Ausgaben_AnteilEinkHH / 12,
         cohort = travel_year - S_Alter) %>%
  dplyr::select(travel_year, S_Alter, cohort, JS_HUR_Ausgaben_AnteilEinkHH,
                JS_HUR_Ausgaben_gesamt, JS_HUR_Ausgaben_pP,
                JS_HUR_Reisebegleitung_Gesamtanzahl, JS_HUR_Reisedistanz, JS_HUR_Reisedauer)
summary(dat_model$JS_HUR_Ausgaben_AnteilEinkHH)

# Modellrechnung:
model <- bam(formula = #JS_HUR_Ausgaben_AnteilEinkHH ~
               JS_HUR_Ausgaben_pP ~
               te(S_Alter, travel_year, bs = "ps") + JS_HUR_Reisedauer + JS_HUR_Reisedistanz +
               JS_HUR_Reisebegleitung_Gesamtanzahl,
             family = Gamma(link = "log"),
             data = dat_model)
summary(model)
plot(model)

dat_model$fitted <- model$fitted.values
dat_model$resids <- residuals(model)
ggplot(data = dat_model, mapping = aes(x = fitted, y = resids)) +
  geom_point()


# Personen ohne Ausgaben:
dat_ohne_ausgaben <- dat_einkommen %>%
  filter(JS_HUR_Ausgaben_AnteilEinkHH == 0)
table(dat_ohne_ausgaben$travel_year)





















