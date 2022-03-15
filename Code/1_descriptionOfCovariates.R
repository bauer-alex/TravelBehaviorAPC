
user <- "Alex"

library(APCtools)
library(dplyr)
library(ggplot2)
library(ggpubr)
theme_set(theme_minimal())

# setwd("2_paper/Bedeutungswandel/Code")
source("0_dataPrep.R")



# data prep ---------------------------------------------------------------
dat <- read_and_prepare_data(user = user, model = "participation")

dat <- dat %>% 
  dplyr::rename(Gender                 = S_Geschlecht,
                Household_net_income   = S_Einkommen_HH,
                Education_level        = S_Bildung,
                Household_size         = S_Haushaltsgroesse,
                Children_under_5_years = S_Kinder_0_bis_5_binaer,
                Size_of_residence      = S_Wohnortgroesse,
                Duration_of_main_trip  = JS_HUR_Reisedauer)

# categorize the household net income
dat <- dat %>% 
  mutate(Household_net_income_cat = case_when(Household_net_income < 1000 ~ "[0,1000)",
                                              Household_net_income < 2000 ~ "[1000,2000)",
                                              Household_net_income < 3000 ~ "[2000,3000)",
                                              Household_net_income < 4000 ~ "[3000,4000)",
                                              Household_net_income < 5000 ~ "[4000,5000)",
                                              Household_net_income < 6000 ~ "[5000,6000)",
                                              TRUE                        ~ ">= 6000"))



# descriptions ------------------------------------------------------------
vars <- c("Gender","Household_net_income_cat","Education_level","Household_size",
          "Children_under_5_years","Size_of_residence","Duration_of_main_trip")

# Note: Among the above variables only the trip duration has some NA values,
#       and these are only in the data for non-travelers.´
#       Accordingly, we don't have to bother with NA values in the following
#       table.

# create the frequency table with separate information for all travelers
freq_dat_list <- lapply(vars, function(var) {
  
  x_all       <- dat[[var]]
  x_travelers <- dat[[var]][dat$y_atLeastOneUR == 1]
  
  tab_abs_all       <- table(x_all)
  tab_abs_travelers <- table(x_travelers)
  tab_rel_all       <- prop.table(tab_abs_all)
  tab_rel_travelers <- prop.table(tab_abs_travelers)
  
  freq_dat <- data.frame(Variable       = var,
                         Value          = names(tab_abs_all),
                         n_overall      = as.vector(tab_abs_all),
                         n_travelers    = as.vector(tab_abs_travelers),
                         freq_overall   = paste0(round(100 * as.vector(tab_rel_all),       1), "%"),
                         freq_travelers = paste0(round(100 * as.vector(tab_rel_travelers), 1), "%"))
  
  # only keep the information on the trip duration for travelers
  if (var == "Duration_of_main_trip") {
    freq_dat$n_overall <- freq_dat$freq_overall <- NA
  }
  
  return(freq_dat)
})

freq_dat <- dplyr::bind_rows(freq_dat_list)

write.csv(freq_dat, file = "../Figures/1_freqTable_covariates.csv", row.names = FALSE)



# table of generations ----------------------------------------------------
gen_table <- dat %>% 
  group_by(generation) %>% 
  summarize(birth_years   = paste0(min(cohort)," - ",max(cohort)),
            rel_frequency = paste0(round(100 * n() / nrow(dat), 1), "%"),
            obs_periods   = paste0(min(period)," - ",max(period)),
            obs_ages      = paste0(min(age)," - ",max(age)))

write.csv(gen_table, file = "../Figures/1_table_generations.csv", row.names = FALSE)
