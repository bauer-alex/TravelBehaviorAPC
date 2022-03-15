
user <- "Alex"

library(APCtools)
library(dplyr)
library(ggplot2)
library(ggpubr)
theme_set(theme_minimal())

# setwd("2_paper/Bedeutungswandel/Code")
source("0_dataPrep.R")


# general prep ------------------------------------------------------------
# define age and period groups for the density matrices
age_groups    <- list(c(70,79),c(60,69),c(50,59),c(40,49),c(30,39),c(20,29),c(14,19))
period_groups <- list(c(1970,1979),c(1980,1989),c(1990,1999),c(2000,2009),c(2010,2018))



# participation plot ------------------------------------------------------
dat_P <- read_and_prepare_data(user = user, model = "participation")

# create the participation variable
dat_P <- dat_P %>% 
  mutate(participation = case_when(JS_Anzahl_URs == 0 ~ "0 trips",
                                   TRUE               ~ ">= 1 trips")) %>% 
  mutate(participation = factor(participation, levels = c("0 trips",">= 1 trips")))

# marginal distribution
(gg1 <- plot_variable(dat_P, "participation", legend_title = "Participation") +
  scale_fill_manual("Participation", values = c("lightblue","dodgerblue3")))
# alternative red colors: c("rosybrown2","indianred2"))
ggsave("../Figures/1_participation.png", width = 5, height = 3)

# density matrix
plot_densityMatrix(dat_P, y_var = "participation",
                   age_groups = age_groups, period_groups = period_groups)
ggsave("../Figures/1_participation_matrix.png", width = 6, height = 4)



# travel frequency plot ---------------------------------------------------
dat_F <- read_and_prepare_data(user = user, model = "frequency")

dat_F <- dat_F %>% 
  filter(JS_Anzahl_URs > 0) %>% 
  mutate(JS_Anzahl_URs_cat = case_when(JS_Anzahl_URs < 5 ~ as.character(JS_Anzahl_URs),
                                       TRUE              ~ "5+ trips")) %>% 
  mutate(JS_Anzahl_URs_cat = factor(JS_Anzahl_URs_cat, levels = c("5+ trips","4","3","2","1")))

# marginal distribution
green_colors <- RColorBrewer::brewer.pal(6, "Greens")[6:2]
(gg2 <- plot_variable(dat_F, "JS_Anzahl_URs_cat") +
  scale_fill_manual("Number of\ntrips", values = green_colors))
ggsave("../Figures/1_frequency.png", width = 5, height = 3)

# density matrix
plot_densityMatrix(dat_F, y_var = "JS_Anzahl_URs_cat",
                   age_groups = age_groups, period_groups = period_groups)
ggsave("../Figures/1_frequency_matrix.png", width = 6, height = 4)



# relative travel expenses plot -------------------------------------------
dat_E <- read_and_prepare_data(user = user, model = "expenses")

# first plot the household income
gg3 <- plot_variable(dat_E, y_var = "S_Einkommen_HH_equi", plot_type = "line-points",
                     ylim = c(0,1900), ylab = "median of household income [€]")

# plot the relative expenses
gg4 <- plot_variable(dat_E, y_var = "rel_expenses", plot_type = "line-points",
                     ylab = "median or rel. expenses [€]", ylim = c(0,1))


# joint plot --------------------------------------------------------------
ggpubr::ggarrange(gg1, gg2, gg3, gg4, ncol = 2, nrow = 2)
ggsave("../Figures/1_descriptiveMatrix.png", width = 10, height = 6)
