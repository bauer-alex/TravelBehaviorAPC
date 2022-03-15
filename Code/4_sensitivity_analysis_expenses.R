
user <- "Alex"

library(APCtools)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(mgcv)
theme_set(theme_minimal())

# setwd("2_paper/Bedeutungswandel/Code")
source("0_dataPrep.R")


# general prep ------------------------------------------------------------
# define age and period groups for the density matrices
age_groups    <- list(c(70,79),c(60,69),c(50,59),c(40,49),c(30,39),c(20,29),c(14,19))
period_groups <- list(c(1990,1999),c(2000,2009),c(2010,2018))

################################################################################

## Descriptive analysis:

# relative travel expenses plot -------------------------------------------
dat_E <- read_and_prepare_data(user = user, model = "expenses_sensitivity")

# first plot the household income
gg3 <- plot_variable(dat_E, y_var = "S_Einkommen_HH_equi", plot_type = "line-points",
                     ylim = c(0,1900), ylab = "median of household income")

# plot the relative expenses
gg4 <- plot_variable(dat_E, y_var = "rel_expenses", plot_type = "line-points",
                     ylab = "median or rel. expenses", ylim = c(0,1))

# joint plot --------------------------------------------------------------
ggpubr::ggarrange(gg3, gg4, ncol = 2, nrow = 1)
ggsave("../Figures/4_descriptiveMatrix_expenses_sensitivity.png", width = 7, height = 3.5)



################################################################################

# Model-based analysis:

# model estimation expenses -----------------------------------------------
model_E <- bam(formula = rel_expenses ~ te(period, age, bs = "ps", k = c(10, 10)) +
                 S_Geschlecht + S_Kinder_0_bis_5_binaer + S_Wohnortgroesse +
                 S_Bildung + S_Haushaltsgroesse + JS_HUR_Reisedauer +
                 s(S_Einkommen_HH_equi, bs = "ps", k = 10),
               family = Gamma(link = "log"),
               data = dat_E)


# data prep for the marginal effect plots ---------------------------------
plot_dat_list <- plot_marginalAPCeffects(model = model_E, dat = dat_E,
                                    variable = "age", return_plotData = TRUE)
plot_dat <- plot_dat_list[2:4] %>% dplyr::bind_rows() %>% 
  mutate(model = "Rel. Expenses") %>%
  mutate(variable = factor(variable, levels = c("Age","Period","Cohort")))



# marginal effect plots ---------------------------------------------------
vlines_cohort <- list("cohort" = c(1938.5,1946.5,1966.5,1982.5,1994.5))
cols          <- rev(scales::hue_pal()(3))

# vertical lines, only to be drawn for the cohort plots
vline_dat <- data.frame(variable = factor("Cohort", levels = c("Age","Period","Cohort")),
                        x        = c(1938.5,1946.5,1966.5,1982.5,1994.5))

plot_dat %>% 
    ggplot(aes(x = value, y = effect)) +
    geom_hline(yintercept = 1, lty = 2, col = gray(0.3)) +
    geom_vline(data = vline_dat, aes(xintercept = x), lty = 2, col = gray(0.3)) +
    geom_line(col = cols[3]) +
    facet_grid(model ~ as.factor(variable), scales = "free") +
    scale_y_continuous("exp(Effect)", breaks = c(0.9,1,1.1), trans = "log2") +
    theme(strip.background = element_rect(fill = gray(0.8)),
          legend.position  = "none",
          axis.title.x     = element_blank())
ggsave("../Figures/4_marginalEffects_expenses.png", width = 6, height = 2)

create_APCsummary(list(model_E), dat = dat_E,
                  apc_range = list("cohort" = 1939:2018))



# linear covariate effects ------------------------------------------------
plot_dat_linear <- plot_linearEffects(model_E, return_plotData = TRUE) %>% 
  mutate(vargroup = as.character(vargroup)) %>% 
  mutate(vargroup = case_when(vargroup == "S_Geschlecht"            ~ "gender",
                              vargroup == "S_Bildung"               ~ "education",
                              vargroup == "S_Kinder_0_bis_5_binaer" ~ "young children",
                              vargroup == "S_Haushaltsgroesse"      ~ "household size",
                              vargroup == "JS_HUR_Reisedauer"       ~ "trip length",
                              vargroup == "S_Wohnortgroesse"        ~ "city size",
                              TRUE ~ vargroup)) %>% 
  mutate(vargroup = factor(vargroup, levels = c("gender","education","household size",
                                                "young children","city size","trip length"))) %>% 
  mutate(param = as.character(param)) %>% 
  mutate(param = case_when(param == "weiblich"                  ~ "female",
                           grepl("Mittlere Reife", param)       ~ "secondary school",
                           grepl("Abitur",         param)       ~ "high school",
                           grepl("Universitaet",   param)       ~ "university or college",
                           param == "Kinder dieser Altersstufe" ~ "yes",
                           param == "5.3"                       ~ ">=5",
                           param == "5.000 bis 49.999"          ~ "[5,000; 50,000)",
                           param == "50.000 bis 99.999"         ~ "[50,000; 100,000)",
                           param == "100.000 bis 499.999"       ~ "[100,000; 500,000)",
                           param == "500.000 und mehr"          ~ ">=500,000",
                           param == "6 bis 8 Tage"              ~ "6-8 days",
                           param == "9 bis 12 Tage"             ~ "9-12 days",
                           param == "13 bis 15 Tage"            ~ "13-15 days",
                           param == "16 bis 19 Tage"            ~ "16-19 days",
                           param == "20 bis 22 Tage"            ~ "20-22 days",
                           param == "23 bis 26 Tage"            ~ "23-26 days",
                           param == "27 bis 29 Tage"            ~ "27-29 days",
                           param == "30 Tage und mehr"          ~ ">=30 days",
                           TRUE ~ param)) %>% 
  mutate(param = factor(param, levels = c("female","secondary school","high school","university or college",
                                          "2","3","4",">=5","yes",
                                          "[5,000; 50,000)","[50,000; 100,000)",
                                          "[100,000; 500,000)",">=500,000",
                                          "6-8 days","9-12 days","13-15 days",
                                          "16-19 days","20-22 days","23-26 days",
                                          "27-29 days",">=30 days")))

ggplot(plot_dat_linear, mapping = aes(x = param, y = coef)) +
  geom_hline(yintercept = 1, col = gray(0.3), lty = 2) +
  geom_pointrange(mapping = aes(ymin = CI_lower, ymax = CI_upper, col = vargroup), size = 1) +
  geom_point(mapping = aes(col = vargroup), size = 1) +
  scale_y_continuous(trans = "log2", name = "exp(Effect)") +
  colorspace::scale_colour_discrete_qualitative(palette = "Dark 3") +
  facet_grid( ~ vargroup, scales = "free_x") +
  theme(legend.position = "none",
        axis.title.x    = element_blank(),
        axis.text.x     = element_text(angle = 45, hjust = 1),
        strip.background = element_rect(fill = gray(0.8)))
ggsave("../Figures/4_linearEffects_expenses.png", width = 8, height = 5)



# nonlinear income effect -------------------------------------------------
plot_1Dsmooth(model_E, select = 2, plot_ci = TRUE, ylim = c(0.25,128)) +
  scale_x_continuous(limits = c(0, 6000), name = "Household income [€]") +
  scale_y_continuous(trans = "log2", name = "exp(Effect)",
                     breaks = 2^c(-2,1,4,7), labels = c("0.25","2","16","128")) +
  scale_color_manual(values = cols[3])
ggsave("../Figures/4_incomeEffects_expenses.png", width = 6, height = 2.5)
