
user <- "Alex"


library(APCtools) # routines for APC analysis
library(mgcv)     # estimation of GAM models
library(dplyr)    # general data preparation
library(ggplot2)  # data visualization
library(scales)   # for getting ggplot2 colors with hue_pal()
library(ggpubr)   # arrange ggplots in a grid with ggarrange()
library(pROC)     # computation of area under the curve

theme_set(theme_minimal())

# setwd("2_paper/Bedeutungswandel/Code")
source("0_dataPrep.R")


# model estimation participation ------------------------------------------
dat_P <- read_and_prepare_data(user = user, model = "participation")

model_P <- bam(formula = y_atLeastOneUR ~ te(period, age, k = c(10, 10), bs = "ps") +
                 S_Geschlecht + S_Kinder_0_bis_5_binaer + S_Wohnortgroesse +
                 S_Bildung + s(S_Einkommen_HH_equi, bs = "ps", k =10) +
                 S_Haushaltsgroesse,
               family = binomial(link = "logit"), data = dat_P)



# model estimation frequency ----------------------------------------------
dat_F <- read_and_prepare_data(user = user, model = "frequency")

model_F <- bam(y_atLeastTwoURs ~ te(period, age, k = c(10, 10), bs = "ps") +
                 S_Geschlecht + S_Kinder_0_bis_5_binaer + S_Wohnortgroesse + S_Bildung +
                 s(S_Einkommen_HH_equi, bs = "ps", k = 10) + S_Haushaltsgroesse,
               family = binomial(link = "logit"), data = dat_F)



# model estimation expenses -----------------------------------------------
dat_E <- read_and_prepare_data(user = user, model = "expenses")

model_E <- bam(formula = rel_expenses ~ te(period, age, bs = "ps", k = c(10, 10)) +
                 S_Geschlecht + S_Kinder_0_bis_5_binaer + S_Wohnortgroesse +
                 S_Bildung + S_Haushaltsgroesse + JS_HUR_Reisedauer +
                 s(S_Einkommen_HH_equi, bs = "ps", k = 10),
               family = Gamma(link = "log"),
               data = dat_E)




# general preparations for all following plots ----------------------------
model_suffices <- c("P","F","E")
model_labels   <- c("Participation","Frequency","Rel. Expenses")



# data prep for the marginal effect plots ---------------------------------
plot_dat_list <- lapply(model_suffices, function(suffix) {
  
  # return a list for all age, period and cohort
  model_dat_list <- plot_marginalAPCeffects(model           = get(paste0("model_",suffix)),
                                            dat             = get(paste0("dat_",suffix)),
                                            variable        = "age",
                                            return_plotData = TRUE)
  
  model_dat <- model_dat_list[2:4] %>% dplyr::bind_rows() %>% 
    mutate(model = suffix)
  
  return(model_dat)
})

plot_dat <- plot_dat_list %>% dplyr::bind_rows() %>% 
  mutate(model    = factor(model,    levels = model_suffices, labels = model_labels),
         variable = factor(variable, levels = c("Age","Period","Cohort")))



# marginal effect plots ---------------------------------------------------
vlines_cohort <- list("cohort" = c(1938.5,1946.5,1966.5,1982.5,1994.5))
cols          <- rev(scales::hue_pal()(3))

# vertical lines, only to be drawn for the cohort plots
vline_dat <- data.frame(variable = factor("Cohort", levels = c("Age","Period","Cohort")),
                        x        = c(1938.5,1946.5,1966.5,1982.5,1994.5))

# 1) plot grid for participation and frequency
gg1 <- plot_dat %>% 
  filter(model %in% c("Participation","Frequency")) %>% 
  ggplot(aes(x = value, y = effect, col = model)) +
  geom_hline(yintercept = 1, lty = 2, col = gray(0.3)) +
  geom_vline(data = vline_dat, aes(xintercept = x), lty = 2, col = gray(0.3)) +
  geom_line() +
  facet_grid(model ~ variable, scales = "free") +
  scale_y_continuous("Odds Ratio", breaks = c(.25,.5,1,2), trans = "log2",
                     limits = range(plot_dat$effect)) +
  scale_color_manual(values = cols[1:2]) +
  theme(axis.title.x     = element_blank(),
        axis.text.x      = element_blank(),
        strip.background = element_rect(fill = gray(0.8)),
        legend.position  = "none")

# 2) plot grid for expenses
gg2 <- plot_dat %>% 
  filter(model == "Rel. Expenses") %>% 
  ggplot(aes(x = value, y = effect, col = model)) +
  geom_hline(yintercept = 1, lty = 2, col = gray(0.3)) +
  geom_vline(data = vline_dat, aes(xintercept = x), lty = 2, col = gray(0.3)) +
  geom_line() +
  facet_grid(model ~ variable, scales = "free") +
  scale_y_continuous("exp(Effect)", breaks = c(0.9,1,1.1), trans = "log2") +
  scale_color_manual(values = cols[3]) +
  theme(axis.title.x     = element_blank(),
        strip.text.x     = element_blank(),
        strip.background = element_rect(fill = gray(0.8)),
        legend.position  = "none")

ggpubr::ggarrange(gg1, gg2, nrow = 2, heights = c(2/3,1/3))
ggsave("../Figures/2_marginalEffects.png", width = 6, height = 4)

create_APCsummary(list(model_P), dat = dat_P, apc_range = list("cohort" = 1939:2018))
create_APCsummary(list(model_F), dat = dat_F, apc_range = list("cohort" = 1939:2018))
create_APCsummary(list(model_E), dat = dat_E, apc_range = list("cohort" = 1939:2018))



# linear covariate effects ------------------------------------------------
plot_dat_list <- lapply(model_suffices, function(suffix) {
  
  m <- get(paste0("model_",suffix))
  plot_dat_m <- plot_linearEffects(m, return_plotData = TRUE) %>% 
    mutate(model = model_labels[match(suffix, model_suffices)])
  
  return(plot_dat_m)
})

plot_dat <- dplyr::bind_rows(plot_dat_list) %>%
  mutate(model = factor(x = model,
                        levels = c("Participation", "Frequency",
                                   "Rel. Expenses"))) %>% 
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

ggplot(plot_dat, mapping = aes(x = param, y = coef)) +
  geom_hline(yintercept = 1, col = gray(0.3), lty = 2) +
  geom_pointrange(mapping = aes(ymin = CI_lower, ymax = CI_upper, col = vargroup), size = 1) +
  geom_point(mapping = aes(col = vargroup), size = 1) +
  scale_y_continuous(trans = "log2", name = "exp(Effect)") +
  colorspace::scale_colour_discrete_qualitative(palette = "Dark 3") +
  facet_grid(model ~ vargroup, scales = "free_x") +
  theme(legend.position = "none",
        axis.title.x    = element_blank(),
        axis.text.x     = element_text(angle = 45, hjust = 1),
        strip.background = element_rect(fill = gray(0.8)))
ggsave("../Figures/2_linearEffects.png", width = 8, height = 7)



# nonlinear income effects ------------------------------------------------
plot_dat_list <- lapply(model_suffices, function(suffix) {
  
  m <- get(paste0("model_",suffix))
  plot_dat_m <- plot_1Dsmooth(m, select = 2, return_plotData = TRUE) %>% 
    mutate(model = model_labels[match(suffix, model_suffices)])
  
  return(plot_dat_m)
})

plot_dat <- dplyr::bind_rows(plot_dat_list) %>%
  mutate(model = factor(x = model,
                        levels = c("Participation", "Frequency",
                                   "Rel. Expenses")))

# trim the CIs to limit the y-axis
ylim <- c(0.25,128)
plot_dat$CI_lower[plot_dat$CI_lower < ylim[1]] <- ylim[1]
plot_dat$CI_upper[plot_dat$CI_upper > ylim[2]] <- ylim[2]

ggplot() +
  geom_hline(yintercept = 1, lty = 2, col = gray(0.3)) +
  geom_ribbon(data = plot_dat, aes(x, ymin = CI_lower, ymax = CI_upper),
              fill = gray(0.75)) +
  geom_line(data = plot_dat, aes(x, y, col = model)) +
  facet_grid(. ~ model) + scale_x_continuous(limits = c(0, 6000)) +
  scale_y_continuous("exp(Effect)", trans = "log2",
                     breaks = 2^c(-2,1,4,7), labels = c("0.25","2","16","128")) +
  scale_color_manual(values = cols) +
  xlab("Household income [€]") +
  theme(strip.background = element_rect(fill = gray(0.8)),
        legend.position  = "none")
ggsave("../Figures/2_incomeEffects.png", width = 6, height = 2)




# model evaluation --------------------------------------------------------

# Participation:
set.seed(3456)
train_index <- sample(1:nrow(dat_P), 0.8 * nrow(dat_P))
dat_P_train <- dat_P[train_index, ]
dat_P_test <- dat_P[-train_index, ]
model_P_auc <- bam(formula = y_atLeastOneUR ~ te(period, age, k = c(10, 10), bs = "ps") +
                 S_Geschlecht + S_Kinder_0_bis_5_binaer + S_Wohnortgroesse +
                 S_Bildung + s(S_Einkommen_HH_equi, bs = "ps", k =10) +
                 S_Haushaltsgroesse,
               family = binomial(link = "logit"), data = dat_P_train)
prediction <- predict(object = model_P_auc, newdata = dat_P_test)
roc(dat_P_test$y_atLeastOneUR, prediction)$auc
# 0.7247

# Frequency:
set.seed(3456)
train_index <- sample(1:nrow(dat_F), 0.8 * nrow(dat_F))
dat_F_train <- dat_F[train_index, ]
dat_F_test <- dat_F[-train_index, ]
model_F_auc <- bam(formula = y_atLeastTwoURs ~ te(period, age, k = c(10, 10), bs = "ps") +
                     S_Geschlecht + S_Kinder_0_bis_5_binaer + S_Wohnortgroesse +
                     S_Bildung + s(S_Einkommen_HH_equi, bs = "ps", k =10) +
                     S_Haushaltsgroesse,
                   family = binomial(link = "logit"), data = dat_F_train)
prediction <- predict(object = model_F_auc, newdata = dat_F_test)
roc(dat_F_test$y_atLeastTwoURs, prediction)$auc
# 0.6648

# Expenses:
set.seed(3456)
train_index <- sample(1:nrow(dat_E), 0.8 * nrow(dat_E))
dat_E_train <- dat_E[train_index, ]
dat_E_test <- dat_E[-train_index, ]
model_E_train <- bam(formula = rel_expenses ~ te(period, age, bs = "ps", k = c(10, 10)) +
                 S_Geschlecht + S_Kinder_0_bis_5_binaer + S_Wohnortgroesse +
                 S_Bildung + S_Haushaltsgroesse + JS_HUR_Reisedauer +
                 s(S_Einkommen_HH_equi, bs = "ps", k = 10),
               family = Gamma(link = "log"),
               data = dat_E_train)
prediction <- predict(object = model_E_train, newdata = dat_E_test, 
                      type = "response")
mean(abs(prediction - dat_E_test$rel_expenses), na.rm = TRUE)
median(abs(prediction - dat_E_test$rel_expenses) / dat_E_test$rel_expenses,
     na.rm = TRUE)
plot(dat_E_test$rel_expenses, prediction)

# QQ plot for the expenses model
png("../Figures/2_expenses_QQplot.png", width = 700, height = 700, pointsize = 20)
qq.gam(model_E)
dev.off()
