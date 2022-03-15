###### File containing functions needed for APC analysis paper:


###################################################################################

# Function to prepare data for further analyses:
data_preparation <- function(data, thresholds, labels, target) {
  
  data$target <- data[[target]]
  
  # Removing of observations:
  data <- data %>% filter(!is.na(target))
  
  # Categorization of travel distances into five categories:
  # data <- data %>%
  #   mutate(JS_HUR_Reisedistanz_cat =
  #            case_when(JS_HUR_Reisedistanz < thresholds[1] ~ labels[1],
  #                      JS_HUR_Reisedistanz < thresholds[2] ~ labels[2],
  #                      JS_HUR_Reisedistanz < thresholds[3] ~ labels[3],
  #                      JS_HUR_Reisedistanz < thresholds[4] ~ labels[4],
  #                      TRUE ~ labels[5]),
  #          JS_HUR_Reisedistanz_cat = factor(JS_HUR_Reisedistanz_cat,
  #                                           levels = labels))
  # 
  # # Transformation of travel distances to log10 scale:
  # data <- data %>% mutate(JS_HUR_Reisedistanz_log10 = log10(JS_HUR_Reisedistanz))
  
  # Calculation of cohort variable (birth_year)
  data <- data %>% mutate(birth_year = travel_year - S_Alter)
  return(data)
}

################################################################################

# # Function to visualize frequencies of travel distance categories over periods:
# freq_periods <- function(data, xlab = "Period", ylab = "Relative frequency",
#                          legend_title = "Distance category") {
#   
#   data$travel_year_cat <- as.factor(data$travel_year)
#   
#   # Creation of graphical output:
#   theme <- theme_minimal() +
#     theme(text = element_text(size = 14), axis.title = element_text(size = 14),
#           axis.text = element_text(size = 14),
#           legend.text = element_text(size = 14),
#           plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
#           strip.text.y = element_text(size = 14), legend.text.align = 0,
#           strip.placement = "outside", strip.background = element_blank(),
#           axis.title.y = element_text(margin = margin(0, 10, 0, 0)),
#           axis.title.x = element_text(margin = margin(10, 0, 0, 0)))
#   plot <- ggplot(data = data,
#                  mapping = aes(x = travel_year,
#                                fill = JS_HUR_Reisedistanz_cat)) +
#     geom_bar(position = position_fill(reverse = TRUE)) + ylab(ylab) + xlab(xlab) +
#     scale_y_continuous(labels = scales::percent) +
#     labs(fill = legend_title) +
#     scale_fill_manual(name = "Distance category",
#                       values = c("#08306B", "#2171B5", "#4292C6", "#6BAED6",
#                                  "#C6DBEF")) +
#     theme + theme(axis.text.y = element_blank())
#   return(plot)
# }

################################################################################

# # Function in order to prepare individual weights:
# 
# finalize_weights <- function(data) {
#   
#   # Preparation of information about overall population:
#   table_df <- data.frame(year = c(1971:1992, 1994:2018),
#                          factor = c((44.38*10^6/6237),
#                                     (((44.38+44.61)/2)*10^6/6057),
#                                     (44.61*10^6/6086), 7399, 7391, 7244, 7579,
#                                     7562, 7545.3, 7643.5, 7426.44, 7738.92,
#                                     7993.193, 8044.10, 8009.43, 8022.72,
#                                     7862.38,  7629.27, 7992.8, 9429.00,
#                                     9303.79, 9696.17, 8062.90, 8008.30,
#                                     8368.00,  8027.00, 8043.00, 8244.00,
#                                     8146.00, 8142.00, 8061.50, 8199.20,
#                                     8399.30,  8353.50, 8481.20, 8250.76,
#                                     8300.96, 8462.90, 9163.90, 9130.10,
#                                     9050.40, 9021.76, 9135.56, 8978.30,
#                                     9017.84, 8988.20, 9110.31))
#   data$year <- data$travel_year
#   lj <- suppressMessages(data %>% left_join(table_df))
#   lj$factor <- ifelse(test = lj$year == 1993 & lj$S_Herkunft == "West",
#                       yes = 7785.9,
#                       no = ifelse(test = lj$year == 1993 &
#                                     lj$S_Herkunft == "Ost",
#                                   yes = 7968.7,
#                                   no = ifelse(test = lj$year == 1993 &
#                                                 is.na(lj$S_Herkunft) == TRUE,
#                                               yes = 7968.7(7785.9 + 7968.7) / 2,
#                                               no = lj$factor)))
#   
#   lj$G_Gewichtung <- ifelse(test = is.na(lj$G_Gewichtung), yes = 1,
#                             no = lj$G_Gewichtung)
#   lj$G_Gewichtung_Pop <- lj$G_Gewichtung * lj$factor
#   x <- lj$G_Gewichtung_Pop
#   return(x)
# }

################################################################################

# Function to preprocess data for modeling:
preprocessing_model <- function(data, target) {
  
  # Renaming of temporal variables:
  data <- data %>%
    dplyr::rename(age = S_Alter, period = travel_year, cohort = birth_year,
                  variable = target)
  
  # Preparation of the binary response variables:
  # data <- data %>%
  #   mutate(y_logit1 = case_when(match(dist, levels(dist)) == 1 ~ "1",
  #                               TRUE                           ~ "0"),
  #          y_logit2 = case_when(match(dist, levels(dist)) == 2 ~ "1",
  #                               TRUE                           ~ "0"),
  #          y_logit3 = case_when(match(dist, levels(dist)) == 3 ~ "1",
  #                               TRUE                           ~ "0"),
  #          y_logit4 = case_when(match(dist, levels(dist)) == 4 ~ "1",
  #                               TRUE                           ~ "0"),
  #          y_logit5 = case_when(match(dist, levels(dist)) == 5 ~ "1",
  #                               TRUE                           ~ "0"),
  #          y_logit1 = factor(y_logit1),
  #          y_logit2 = factor(y_logit2),
  #          y_logit3 = factor(y_logit3),
  #          y_logit4 = factor(y_logit4),
  #          y_logit5 = factor(y_logit5))
  
  # Preperatin of covariates for covariate model:
  data$S_Einkommen_HH[data$S_Einkommen_HH == -99] <- NA
  data <- data %>%
    mutate(S_Haushaltsgroesse =
             case_when(is.na(S_Haushaltsgroesse) ~ NA_character_,
                       S_Haushaltsgroesse == "keine Angabe" ~ NA_character_,
                       TRUE ~ as.character(S_Haushaltsgroesse)),
           S_Haushaltsgroesse = factor(S_Haushaltsgroesse,
                                       levels = c("1 Person", "2 Personen",
                                                  "3 Personen","4 Personen",
                                                  "5 Personen und mehr")),
           S_Haushaltsgroesse_num =
             case_when(is.na(S_Haushaltsgroesse) ~ NA_real_,
                       TRUE ~ as.numeric(substr(S_Haushaltsgroesse, 1, 1))),
           JS_HUR_Reisedauer =
             case_when(is.na(JS_HUR_Reisedauer) ~ NA_character_,
                       JS_HUR_Reisedauer == "keine Angabe" ~ NA_character_,
                       TRUE ~ as.character(JS_HUR_Reisedauer)),
           JS_HUR_Reisedauer = factor(JS_HUR_Reisedauer,
                                      levels = c("bis 5 Tage", "6 bis 8 Tage",
                                                 "9 bis 12 Tage", "13 bis 15 Tage",
                                                 "16 bis 19 Tage", "20 bis 22 Tage",
                                                 "23 bis 26 Tage", "27 bis 29 Tage",
                                                 "30 Tage und mehr")))
  return(data)
}

################################################################################

# Function in order to estimate a pure APC model:
model_pure <- function(data, target, method = "gam") {
  message("Estimate model ", target, "...")
  #data$y <- data[[paste0("y_logit", target)]]
  data$y <- data[[target]]
  if (method == "bam") {
    model <- bam(formula = y ~ te(period, age, k = c(10, 10), bs = "ps"),
                 family = Gamma(link = "log"),
                 data = data)
  }
  if (method == "gam") {
    model <- gam(formula = y ~ te(period, age, k = c(10, 10), bs = "ps"),
                 family = Gamma(link = "log"),
                 data = data)
  }
  return(model)
}

################################################################################

# Function in order to estimate a covariate model:
model_covariate <- function(data, target, method = "gam", variables_num, variables_cat) {
  message("Estimate model ", target, "...")
  #data$y <- data[[paste0("y_logit", target)]]
  data$y <- data[[target]]
  
  if(length(variables_num) == 0 & length(variables_cat) == 0) {
    stop("Use model_pure().")
  }
  if (length(variables_num) >= 1 & length(variables_cat) >= 1) {
    form <- as.formula(paste("y ~ te(period, age, k = c(10, 10), bs = \"ps\") +",
                             paste("s(", variables_num, ", bs = \"ps\", k = 10)", collapse = "+"),
                             "+",
                             paste(variables_cat, collapse = "+")))
  }
  if (length(variables_num) >= 1 & length(variables_cat) == 0) {
    form <- as.formula(paste("y ~ te(period, age, k = c(10, 10), bs = \"ps\") +",
                             paste("s(", variables_num, ", bs = \"ps\", k = 10)", collapse = "+")))
  }
  if (length(variables_num) == 0 & length(variables_cat) >= 1) {
    form <- as.formula(paste("y ~ te(period, age, k = c(10, 10), bs = \"ps\") +",
                             paste(variables_cat, collapse = "+")))
  }

  
  if (method == "bam") {
    model <- bam(formula = form,
                   # y ~ te(period, age, k = c(10, 10), bs = "ps") +
                   # s(S_Einkommen_HH, bs = "ps", k = 10) + S_Haushaltsgroesse +
                   # JS_HUR_Reisedauer, 
                 family = Gamma(link = "log"),
                 data = data)
  }
  if (method == "gam") {
    model <- gam(formula = form,
                   # y ~ te(period, age, k = c(10, 10), bs = "ps") +
                   # s(S_Einkommen_HH, bs = "ps", k = 10) + S_Haushaltsgroesse +
                   # JS_HUR_Reisedauer,
                 family = Gamma(link = "log"),
                 data = data)
  }
  return(model)
}


################################################################################

# # Function in order to calculate AUC value of a model:
# calculate_auc <- function(data, target, method = "gam", type = "pure",
#                           seed = 2020) {
#   
#   set.seed(seed)
#   message("Calculate AUC for model ", target, "...")
#   data$y <- data[[paste0("y_logit", target)]]
#   
#   # Splitting the data into training and test data:
#   train_index <- sample(1:nrow(data), 0.8 * nrow(data))
#   data_train <- data[train_index, ]
#   data_test <- data[-train_index, ]
#   
#   # Estimation of the model:
#   if (method == "bam") {
#     if (type == "pure") {
#       model <- model_pure(data_train, target, method = "bam")
#     }
#     if (type == "covariate") {
#       model <- model_covariate(data_train, target, method = "bam")
#     }
#   }
#   if (method == "gam") {
#     if (type == "pure") {
#       model <- model_pure(data_train, target, method = "gam")
#     }
#     if (type == "covariate") {
#       model <- model_covariate(data_train, target, method = "gam")
#     }
#   }
#   
#   # Computation of AUC:
#   prediction <- predict(object = model, newdata = data_test)
#   auc <- suppressMessages(roc(data_test$y, prediction))
#   return(auc)
# }

################################################################################

# Function in order to visualize estimated tensor products via heatmaps:
plot_heatmap <- function(model, data, type = "pure", angle = 15, variables_num = NULL, variables_cat = NULL, markdown = FALSE) {
  
  # Data preparations:
  ages <- min(data$age):max(data$age)
  periods <- min(data$period):max(data$period)
  grid <- expand.grid(age = ages, period = periods)
  if (type == "covariate") {
    for (i in variables_num) {
      grid <- mutate(grid, "{i}" := mean(data[[i]], na.rm = TRUE))
    }
    
    # For categorical variables use first level
    for (i in variables_cat) {
      grid <- mutate(grid, "{i}" := levels(data[[i]])[1])
    }
    
    # grid <- grid %>% mutate(S_Einkommen_HH = mean(data$S_Einkommen_HH,
    #                                               na.rm = TRUE),
    #                         S_Haushaltsgroesse = "2 Personen",
    #                         JS_HUR_Reisedauer = "6 bis 8 Tage")
  }
  
  # Creation of plot data:
  terms <- "te(period,age)"
  prediction <- predict(object = model, newdata = grid, type = "terms",
                        terms = terms, se.fit = TRUE) 
  plot_dat <- grid %>%
    mutate(effect = prediction$fit, se = prediction$se.fit,
           cohort = (period - age))
  plot_dat <- plot_dat %>% mutate(effect = effect - mean(effect)) %>%
    mutate(exp_effect = exp(effect), exp_se = sqrt((se^2 )* (exp_effect^2)),
           upper = effect + qnorm(0.95) * se, lower = effect - qnorm(0.95) * se, 
           exp_upper =  (exp_effect + qnorm(0.975) * exp_se),
           exp_lower = (exp_effect - qnorm(0.975) * exp_se),
           cohort = (period - age))
  
  # Definition of thresholds:
  age_thresholds <- seq(10, 100, by = 5)
  #period_thresholds <- seq(1970, 2020, by = 5)
  period_thresholds <- seq(periods[1], periods[length(periods)], by = 5)
  cohort_thresholds <- c(1946, 1966, 1982, 1994)
  plot_dat <- plot_dat %>%
    mutate(age_cat = cut(age, breaks = age_thresholds, right = F),
           period_cat = cut(period, breaks = period_thresholds, right = FALSE),
           cohort_cat = cut(cohort, breaks = cohort_thresholds, right = FALSE))
  dat_effects <- plot_dat %>%
    group_by(age_cat, period_cat) %>%
    summarize(mean_effect = mean(effect),
              mean_upper = mean(upper),
              mean_lower = mean(lower),
              mean_exp_effect = mean(exp_effect),
              mean_exp_upper = mean(exp_upper),
              mean_exp_lower = mean(exp_lower)
    ) %>%
    ungroup()
  dat_effects <- dat_effects %>%
    mutate(mean_exp_effect = ifelse(test = mean_exp_effect < 0.1, yes = 0.1,
                                    no = mean_exp_effect),
           mean_exp_upper = ifelse(test = mean_exp_upper < 0.1, yes = 0.1,
                                   no = mean_exp_effect),
           mean_exp_lower = ifelse(test = mean_exp_lower < 0.1, yes = 0.1,
                                   no = mean_exp_lower),
           mean_exp_effect = ifelse(test = mean_exp_effect > 6, yes = 6,
                                    no = mean_exp_effect),
           mean_exp_upper = ifelse(test = mean_exp_upper > 6, yes = 6,
                                   no = mean_exp_effect),
           mean_exp_lower = ifelse(test = mean_exp_lower > 6, yes = 6,
                                   no = mean_exp_lower))
  plot_dat <- full_join(plot_dat, dat_effects)
  
  # Drawing of cohort lines:
  if (periods[1] <= 1970) {
    x1 <- (1970:periods[length(periods)]) + 0.5
  } else {
    x1 <- (periods[1]:periods[length(periods)]) + 0.5
  }
  beta1 <- c(intercept = -1950, slope = 1)
  y1 <- cbind(1, x1) %*% beta1
  if (periods[1] <= 1978) {
    x2 <- (1978:periods[length(periods)]) + 0.5
  } else {
    x2 <- (periods[1]:periods[length(periods)]) + 0.5
  }
  beta2 <- c(intercept = -1965, slope = 1)
  y2 <- cbind(1, x2) %*% beta2
  if (periods[1] <= 1993) {
    x3 <- (1993:periods[length(periods)]) + 0.5
  } else {
    x3 <- (periods[1]:periods[length(periods)]) + 0.5
  }
  beta3 <- c(intercept = -1980, slope = 1)
  y3 <- cbind(1, x3) %*% beta3
  if (periods[1] <= 2008) {
    x4 <- (2008:periods[length(periods)]) + 0.5
  } else {
    x4 <- (periods[1]:periods[length(periods)]) + 0.5
  }
  beta4 <- c(intercept = -1995, slope = 1)
  y4 <- cbind(1, x4) %*% beta4
  
  # Graphical visualization of heatmap:

  
  if (markdown == FALSE) {
    theme <- theme_minimal() +
      theme(text = element_text(size = 12), axis.title = element_text(size = 20),
            axis.text = element_text(size = 18),
            legend.text = element_text(size = 15),
            legend.title = element_text(size = 20),
            plot.title = element_text(hjust = 0.5, size = 22, face = "bold"),
            strip.text.y = element_text(size = 18),
            strip.text.x = element_text(size = 18),
            strip.placement = "outside", strip.background = element_blank(),
            axis.title.y = element_text(margin = margin(0, 10, 0, 0)),
            axis.title.x = element_text(margin = margin(10, 0, 0, 0)))
  }
  
  if (markdown == TRUE) {
    theme <- theme_minimal() +
      theme(text = element_text(size = 10), axis.title = element_text(size = 15),
            axis.text = element_text(size = 10),
            legend.text = element_text(size = 10),
            legend.title = element_text(size = 15),
            plot.title = element_text(hjust = 0.5, size = 17, face = "bold"),
            strip.text.y = element_text(size = 10),
            strip.text.x = element_text(size = 10),
            strip.placement = "outside", strip.background = element_blank(),
            axis.title.y = element_text(margin = margin(0, 10, 0, 0)),
            axis.title.x = element_text(margin = margin(10, 0, 0, 0)))
  }
  
  # Labels depending on length of x-axis
  if (length(periods) >= 30) {
    breaks <- c(1970, 1980, 1990, 2000, 2010)
  } else {
    breaks <- c(1970, 1975, 1980, 1985, 1990, 1995, 2000, 2005, 2010, 2015)
  }
  
  # Limits for fill color
  limits_color <- c(min(floor(plot_dat$mean_exp_lower * 1000)/ 1000), max(ceiling(plot_dat$mean_exp_upper * 1000)/ 1000))
  
  plot1 <- ggplot(data = plot_dat, mapping = aes(x = period, y = age,
                                                 fill = mean_exp_effect)) +
    geom_tile() +
    scale_fill_gradient2(trans = "log", 
                         low = "dodgerblue3", mid = "white",
                         high = "firebrick3",
                         # breaks = c(0.125, 0.25, 0.5, 1, 2, 4),
                         # labels = c(0.125, 0.25, 0.5, 1, 2, 4),
                         breaks = limits_color,
                         labels = limits_color,
                         limits = limits_color,
                         name = "Mean exp effect     ") +
    #limits = c(0.1, 6))+#, name = "Odds Ratio") +
    scale_x_continuous(breaks = breaks) + 
    ggtitle("Effect") +
    theme + 
    theme(legend.position = "bottom",
          plot.title = element_text(hjust = 0.5)) +
    guides(fill = guide_colourbar(barwidth = 17, barheight = 0.5)) +
    geom_segment(aes(x = x1[1], xend = x1[length(x1)], y = y1[1],
                     yend = y1[length(y1)])) +
    geom_text(aes(x = x1[length(x1)] - 6,
                  y = y1[length(y1)] - 4, label = "Cohort 1945 - 1949"), 
              #angle = 30, size = 4) +
              angle = angle, size = 4) +
    geom_segment(aes(x = x2[1], xend = x2[length(x2)], y = y2[1],
                     yend = y2[length(y2)])) +
    geom_text(aes(x = x2[length(x2)] - 6,
                  y = y2[length(y2)] - 4, label = "Cohort 1960 - 1964"), 
              angle = angle, size = 4) +
    geom_segment(aes(x = x3[1], xend = x3[length(x3)], y = y3[1],
                     yend = y3[length(y3)])) +
    geom_text(aes(x = x3[length(x3)] - 6,
                  y = y3[length(y3)] - 4, label = "Cohort 1975 - 1979"), 
              angle = angle, size = 4) +
    geom_segment(aes(x = x4[1], xend = x4[length(x4)], y = y4[1],
                     yend = y4[length(y4)])) +
    geom_text(aes(x = x4[length(x4)] - 6,
                  y = y4[length(y4)] - 4, label = "Cohort 1990 - 1994"), 
              angle = angle, size = 4) +
    ylab("Age") + xlab("Period")
  plot2 <- ggplot(data = plot_dat, mapping = aes(x = period, y = age,
                                                 fill = mean_exp_lower)) +
    geom_tile() +
    
    scale_fill_gradient2(trans = "log", 
                         low = "dodgerblue3", mid = "white",
                         high = "firebrick3",
                         # breaks = c(0.125, 0.25, 0.5, 1, 2, 4),
                         # labels = c(0.125, 0.25, 0.5, 1, 2, 4),
                         # limits = c(0.1, 6))+#, name = "Odds Ratio") +
                         breaks = limits_color,
                         labels = limits_color,
                         limits = limits_color) +
    scale_x_continuous(breaks = breaks) + 
    ggtitle("Lower 95% CI boundary") +
    theme + 
    theme(legend.position = "bottom",
          plot.title = element_text(hjust = 0.5)) +
    guides(fill = guide_colourbar(barwidth = 17, barheight = 0.5)) +
    geom_segment(aes(x = x1[1], xend = x1[length(x1)], y = y1[1],
                     yend = y1[length(y1)])) +
    geom_text(aes(x = x1[length(x1)] - 6,
                  y = y1[length(y1)] - 4, label = "Cohort 1945 - 1949"), 
              
              angle = angle, size = 4) +
    geom_segment(aes(x = x2[1], xend = x2[length(x2)], y = y2[1],
                     yend = y2[length(y2)])) +
    geom_text(aes(x = x2[length(x2)] - 6,
                  y = y2[length(y2)] - 4, label = "Cohort 1960 - 1964"), 
              
              angle = angle, size = 4) +
    geom_segment(aes(x = x3[1], xend = x3[length(x3)], y = y3[1],
                     yend = y3[length(y3)])) +
    geom_text(aes(x = x3[length(x3)] - 6,
                  y = y3[length(y3)] - 4, label = "Cohort 1975 - 1979"), 
              
              angle = angle, size = 4) +
    geom_segment(aes(x = x4[1], xend = x4[length(x4)], y = y4[1],
                     yend = y4[length(y4)])) +
    geom_text(aes(x = x4[length(x4)] - 6,
                  y = y4[length(y4)] - 4, label = "Cohort 1990 - 1994"), 
              
              angle = angle, size = 4) +
    ylab("Age") + xlab("Period")
  plot3 <- ggplot(data = plot_dat, mapping = aes(x = period, y = age,
                                                 fill = mean_exp_upper)) +
    geom_tile() +
    scale_fill_gradient2(trans = "log", 
                         low = "dodgerblue3", mid = "white",
                         high = "firebrick3",
                         # breaks = c(0.125, 0.25, 0.5, 1, 2, 4),
                         # labels = c(0.125, 0.25, 0.5, 1, 2, 4),
                         # limits = c(0.1, 6), name = "Odds Ratio") +
                         breaks = limits_color,
                         labels = limits_color,
                         limits = limits_color) +
    scale_x_continuous(breaks = breaks) + 
    ggtitle("Upper 95% CI boundary") +
    theme + 
    theme(legend.position = "bottom",
          plot.title = element_text(hjust = 0.5)) +
    guides(fill = guide_colourbar(barwidth = 17, barheight = 0.5)) +
    geom_segment(aes(x = x1[1], xend = x1[length(x1)], y = y1[1],
                     yend = y1[length(y1)])) +
    geom_text(aes(x = x1[length(x1)] - 6,
                  y = y1[length(y1)] - 4, label = "Cohort 1945 - 1949"), 
              
              angle = angle, size = 4) +
    geom_segment(aes(x = x2[1], xend = x2[length(x2)], y = y2[1],
                     yend = y2[length(y2)])) +
    geom_text(aes(x = x2[length(x2)] - 6,
                  y = y2[length(y2)] - 4, label = "Cohort 1960 - 1964"), 
              
              angle = angle, size = 4) +
    geom_segment(aes(x = x3[1], xend = x3[length(x3)], y = y3[1],
                     yend = y3[length(y3)])) +
    geom_text(aes(x = x3[length(x3)] - 6,
                  y = y3[length(y3)] - 4, label = "Cohort 1975 - 1979"), 
              
              angle = angle, size = 4) +
    geom_segment(aes(x = x4[1], xend = x4[length(x4)], y = y4[1],
                     yend = y4[length(y4)])) +
    geom_text(aes(x = x4[length(x4)] - 6,
                  y = y4[length(y4)] - 4, label = "Cohort 1990 - 1994"), 
              
              angle = angle, size = 4) +
    ylab("Age") + xlab("Period")
  
  if (markdown == FALSE) {
    plots <- ggarrange(plotlist = list(plot1, plot2, plot3),legend = "bottom",
                       common.legend = TRUE, ncol = 3)
  }
  if (markdown == TRUE) {
    plots <- ggarrange(plotlist = list(plot1, plot2, plot3),legend = "bottom",
                       common.legend = TRUE, nrow = 3)
  }
  
  return(plots)
}

################################################################################

# # Function in order to plot marginal age, period and cohort effects:
# plot_marginal_effects <- function(model_list, data, type = "pure") {
#   
#   # Extraction of marginal effects:
#   categories <- c("< 500 km", "500 - 1,000 km", "1,000 - 2,000 km",
#                   "2,000 - 6,000 km", "> 6,000 km")
#   ages <- min(data$age):max(data$age)
#   periods <- min(data$period):max(data$period)
#   
#   plot_dat_list <- lapply(1:length(categories), function(i) {
#     # Extraction for each individual model:
#     dat_overallEffect <- expand.grid(age = ages,
#                                      period = periods)
#     if (type == "covariate") {
#       dat_overallEffect <- dat_overallEffect %>%
#         mutate(S_Einkommen_HH = mean(data$S_Einkommen_HH, na.rm = TRUE),
#                S_Haushaltsgroesse = "2 Personen",
#                JS_HUR_Reisedauer = "6 bis 8 Tage")
#     }
#     dat_overallEffect <- dat_overallEffect %>%
#       mutate(effect = predict(model_list[[i]], newdata = ., type = "terms",
#                               terms = c("te(period,age)"))) %>%
#       mutate(cohort = period - age)
#     
#     dat_age <- dat_overallEffect %>%
#       group_by(age) %>% summarize(effect = mean(effect)) %>% ungroup() %>%
#       mutate(exp_effect = exp(effect - mean(effect)), 
#              variable   = "Age",
#              model      = categories[i]) %>%
#       dplyr::rename(value = age)
#     dat_period <- dat_overallEffect %>%
#       group_by(period) %>% summarize(effect = mean(effect)) %>% ungroup() %>%
#       mutate(exp_effect = exp(effect - mean(effect)),
#              variable   = "Period",
#              model      = categories[i]) %>%
#       dplyr::rename(value = period)
#     dat_cohort <- dat_overallEffect %>%
#       group_by(cohort) %>% summarize(effect = mean(effect)) %>% ungroup() %>%
#       mutate(exp_effect = exp(effect - mean(effect)), 
#              variable   = "Cohort",
#              model      = categories[i]) %>%
#       dplyr::rename(value = cohort)
#     
#     dplyr::bind_rows(dat_age, dat_period, dat_cohort)
#   })
#   
#   plot_dat <- dplyr::bind_rows(plot_dat_list) %>%
#     mutate(variable = factor(variable, levels = c("Age","Period","Cohort")),
#            model    = factor(model,    levels = categories))
#   
#   # plot marginal effects ---------------------------------------------------
#   ylim <- c(0.01,8)
#   
#   # data for generation thresholds
#   dat_th <- data.frame(variable  = "Cohort",
#                        threshold = c(1938.5, 1946.5, 1966.5, 1982.5, 1994.5))
#   
#   # Actual plotting: 
#   theme <- theme_minimal() +
#     theme(text = element_text(size = 16), axis.title = element_text(size = 18),
#           axis.text = element_text(size = 14),
#           legend.text = element_text(size = 16),
#           legend.key.width = unit(2, "lines"),
#           plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
#           strip.text = element_text(size = 18, face = "bold"),
#           strip.text.y = element_text(size = 16), legend.text.align = 0,
#           strip.placement = "outside", strip.background = element_blank(),
#           axis.title.y = element_text(margin = margin(0, 10, 0, 0)),
#           axis.title.x = element_text(margin = margin(10, 0, 0, 0)))
#   plot <- ggplot(plot_dat, aes(x = value, y = exp_effect, col = model)) +
#     geom_vline(data = dat_th, aes(xintercept = threshold, class = variable),
#                lty = 2, col = gray(0.75)) +
#     geom_line() +
#     facet_wrap(~ variable, scales = "free_x") +
#     scale_y_continuous("Odds Ratio",
#                        trans  = "log",
#                        breaks = c(0.009,0.017, 0.034, 0.068, 0.125, 0.25, 0.5,
#                                   1, 2, 4, 8),
#                        labels = c(0.009,0.017, 0.034, 0.068, 0.125, 0.25, 0.5,
#                                   1, 2, 4, 8),
#                        limits = ylim) +
#     scale_color_manual("Distance category", values = c("#08306B", "#2171B5",
#                                                        "#4292C6", "#6BAED6",
#                                                        "#C6DBEF"),
#                        labels = c("< 500 km", "500 - 1,000 km",
#                                   "1,000 - 2,000 km",
#                                   "2,000 - 6,000 km", "> 6,000 km")) +
#     guides(col = guide_legend(nrow = 2, byrow = FALSE)) + theme +
#     theme(legend.position = "bottom",
#           axis.title.x    = element_blank(),
#           panel.spacing = unit(2.5, "lines"))
#   return(plot)
# }

################################################################################

# Function in order to visualize partial APC plots:
partial_APC_plots <- function(model, data, variable, type = "pure",
                              title = TRUE,
                              variables_num = NULL, variables_cat = NULL,
                              markdown = FALSE) {
  
  # Definition of a grid:
  # categories <- c("< 500 km", "500 - 1,000 km", "1,000 - 2,000 km",
  #                 "2,000 - 6,000 km", "> 6,000 km")
  ages <- min(data$age):max(data$age)
  periods <- min(data$period):max(data$period)
  #ylim <- c(0.1, 8)
  
  # Data preparation:
  dat_overallEffect <- expand.grid(age = ages,
                                   period = periods)
  if (type == "covariate") {
    
    for (i in variables_num) {
      dat_overallEffect <- mutate(dat_overallEffect, "{i}" := mean(data[[i]], na.rm = TRUE))
    }
    
    # For categorical variables use first level
    for (i in variables_cat) {
      dat_overallEffect <- mutate(dat_overallEffect, "{i}" := levels(data[[i]])[1])
    }
    
    # dat_overallEffect <- dat_overallEffect %>%
    #   mutate(S_Einkommen_HH = mean(data$S_Einkommen_HH, na.rm = TRUE),
    #          S_Haushaltsgroesse = "2 Personen",
    #          JS_HUR_Reisedauer = "6 bis 8 Tage")
  }
  dat_overallEffect <- dat_overallEffect %>%
    mutate(effect = rowSums(predict(object = model, newdata = .,
                                    type = "terms",
                                    terms = c("te(period,age)"))),
           exp_effect = exp(effect - mean(effect))) %>%
    mutate(cohort = period - age)
  dat_age <- dat_overallEffect %>%
    group_by(age) %>% summarize(effect = mean(effect)) %>% ungroup() %>%
    mutate(exp_effect = exp(effect - mean(effect)), variable = "Age") %>%
    dplyr::rename(value = age)
  dat_period <- dat_overallEffect %>%
    group_by(period) %>% summarize(effect = mean(effect)) %>% ungroup() %>%
    mutate(exp_effect = exp(effect - mean(effect)), variable   = "Period") %>%
    dplyr::rename(value = period)
  dat_cohort <- dat_overallEffect %>%
    group_by(cohort) %>% summarize(effect = mean(effect)) %>% ungroup() %>%
    mutate(exp_effect = exp(effect - mean(effect)), variable = "Cohort") %>%
    dplyr::rename(value = cohort)
  
  if (markdown == FALSE) {
    theme <- theme_minimal() +
      theme(text = element_text(size = 16), axis.title = element_text(size = 16),
            axis.text = element_text(size = 16),
            legend.text = element_text(size = 12),
            plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
            strip.text.y = element_text(size = 16), 
            strip.placement = "outside", strip.background = element_blank(),
            axis.title.y = element_text(margin = margin(0, 10, 0, 0)),
            axis.title.x = element_text(margin = margin(10, 0, 0, 0))) 
  }
  
  if (markdown == TRUE) {
    theme <- theme_minimal() +
      theme(text = element_text(size = 10), axis.title = element_text(size = 10),
            axis.text = element_text(size = 10),
            axis.text.x = element_text(angle = 90),
            legend.text = element_text(size = 10),
            plot.title = element_text(hjust = 0.5, size = 10, face = "bold"),
            strip.text.y = element_text(size = 10), 
            strip.placement = "outside", strip.background = element_blank(),
            axis.title.y = element_text(margin = margin(0, 10, 0, 0)),
            axis.title.x = element_text(margin = margin(10, 0, 0, 0))) 
  }  
  # Plots for age:
  if (variable == "age") {
    # against period
    ageperiod <- ggplot() +
      geom_line(data = dat_overallEffect,
                mapping = aes(x = age, y = exp_effect, group = period,
                              col = period)) +
      scale_color_continuous(low = "grey90", high = "grey10",
                             name = "Period",
                             breaks = as.integer(c(1980, 2010))) +
      geom_line(data = dat_age,
                mapping = aes(x = value, y = exp_effect),
                size = 1.5, col = "RoyalBlue3") +
      scale_y_continuous(trans  = "log") +#,
                         # breaks = c(0.009,0.017, 0.034, 0.068, 0.125, 0.25,
                         #            0.5, 1, 2, 4, 8),
                         # labels = c(0.009,0.017, 0.034, 0.068, 0.125, 0.25,
                         #            0.5, 1, 2, 4, 8),
                         #limits = ylim) +
      ylab("Exp effect") + xlab("Period") + theme
    if (title == TRUE) {
      ageperiod <- ageperiod + ggtitle("Age effect by periods")
    }
    # against cohort
    agecohort <- ggplot() +
      geom_line(data    = dat_overallEffect,
                mapping = aes(x = age, y = exp_effect, group = cohort,
                              col = cohort)) +
      scale_color_continuous(low = "grey90", high = "grey10",
                             name = "Cohort",
                             breaks = as.integer(c(1900, 1950, 2000))) +
      geom_line(data = dat_age,
                mapping = aes(x = value, y = exp_effect),
                size = 1.5, col = "RoyalBlue3") +
      scale_y_continuous(trans  = "log") + #,
                         # breaks = c(0.009,0.017, 0.034, 0.068, 0.125, 0.25,
                         #            0.5, 1, 2, 4, 8),
                         # labels = c(0.009,0.017, 0.034, 0.068, 0.125, 0.25,
                         #            0.5, 1, 2, 4, 8),
                         #limits = ylim) +
      theme + theme(axis.text.y = element_blank()) + ylab(" ") + xlab("Age")
    if (title == TRUE) {
      agecohort <- agecohort + ggtitle("Age effect by cohorts")
    }
    # both plots:
    plots <- ggarrange(plotlist = list(ageperiod, agecohort), ncol = 2,
                       legend = "bottom")
  }
  
  # Plots for period:
  if (variable == "period") {
    # against age
    periodage <- ggplot() +
      geom_line(data    = dat_overallEffect,
                mapping = aes(x = period, y = exp_effect, group = age,
                              col = age)) +
      scale_color_continuous(low = "grey90", high = "grey10", name = "Age") +
      geom_line(data = dat_period,
                mapping = aes(x = value, y = exp_effect),
                size = 1.5, col = "RoyalBlue3") +
      scale_y_continuous(trans  = "log") + #,
                         # breaks = c(0.009,0.017, 0.034, 0.068, 0.125, 0.25,
                         #            0.5, 1, 2, 4, 8),
                         # labels = c(0.009,0.017, 0.034, 0.068, 0.125, 0.25,
                         #            0.5, 1, 2, 4, 8),
                         #limits = ylim) +
      scale_x_continuous(breaks = c(1970, 1980, 1990, 2000, 2010)) + 
      ylab("Exp effect") + xlab("Period") + theme
    if (title == TRUE) {
      periodage <- periodage + ggtitle("Period effect by age")
    }
    # against cohort
    periodcohort <- ggplot() +
      geom_line(data    = dat_overallEffect,
                mapping = aes(x = period, y = exp_effect, group = cohort,
                              col = cohort)) +
      scale_color_continuous(low = "grey90", high = "grey10",
                             name = "Cohort",
                             breaks = as.integer(c(1900, 1950, 2000))) +
      geom_line(data = dat_period,
                mapping = aes(x = value, y = exp_effect),
                size = 1.5, col = "RoyalBlue3") +
      scale_y_continuous(trans  = "log") +#,
                         # breaks = c(0.009,0.017, 0.034, 0.068, 0.125, 0.25,
                         #            0.5, 1, 2, 4, 8),
                         # labels = c(0.009,0.017, 0.034, 0.068, 0.125, 0.25,
                         #            0.5, 1, 2, 4, 8),
                         #limits = ylim) +
      scale_x_continuous(breaks = c(1970, 1980, 1990, 2000, 2010)) +
      theme + theme(axis.text.y = element_blank()) + ylab(" ") + xlab("Period")
    if (title == TRUE) {
      periodcohort <- periodcohort + ggtitle("Period effect by cohorts")
    }
    # both plots
    plots <- ggarrange(plotlist = list(periodage, periodcohort), ncol = 2,
                       legend = "bottom")
  }
  
  # Plots for cohort:
  if (variable == "cohort") {
    # against age:
    cohortage <- ggplot() +
      geom_line(data    = dat_overallEffect,
                mapping = aes(x = cohort, y = exp_effect, group = age,
                              col = age)) +
      scale_color_continuous(low = "grey90", high = "grey10", name = "Age") +
      geom_line(data = dat_cohort,
                mapping = aes(x = value, y = exp_effect),
                size = 1.5, col = "RoyalBlue3", alpha =0.8) +
      scale_y_continuous(trans  = "log") + #,
                         # breaks = c(0.009,0.017, 0.034, 0.068, 0.125, 0.25,
                         #            0.5, 1, 2, 4, 8),
                         # labels = c(0.009,0.017, 0.034, 0.068, 0.125, 0.25,
                         #            0.5, 1, 2, 4, 8),
                         #limits = ylim) +
      ylab("Exp effect") + xlab("Cohort") + theme 
    if (title == TRUE) {
      cohortage <- cohortage + ggtitle("Cohort effect by age")
    }
    # agaist period
    cohortperiod <- ggplot() +
      geom_line(data    = dat_overallEffect,
                mapping = aes(x = cohort, y = exp_effect, group = period,
                              col = period)) +
      scale_color_continuous(low = "grey90", high = "grey10",
                             name = "Period",
                             breaks = as.integer(c(1980, 2010))) +
      geom_line(data = dat_cohort,
                mapping = aes(x = value, y = exp_effect),
                size = 1.5, col = "RoyalBlue3", alpha = 0.8) +
      scale_y_continuous(trans  = "log") + #,
                         # breaks = c(0.009,0.017, 0.034, 0.068, 0.125, 0.25,
                         #            0.5, 1, 2, 4, 8),
                         # labels = c(0.009,0.017, 0.034, 0.068, 0.125, 0.25,
                         #            0.5, 1, 2, 4, 8),
                         #limits = ylim) +
      theme + theme(axis.text.y = element_blank()) + ylab(" ") + xlab("Cohort")
    if (title == TRUE) {
      cohortperiod <- cohortperiod + ggtitle("Cohort effect by periods")
    }
    # both plots
    plots <- ggarrange(plotlist = list(cohortage, cohortperiod), ncol = 2,
                       legend = "bottom")
  }
  # Return of the function:
  return(plots)
}

################################################################################

# Function to extract the summary of a GAM or BAM model:

extract_modelSummary <- function(model, include_expCoefs = FALSE) {
  
  checkmate::check_class(model, classes = "gam")
  checkmate::check_logical(include_expCoefs)
  
  x <- mgcv::summary.gam(model)$p.table
  dat <- data.frame("param"    = row.names(x),
                    "coef"     = unname(x[,1]),
                    "se"       = unname(x[,2]),
                    "CI_lower" = unname(x[,1] - qnorm(0.975) * x[,2]),
                    "CI_upper" = unname(x[,1] + qnorm(0.975) * x[,2]),
                    "pvalue"   = unname(x[,4]),
                    stringsAsFactors = FALSE) %>%
    mutate(param = factor(param, levels = row.names(x)))
  if (include_expCoefs) {
    # confidence intervals on exp scale are based on delta method
    dat <- dat %>%
      mutate(coef_exp = exp(coef),
             se_exp = sqrt(se^2 * exp(coef)^2)) %>%
      mutate(CI_lower_exp = coef_exp - qnorm(0.975) * se_exp,
             CI_upper_exp = coef_exp + qnorm(0.975) * se_exp) %>%
      dplyr::select(param, coef, se, CI_lower, CI_upper,
                    coef_exp, se_exp, CI_lower_exp, CI_upper_exp, pvalue)
  }
  
  return(dat)
}


################################################################################

# Function in order to visualize effects of other covariates than age, period
# and cohort:
plot_covariates <- function(model, data, variables_num, variables_cat, markdown = FALSE) {
  
  # Creation of plot data:
  plot_dat <- extract_modelSummary(model = model, include_expCoefs = TRUE)
  #ylim <- c(0.1, 8)
  
  # ggplot theme:
  theme <- theme_minimal() +
    theme(text = element_text(size = 16), axis.title = element_text(size = 16),
          axis.text = element_text(size = 16),
          legend.text = element_text(size = 16),
          plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
          strip.text.y = element_text(size = 16),
          strip.placement = "outside", strip.background = element_blank(),
          axis.title.y = element_text(margin = margin(0, 10, 0, 0)),
          axis.title.x = element_text(margin = margin(10, 0, 0, 0))) 
  
  # numeric variables

  hilfsfun <- function(z) {
    x <- plot(model, select = 2)
    plot_dat2 <- data.frame(z = x[[2]]$x,
                            estimate = as.vector(x[[2]]$fit),
                            se = as.vector(x[[2]]$se)) %>%
      filter(z < 10500) %>% 
      mutate(exp_estimate = exp(estimate),
             exp_se = sqrt(se^2 * exp_estimate^2),
             CIlower = exp_estimate - exp_se,
             CIupper = exp_estimate + exp_se)
    gg_z <- ggplot(data = plot_dat2, mapping = aes(x = z, y = exp_estimate)) +
      geom_hline(yintercept = 1, col = "red") + geom_line() +
      geom_ribbon(mapping = aes(ymin = CIlower, ymax = CIupper), alpha = 0.3) +
      # scale_x_continuous("Household income [€ / month]",
      #                    breaks = c(0, 3000, 6000, 9000),
      #                    labels = c("0","3,000","6,000","9,000")) +
      scale_x_continuous(z) +
      scale_y_continuous(trans  = "log") + #,
                         # breaks = c(min(plot_dat2$exp_estimate), max(plot_dat2$exp_estimate)),
                         # labels = c(min(plot_dat2$exp_estimate), max(plot_dat2$exp_estimate))) +
                         # breaks = c(0.125, 0.25, 0.5, 1, 2, 4, 8),
                         # labels = c(0.125, 0.25, 0.5, 1, 2, 4, 8),
                         # limits = ylim) +
      theme +
      theme(axis.title.y = element_blank(),
            #axis.text.y  = element_blank(),
            #axis.ticks.y = element_blank(),
            panel.grid.minor.y = element_blank())
    
    if (markdown == TRUE) {
      gg_z <- gg_z +
        theme(axis.text = element_text(size = 10), axis.text.x = element_text(angle = 90), axis.title = element_text(size = 12))
    }

  }
  
  plots_num <- map(variables_num, hilfsfun)
  
  # categorical variables
  
  hilfsfun2 <- function(z) {
    
    plot_dat1 <- plot_dat %>%
      filter(grepl(z, param))
    gg_z <- ggplot(data = plot_dat1,
                               mapping = aes(x = param, y = coef_exp)) +
      geom_hline(yintercept = 1, col = "red") +
      geom_point() +
      geom_pointrange(mapping = aes(ymin = CI_lower_exp, ymax = CI_upper_exp)) +
      scale_x_discrete(labels = substring(plot_dat1$param, nchar(z) + 1)) +
      # scale_x_discrete(
      #   breaks = c("2","3","4",expression("">=5)),
      #   label = c("2","3","4",expression("">=5))
      # ) +
      xlab(z) +
      scale_y_continuous("Exp Coef",
                         trans  = "log")+#,
                         # breaks = c(0.125, 0.25, 0.5, 1, 2, 4, 8),
                         # labels = c(0.125, 0.25, 0.5, 1, 2, 4, 8),
                         # limits = ylim) +
      theme +
      theme(panel.grid.minor.y = element_blank())
    
    if (markdown == TRUE) {
      gg_z <- gg_z +
        theme(axis.text = element_text(size = 10), axis.text.x = element_text(angle = 90), axis.title = element_text(size = 12))
    }
  }
  
  plots_cat <- map(variables_cat, hilfsfun2)
  
  # # Household size:
  # plot_dat1 <- plot_dat %>%
  #   filter(grepl("Haushalt", param)) %>%
  #   mutate(param = gsub("S_Haushaltsgroesse", "", param),
  #          param = gsub("5 Personen und mehr", expression("">=5), param),
  #          param = gsub(" Personen", "", param),
  #          param = factor(param, levels = c("2","3","4",expression("">=5))))
  # gg_householdSize <- ggplot(data = plot_dat1,
  #                            mapping = aes(x = param, y = coef_exp)) +
  #   geom_hline(yintercept = 1, col = "red") +
  #   geom_point() +
  #   geom_pointrange(mapping = aes(ymin = CI_lower_exp, ymax = CI_upper_exp)) +
  #   scale_x_discrete(
  #     breaks = c("2","3","4",expression("">=5)),
  #     label = c("2","3","4",expression("">=5))
  #   ) +
  #   xlab("Household size [Persons]") +
  #   scale_y_continuous("Odds Ratio", 
  #                      trans  = "log",
  #                      breaks = c(0.125, 0.25, 0.5, 1, 2, 4, 8),
  #                      labels = c(0.125, 0.25, 0.5, 1, 2, 4, 8),
  #                      limits = ylim) +
  #   theme +
  #   theme(panel.grid.minor.y = element_blank())
  # 
  # 
  # 
  # 
  # # Trip length
  # plot_dat3 <- plot_dat %>%
  #   filter(grepl("Reisedauer", param)) %>%
  #   mutate(param = gsub("JS_HUR_Reisedauer", "", param),
  #          param = gsub("30 Tage und mehr", expression("">=30), param),
  #          param = gsub(" Tage", "", param),
  #          param = gsub(" bis ", "-", param),
  #          param = factor(param, levels = c("6-8", "9-12", "13-15", "16-19",
  #                                           "20-22","23-26", "27-29",
  #                                           expression("">=30))))
  # gg_tripDuration <- ggplot(plot_dat3, aes(x = param, y = coef_exp)) +
  #   geom_hline(yintercept = 1, col = "red") +
  #   geom_point() +
  #   geom_pointrange(mapping = aes(ymin = CI_lower_exp, ymax = CI_upper_exp)) +
  #   xlab("Trip duration [days]") +
  #   scale_x_discrete(
  #     breaks = c("6-8","9-12","13-15","16-19",
  #                "20-22","23-26","27-29",expression("">=30)),
  #     label = c("6-8","9-12","13-15","16-19",
  #               "20-22","23-26","27-29",expression("">=30))
  #   ) +
  #   scale_y_continuous("Odds Ratio", 
  #                      trans  = "log",
  #                      breaks = c(0.125, 0.25, 0.5, 1, 2, 4, 8, 16, 32, 64, 128),
  #                      labels = c("", 0.25, "", 1, "", 4, "", 16, "", 64, ""),
  #                      limits = c(ylim[1], 192)) +
  #   theme +
  #   theme(panel.grid.minor.y = element_blank())
  
  # Resulting layout
  # plots <- ggpubr::ggarrange(gg_householdSize, gg_income, 
  #                            nrow = 1, ncol = 2, heights = c(0.5, 0.5),
  #                            widths = c(0.35, 0.65)) 
  # plots <- ggpubr::ggarrange(plots, gg_tripDuration, nrow = 2, ncol = 1)
  plots <- purrr::flatten(list(plots_cat, plots_num))
  plots <- ggpubr::ggarrange(plotlist = plots, ncol = 2, nrow = ceiling(length(plots)/2))

  return(plots)
}


################################################################################

# Function in order to compare marginal effects of pure and covariate model:
compare_pure_covariate <- function(model_pure, model_covariate, data, variables_num, variables_cat) {
  
  # Extraction of marginal effects:
  # categories <- c("< 500 km", "500 - 1,000 km", "1,000 - 2,000 km",
  #                 "2,000 - 6,000 km", "> 6,000 km")
  ages    <- min(data$age):max(data$age)
  periods <- min(data$period):max(data$period)
  
  # Pure model:
  dat_overallEffect <- expand.grid(age = ages, period = periods) %>%
    mutate(effect = rowSums(predict(object = model_pure, newdata = .,
                                    type = "terms",
                                    terms = c("te(period,age)")))) %>%
    mutate(cohort = period - age)
  dat_pureAgeEffect <- dat_overallEffect %>%
    group_by(age) %>% summarize(effect = mean(effect)) %>% ungroup() %>%
    mutate(exp_effect = exp(effect - mean(effect)))
  dat_purePeriodEffect <- dat_overallEffect %>%
    group_by(period) %>% summarize(effect = mean(effect)) %>% ungroup() %>%
    mutate(exp_effect = exp(effect - mean(effect)))
  dat_pureCohortEffect <- dat_overallEffect %>%
    group_by(cohort) %>% summarize(effect = mean(effect)) %>% ungroup() %>%
    mutate(exp_effect = exp(effect - mean(effect)))
  
  # Covariate model:
  dat_overallEffect <- expand.grid(age = ages,
                                   period = periods)
  for (i in c(variables_num, variables_cat)) {
    dat_overallEffect <- mutate(dat_overallEffect, "{i}" := data[[i]][1])
  }
  
  dat_overallEffect <- dat_overallEffect %>%
    mutate(effect = predict(object = model_covariate, newdata = .,
                            type = "terms", terms = "te(period,age)"),
           cohort = period - age)
  dat_covariateAgeEffect <- dat_overallEffect %>%
    group_by(age) %>% summarize(effect = mean(effect)) %>% ungroup() %>%
    mutate(exp_effect = exp(effect - mean(effect)))
  dat_covariatePeriodEffect <- dat_overallEffect %>%
    group_by(period) %>% summarize(effect = mean(effect)) %>% ungroup() %>%
    mutate(exp_effect = exp(effect - mean(effect))) 
  dat_covariateCohortEffect <- dat_overallEffect %>%
    group_by(cohort) %>% summarize(effect = mean(effect)) %>% ungroup() %>%
    mutate(exp_effect = exp(effect - mean(effect)))
  
  # Joining of effects of both model types into a data.frame:
  dat_pureAgeEffect <- dat_pureAgeEffect %>%
    mutate(model = "Pure APC model")
  dat_purePeriodEffect <- dat_purePeriodEffect %>%
    mutate(model = "Pure APC model")
  dat_pureCohortEffect <- dat_pureCohortEffect %>%
    mutate(model = "Pure APC model")
  dat_overallAgeEffect <- dat_covariateAgeEffect %>%
    mutate(model = "Covariate model") %>% dplyr::bind_rows(dat_pureAgeEffect)
  dat_overallPeriodEffect <- dat_covariatePeriodEffect %>%
    mutate(model = "Covariate model") %>% dplyr::bind_rows(dat_purePeriodEffect)
  dat_overallCohortEffect <- dat_covariateCohortEffect %>%
    mutate(model = "Covariate model") %>% dplyr::bind_rows(dat_pureCohortEffect)
  
  # Actual plotting:
  ylim <- c(0.1,12)
  theme <- theme_minimal() +
    theme(text = element_text(size = 16), axis.title = element_text(size = 16),
          axis.text = element_text(size = 15),
          legend.text = element_text(size = 14),
          plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
          strip.text.y = element_text(size = 16),
          strip.placement = "outside", strip.background = element_blank(),
          axis.title.y = element_text(margin = margin(0, 10, 0, 0)),
          axis.title.x = element_text(margin = margin(10, 0, 0, 0)))
  # Age plot:
  gg_age <- ggplot(data = dat_overallAgeEffect,
                   mapping = aes(x = age, y = exp_effect, lty = model)) +
    geom_hline(yintercept = 1, col = "red") +
    geom_line() +
    xlab("Age") +
    scale_y_continuous("Exp effect",
                       trans  = "log",
                       breaks = c(0.125, 0.25, 0.5, 1, 2, 4, 8),
                       labels = c(0.125, 0.25, 0.5, 1, 2, 4, 8),
                       limits = ylim) +
    theme + theme(legend.position = "bottom", legend.title = element_blank())
  # Period plot:
  gg_period <- ggplot(data = dat_overallPeriodEffect,
                      mapping = aes(x = period, y = exp_effect, lty = model)) +
    geom_hline(yintercept = 1, col = "red") +
    geom_line() +
    xlab("Period") +
    scale_y_continuous(trans  = "log",
                       breaks = c(0.125, 0.25, 0.5, 1, 2, 4, 8),
                       labels = c(0.125, 0.25, 0.5, 1, 2, 4, 8),
                       limits = ylim) +
    theme + theme(axis.text.y = element_blank()) + ylab(" ") +
    theme(legend.position = "bottom", legend.title = element_blank()) 
  # Cohort plot:
  dat_th <- data.frame(variable = "Cohort",
                       threshold = c(1938.5, 1946.5, 1966.5, 1982.5, 1994.5))
  gg_cohort <- ggplot(data = dat_overallCohortEffect,
                      mapping = aes(x = cohort, y = exp_effect, lty = model)) +
    geom_hline(yintercept = 1, col = "red") +
    geom_vline(data = dat_th, aes(xintercept = threshold, class = variable),
               lty = 2, col = gray(0.75)) + 
    geom_line() +
    xlab("Cohort") +
    scale_y_continuous(trans  = "log",
                       breaks = c(0.125, 0.25, 0.5, 1, 2, 4, 8),
                       labels = c(0.125, 0.25, 0.5, 1, 2, 4, 8),
                       limits = ylim) +
    theme + theme(axis.text.y = element_blank()) + ylab(" ") +
    theme(legend.position = "bottom", legend.title = element_blank())
  # Resulting layout:
  plots <- ggpubr::ggarrange(gg_age, gg_period, gg_cohort, nrow = 1,
                             legend = "bottom", common.legend = TRUE) 
  return(plots)
}

################################################################################


