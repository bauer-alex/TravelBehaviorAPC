#' Read the dataset and prepare it for estimating a specific model
#' 
#' @param model One of \code{c("participation","frequency","expenses",
#' "participation_east", "frequency_east", "expenses_east",
#' "expenses_sensitivity)}.
#' 
#' @import checkmate dplyr
#' 
read_and_prepare_data <- function(model = NULL) {
  
  checkmate::assert_choice(model,
                           choices = c("participation","frequency","expenses",
                                       "participation_east", "frequency_east",
                                       "expenses_east",
                                       "expenses_sensitivity"))
  
  
  ### 1. read the data
  dat <- readRDS("Data/data.rds")
  
  
  ### 2. only use relevant variables
  dat <- dat %>% select(travel_year, S_Geschlecht, S_Alter, S_Hauptverdiener,
                        S_Herkunft, S_Staatsangehoerigkeit,
                        S_Bildung, S_Wohnortgroesse, S_Kinder_0_bis_5_binaer,
                        S_Einkommen_HH, S_Haushaltsgroesse, S_Haushaltsgroesse_14plus,
                        JS_HUR_Ausgaben_gesamt, JS_HUR_Reisebegleitung_HH,
                        JS_HUR_Reisebegleitung_Gesamtanzahl, JS_HUR_Reisedauer,
                        JS_Gesamt_Ausgaben, JS_Anzahl_URs,
                        S_BerufAktuell, S_BerufAktuell_HV)
  
  
  ### 3. only use persons between 18 and 80 years of age, as younger and older
  ###    people are not completely free in their decision if and how to travel
  dat <- dat %>% 
    filter(S_Alter >= 18 & S_Alter <= 80)
  
  
  ### 4. recode 'keine Angabe' and '-99' values to NA
  dat <- dat %>% 
    mutate_all(~ na_if(., "keine Angabe")) %>% 
    mutate_all(~ na_if(., "-99"))
  
  
  ### 5. recode 'mehr als 5 Personen' in household size to 5.3 persons.
  ###    Reasoning:
  ###    5.3 is the average number of persons (overall, but also only regarding
  ###    persons with 14+ years of age) in households with 5+ persons for the
  ###    travel year 2000 (variables 's9a' and 's9b')
  dat <- dat %>% 
    mutate(S_Haushaltsgroesse        = as.numeric(substr(S_Haushaltsgroesse, 1, 1)),
           S_Haushaltsgroesse_14plus = as.numeric(substr(S_Haushaltsgroesse_14plus, 1, 1))) %>% 
    mutate(S_Haushaltsgroesse        = case_when(S_Haushaltsgroesse == 5 ~ 5.3,
                                                 TRUE                    ~ S_Haushaltsgroesse),
           S_Haushaltsgroesse_14plus = case_when(S_Haushaltsgroesse_14plus == 5 ~ 5.3,
                                                 TRUE                                  ~ S_Haushaltsgroesse_14plus))
  
  
  ### 6. compute the number of children with <14 years of age in the household
  dat <- dat %>% 
    mutate(S_Haushaltsgroesse_Kinder = S_Haushaltsgroesse - S_Haushaltsgroesse_14plus)
  
  
  ### 7. compute the 'effective of number of fully paying persons' in the
  ###    household based on the OECD-modified equivalence scale.
  ###    The scale is used to account for synergy effects when multiple people
  ###    jointly run one household:
  ###    - the first adult (with 14+ years) counts as 1 person
  ###    - every further adult counts as 0.5 persons
  ###    - every child (<14 years) counts as 0.3 persons
  dat <- dat %>% 
    mutate(S_Haushaltsgroesse_equi = 1 + 0.5 * (S_Haushaltsgroesse_14plus - 1) +
             0.3 * S_Haushaltsgroesse_Kinder)
  
  
  ### 8. compute the 'household income per effective person'
  dat <- dat %>% 
    mutate(S_Einkommen_HH_equi = S_Einkommen_HH / S_Haushaltsgroesse_equi)
  
  ### 9. compute the 'travel expenses per effective housemate that joined the trip':
  ###    1. The information how many people from the household jointly traveled
  ###       with the respondent is saved in variable 'JS_HUR_Reisebegleitung_HH'.
  ###    2. Problem:  The number of these people is sometimes larger than the
  ###                 number of actual housemates living in the household.
  ###       Solution: When this is the case we set the number of 'people from the
  ###                 household who jointly traveled with the respondent' to the
  ###                 number of housemates living in the household.
  dat <- dat %>% 
    mutate(JS_HUR_Reisebegleitung_HH = case_when(JS_HUR_Reisebegleitung_HH > S_Haushaltsgroesse ~ S_Haushaltsgroesse,
                                                 TRUE ~ JS_HUR_Reisebegleitung_HH))
  ###    3. Compute the effective number of 'housemates the joined the trip':
  ###       - if the number of 'housemates that joined the trip' equals the
  ###         number of 'housemates in the household':
  ###         in this case the overall equivalence score of the household is used
  ###         as the effective number
  ###       - if the number of 'housemates that joined the trip' is smaller than
  ###         the number of 'housemates in the household':
  ###         in this case the effective number is calculated as
  ###         1 (reasoning: there's always at least one adult from the household) + k * mean_equi,
  ###         with 'k' the 'number of housemates that joined the trip minus one'
  ###         and 'mean_equi' the 'mean of the <overall equivalence score of the household minus 1>'
  ###         (i.e. when excluding one adult person).
  dat <- dat %>% 
    mutate(JS_HUR_Reisebegleitung_HH_equi = case_when(JS_HUR_Reisebegleitung_HH == S_Haushaltsgroesse ~ S_Haushaltsgroesse_equi,
                                                      TRUE ~ 1 + (JS_HUR_Reisebegleitung_HH - 1) * ((S_Haushaltsgroesse_equi - 1) / (S_Haushaltsgroesse - 1))))
  ###    4. compute the person-adjusted travel expenses of the household
  dat <- dat %>% 
    mutate(JS_HUR_Ausgaben_gesamt_equi = JS_HUR_Ausgaben_gesamt / JS_HUR_Reisebegleitung_HH_equi,
           JS_Ausgaben_gesamt_equi = JS_Gesamt_Ausgaben / JS_HUR_Reisebegleitung_HH_equi)
  
  
  ### 10. calculate the person-adjusted travel expenses per person-adjusted income
  dat <- dat %>% 
    mutate(rel_expenses = JS_HUR_Ausgaben_gesamt_equi / S_Einkommen_HH_equi)
  
  
  ### 11. create some variables for the number of performed Urlaubsreisen
  dat <- dat %>%
    mutate(y_atLeastOneUR = as.factor(if_else(condition = JS_Anzahl_URs == 0, true = 0,
                                              false = 1)),
           y_atLeastTwoURs = as.factor(if_else(JS_Anzahl_URs > 1, 1, 0)))
  
  
  ### 12. rename the time variables to 'period' and 'age' and calculate the cohort
  dat <- dat %>%
    dplyr::rename(period = travel_year, age = S_Alter) %>% 
    mutate(cohort = period - age)
  
  
  ### 13. only analyze data since 1983 as the travel expense data cannot be
  ###     trusted before that year.
  ###     Also remove 1986 since the income was unrealistically low in that year.
  dat <- dat %>% filter(period >= 1983 & period != 1986)
  
  
  ### 14. restrict data to Western Germans with German citizenship
  ### moved to specific model types
  #dat <- dat %>%
  #  filter(is.na(S_Herkunft) | S_Herkunft == "West",
  #         is.na(S_Staatsangehoerigkeit) | S_Staatsangehoerigkeit == "Deutsch")
  
  
  ### 15. factorize the household size and drop old factor levels of categorical
  ###     variables
  dat <- dat %>% mutate(S_Haushaltsgroesse = factor(S_Haushaltsgroesse))
  vars_cat <- c("S_Geschlecht", "S_Bildung", "S_Kinder_0_bis_5_binaer",
                "S_Wohnortgroesse", "S_Haushaltsgroesse")
  dat <- dat %>% mutate(across(all_of(vars_cat), ~ droplevels(.)))
  
  
  ### 16. winsorize household income per effective person:
  #dat <- dat %>%
  #  mutate(S_Einkommen_HH_equi = if_else(S_Einkommen_HH_equi > 6000, 6000,
  #                                       S_Einkommen_HH_equi))
  
  ### 17. create a column containing the name of the person's generation
  generations <- c("Generations before 1939","Silent Generation","Baby Boomer",
                   "Generation X","Generation Y","Generation Z")
  dat <- dat %>% 
    mutate(generation = case_when(cohort <= 1938 ~ generations[1],
                                  cohort <= 1946 ~ generations[2],
                                  cohort <= 1966 ~ generations[3],
                                  cohort <= 1982 ~ generations[4],
                                  cohort <= 1994 ~ generations[5],
                                  cohort <= 2010 ~ generations[6])) %>% 
    mutate(generation = factor(generation, levels = generations))
  
  ### 18. calculate new variable of occupation (if S_BerufAktuell = nicht
  ###     beschaeftigt, S_BerufAktuell_HV)
  ###     Not used in the end, because of the multicollinearity with age
  ###     (most 18/19 year-olds are still in education, and nearly all
  ###      65+ year-olds are in retirement)
  dat <- dat %>%
    mutate(S_BerufAktuell = as.character(S_BerufAktuell)) %>%
    mutate(S_BerufAktuell = if_else(S_BerufAktuell == "nicht berufstaetig" &
                                      period != 1991,
                                    as.character(S_BerufAktuell_HV),
                                    S_BerufAktuell)) %>%
    mutate(S_BerufAktuell = case_when(S_BerufAktuell == "stundenweise" ~
                                        "halbtaegig",
                                      #S_BerufAktuell == "halbtaegig" ~
                                      #  "ganztaegig",
                                      #S_BerufAktuell == "nicht beschaeftigt" ~
                                      #  "ganztaegig",
                                      TRUE ~ S_BerufAktuell))
  
  
  
  ### Last step: Final, model-specific preparations
  if (model %in% c("participation")) {
    
    # 1. restrict data to Western Germans with German citizenship
    dat <- dat %>%
      filter(is.na(S_Herkunft) | S_Herkunft == "West",
             is.na(S_Staatsangehoerigkeit) | S_Staatsangehoerigkeit == "Deutsch")
    
  } else if (model == "frequency") {
    
    # 1. restrict data to Western Germans with German citizenship
    dat <- dat %>%
      filter(is.na(S_Herkunft) | S_Herkunft == "West",
             is.na(S_Staatsangehoerigkeit) | S_Staatsangehoerigkeit == "Deutsch")
    
    # 2. restrict data to people with at least one trip
    dat <- dat %>% filter(JS_Anzahl_URs > 0)
    
  } else if (model == "expenses") {
    
    # 1. restrict data to Western Germans with German citizenship
    dat <- dat %>%
      filter(is.na(S_Herkunft) | S_Herkunft == "West",
             is.na(S_Staatsangehoerigkeit) | S_Staatsangehoerigkeit == "Deutsch")
    
    # 2. restrict data to people with information on the travel expenses,
    #    i.e., exclude non-travelers
    dat <- dat %>% filter(!is.na(JS_HUR_Ausgaben_gesamt))
    
    # 3. exclude people where the person-adjusted household income is zero
    dat <- dat %>% filter(S_Einkommen_HH_equi > 0)
  }
  
  else if (model == "expenses_sensitivity") {
    
    # 1. restrict data to Western Germans with German citizenship
    dat <- dat %>%
      filter(is.na(S_Herkunft) | S_Herkunft == "West",
             is.na(S_Staatsangehoerigkeit) | S_Staatsangehoerigkeit == "Deutsch")
    
    # 2. restrict data to years where detailed information about twelve trips
    #    are available
    dat <- dat %>% filter(period %in% 1995:2018) %>%
      filter(!is.na(JS_Gesamt_Ausgaben))
    
    # 3. calculate relative expenses based on expenses for all trips
    dat <- dat %>%
      mutate(rel_expenses = JS_Ausgaben_gesamt_equi / S_Einkommen_HH_equi)
    
  }
  
  else if (model == "participation_east") {
    
    # 1. restrict data to Eastern Germans with German citizenship
    dat <- dat %>%
      filter(S_Herkunft == "Ost",
             is.na(S_Staatsangehoerigkeit) | S_Staatsangehoerigkeit == "Deutsch")
    
  }
  
  else if (model == "frequency_east") {
    
    # 1. restrict data to Eastern Germans with German citizenship
    dat <- dat %>%
      filter(S_Herkunft == "Ost",
             is.na(S_Staatsangehoerigkeit) | S_Staatsangehoerigkeit == "Deutsch")
    
    # 2. restrict data to people with at least one trip
    dat <- dat %>% filter(JS_Anzahl_URs > 0)
    
  }
  
  else if (model == "expenses_east") {
    
    # 1. restrict data to Western Germans with German citizenship
    dat <- dat %>%
      filter(S_Herkunft == "Ost",
             is.na(S_Staatsangehoerigkeit) | S_Staatsangehoerigkeit == "Deutsch")
    
    # 2. restrict data to people with information on the travel expenses,
    #    i.e., exclude non-travelers
    dat <- dat %>% filter(!is.na(JS_HUR_Ausgaben_gesamt))
    
    # 3. exclude people where the person-adjusted household income is zero
    dat <- dat %>% filter(S_Einkommen_HH_equi > 0)
  }
  
  return(dat)
}







