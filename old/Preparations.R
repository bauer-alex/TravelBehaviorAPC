
dat[,S_Jahrgang := travel_year - S_Alter]
alle_jahrgaenge <- min(dat$S_Jahrgang, na.rm = TRUE):max(dat$S_Jahrgang, na.rm = TRUE)
dat <- add_row(dat, S_Jahrgang = setdiff(alle_jahrgaenge, unique(dat$S_Jahrgang)))

dat[JS_Anzahl_URs > 3, AnzUR_new := 3]
dat[JS_Anzahl_URs <= 3, AnzUR_new := JS_Anzahl_URs]

dat[JS_HUR_Verkehrsmittel %in% "Flugzeug" |
      JS_UR2_Verkehrsmittel %in% "Flugzeug" | 
      JS_UR3_Verkehrsmittel %in% "Flugzeug", Flugreise := 1]

dat[is.na(Flugreise), Flugreise := 0]

dat[is.na(JS_HUR_Verkehrsmittel) &
      is.na(JS_UR2_Verkehrsmittel) &
      is.na(JS_UR2_Verkehrsmittel), Flugreise := NA]

dat <- dat %>% mutate(Flugreisen_Anzahl = ifelse(JS_HUR_Verkehrsmittel %in% "Flugzeug", 1, 0) +
                        ifelse(JS_UR2_Verkehrsmittel %in% "Flugzeug", 1, 0) +
                        ifelse(JS_UR3_Verkehrsmittel %in% "Flugzeug", 1, 0))

dat[, Flugreisen_Anzahl := ifelse(JS_HUR_Verkehrsmittel %in% "Flugzeug", 1, 0) +
      ifelse(JS_UR2_Verkehrsmittel %in% "Flugzeug", 1, 0) +
      ifelse(JS_UR3_Verkehrsmittel %in% "Flugzeug", 1, 0)]


dat[JS_Anzahl_URs == 0, Flugreise_Anteil := 99]
dat[JS_Anzahl_URs != 0, Flugreise_Anteil := Flugreisen_Anzahl/AnzUR_new]


dat[, Verkehrsmittel_Anzahl := ifelse(!is.na(JS_HUR_Verkehrsmittel), 1, 0) +
      ifelse(!is.na(JS_UR2_Verkehrsmittel), 1, 0) +
      ifelse(!is.na(JS_UR3_Verkehrsmittel), 1, 0)]

dat[Verkehrsmittel_Anzahl == 0, Flugreise_Anteil_Verkehrsmittel := 99]
dat[Verkehrsmittel_Anzahl > 0, Flugreise_Anteil_Verkehrsmittel := Flugreisen_Anzahl/Verkehrsmittel_Anzahl]


dat_share_period <- dat[,.(Flugreise_Anteil_Total = sum(Flugreisen_Anzahl) / sum(AnzUR_new)) , by = .(travel_year)]

dat_share_age <- dat[,.(Flugreise_Anteil_Total = sum(Flugreisen_Anzahl) / sum(AnzUR_new)) , by = .(S_Alter)]

dat_share_cohort <- dat[,.(Flugreise_Anteil_Total = sum(Flugreisen_Anzahl) / sum(AnzUR_new)) , by = .(S_Jahrgang)]



dat_share_period_verkehrsmittel <- dat[,.(Flugreise_Anteil_Total_Verkehrsmittel = sum(Flugreisen_Anzahl) / sum(Verkehrsmittel_Anzahl)),
                                       by = .(travel_year)]

dat_share_age_verkehrsmittel <- dat[,.(Flugreise_Anteil_Total_Verkehrsmittel = sum(Flugreisen_Anzahl) / sum(Verkehrsmittel_Anzahl)),
                                    by = .(S_Alter)]

dat_share_cohort_verkehrsmittel <- dat[,.(Flugreise_Anteil_Total_Verkehrsmittel = sum(Flugreisen_Anzahl) / sum(Verkehrsmittel_Anzahl)),
                                       by = .(S_Jahrgang)]
