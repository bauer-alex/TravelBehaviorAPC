
library(dplyr)
library(ggplot2)

theme_set(theme_minimal())


# face-to-face data -------------------------------------------------------
dat <- readRDS("~/LRZ Sync+Share/TourIST_neu/TourIST_2021-03-07T0005/Daten/Daten_aufbereitet/210727_dat_ftf.rds")

# restrict to persons between 14 and 70 years, in analogy to the online data
# (even though for 2019 the online data also comprises age values 71-75, but
#  earlier years don't)
dat <- dat %>%
  filter(S_Alter >= 14 & S_Alter <= 70)

# plot of the KUR travel participation
dat %>% 
  filter(RH_AnzKUR_VJ != -99, !is.na(RH_AnzKUR_VJ)) %>% 
  group_by(travel_year) %>% 
  summarize(travel_participation = sum(RH_AnzKUR_VJ >= 1) / n()) %>% 
  ggplot(aes(travel_year, travel_participation)) +
  geom_line() +
  geom_vline(xintercept = 2011, lty = 2)
# ggsave("211216_KURparticipation_faceToFaceData.pdf", width = 6, height = 3.5)



# online data -------------------------------------------------------------
dat_online <- readRDS("~/LRZ Sync+Share/TourIST_neu/TourIST_2021-03-07T0005/Daten/Daten_aufbereitet/200819_dat_online_person.rds")

# plot of the KUR travel participation
dat_online %>% 
  mutate(x = as.numeric(as.character(RH_Reisehaeufigkeit_Urlaubsreise_2bis4Tage))) %>% 
  group_by(travel_year) %>% 
  summarize(travel_participation = sum(x) / n()) %>% 
  ggplot(aes(travel_year, travel_participation)) +
  geom_line() +
  scale_x_continuous(breaks = sort(unique(dat_online$travel_year)))
# ggsave("211216_KURparticipation_onlineData.pdf", width = 6, height = 3.5)
