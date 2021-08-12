library(haven)
library(tidyverse)

#########################
# SEC 1 - READ IN #
########################

hse07 <- read_sav("data/hse07ai.sav")

hse17 <- read_sav("data/hse17i_eul_v1.sav")

#############################
# SEC 2 - DATA WRANGELLING #
############################

hse07_p <- hse07 %>% 
  select(age,weight,height) %>% 
  mutate(year = '2007') %>% 
  mutate(age_groups = case_when(age>19&age<40 ~ '20-40',
                                age>39&age<60 ~ '40-60',
                                age>59 ~ '60+')) %>% 
  filter(!is.na(age_groups),
         !is.na(weight),
         !is.na(height)) %>% 
  select(-age)

hse17_p <- hse17 %>%
  select(Age35g,Weight,Height) %>% 
  mutate(year = '2017') %>% 
  mutate(age_groups = case_when(Age35g %in% c(8,9,10,11) ~ '20-40',
                                Age35g %in% c(12,13,14,15) ~ '40-60',
                                Age35g>15 ~ '60+')) %>% 
  filter(!is.na(age_groups),
         !is.na(Weight),
         !is.na(Height)) %>% 
  rename(weight = Weight,
         height = Height) %>% 
  select(-Age35g)

hse_percentile <- bind_rows(hse07_p,hse17_p) %>% 
  group_by(year,age_groups) %>%
  summarise(percent05 = quantile(weight, probs = .05),
            percent10 = quantile(weight, probs = .1),
            percent15 = quantile(weight, probs = .15),
            percent20 = quantile(weight, probs = .2),
            percent25 = quantile(weight, probs = .25),
            percent30 = quantile(weight, probs = .3),
            percent35 = quantile(weight, probs = .35),
            percent40 = quantile(weight, probs = .4),
            percent45 = quantile(weight, probs = .45),
            percent50 = quantile(weight, probs = .5),
            percent55 = quantile(weight, probs = .55),
            percent60 = quantile(weight, probs = .6),
            percent65 = quantile(weight, probs = .65),
            percent70 = quantile(weight, probs = .7),
            percent75 = quantile(weight, probs = .75),
            percent80 = quantile(weight, probs = .8),
            percent85 = quantile(weight, probs = .85),
            percent90 = quantile(weight, probs = .9),
            percent95 = quantile(weight, probs = .95)) %>% 
  pivot_longer(cols = percent05:percent95) 

#############################
# SEC 3 - ANALYSIS #
####################

