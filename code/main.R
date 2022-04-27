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
                                age>39&age<65 ~ '40-65',
                                age>64&age<75 ~ '65-75')) %>% 
  filter(!is.na(age_groups),
         !is.na(weight),
         !is.na(height)) %>% 
  select(-age)

hse17_p <- hse17 %>%
  select(Age35g,Weight,Height) %>% 
  mutate(year = '2017') %>% 
  mutate(age_groups = case_when(Age35g %in% c(8,9,10,11) ~ '20-40',
                                Age35g %in% c(12,13,14,15,16) ~ '40-65',
                                Age35g %in% c(17,18) ~ '65-75')) %>% 
  filter(!is.na(age_groups),
         !is.na(Weight),
         !is.na(Height)) %>% 
  rename(weight = Weight,
         height = Height) %>% 
  select(-Age35g)

hse_percentile <- bind_rows(hse07_p,hse17_p) %>% 
  mutate(BMI = weight/(height/100)^2) %>% 
  filter(BMI>18.5) %>% 
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
  pivot_longer(cols = percent05:percent95) %>% 
  mutate(percentile = parse_number(name)) %>% 
  select(-name)

#############################
# SEC 3 - ANALYSIS #
####################

hse_percentile %>% ggplot(aes(percentile,value,col = year)) +
  geom_line() +
  facet_wrap(~age_groups) +
  labs(title = 'Weight gain from 2007 - 2017',
       y = 'Weight (kg)',
       caption = 'source: HSE') +
  scale_y_continuous(limits = c(0,130))

hse_percentile %>% pivot_wider(names_from = year,
                               values_from = value) %>% 
  mutate(change = `2017` - `2007`) %>% 
  ggplot(aes(percentile,change)) +
  geom_line() +
  facet_wrap(~age_groups) +
  labs(title = 'Weight gain from 2007 - 2017',
       y = 'Weight (kg)',
       caption = 'source: HSE') 

hse_percentile %>% pivot_wider(names_from = year,
                               values_from = value) %>% 
  mutate(change = `2017` - `2007`) %>% 
  select(-`2007`,-`2017`) %>% 
  filter(percentile %in% c(10,25,50,75,90))
