#### Preamble ####
# Purpose:Prediction of proportion of voters will vote Joe Biden in 2020 U.S.Election 
# Author: Shihan Wang 1005165063 Wenyu Shu 1004951082 Kefan Cai 1004819949
# Data: Nov.1st, 2020
# Contact: Shihan.wang@mail.utoronto.ca   Wenyu Shu@mail.utoronto.ca 
#          Kefan Cai@mail.utoronto.ca
# License:MIT 
#         Samantha-Jo Caetano

library(haven)
library(tidyverse)

setwd("/Users/helen/Desktop/ps3")
raw_data <- read_dta("usa_00005.dta.gz")


raw_data <- labelled::to_factor(raw_data)


#select interesting data
reduced_data <- 
  raw_data %>% 
  select(sex,
         age,
         race,
         empstat) 

#clean age data
reduced_data$age <- as.integer(reduced_data$age)
reduced_data <- 
  reduced_data %>% 
  filter(age != "less than 1 year old") %>%
  filter(age >= 18) %>%
  filter(age != "90 (90+ in 1980 and 1990)")

reduced_data <- reduced_data %>%
  mutate(age = as.character(age))%>%
  mutate(age_group = case_when(age <= '24' ~ 'youth',
                               '59' < age  ~'senior',
                               age >= '25'~ 'adult'))

#clean race data
reduced_data <- reduced_data %>% 
  mutate(race = as.character(race)) %>%
  mutate(race = case_when(race == "white" ~ "white",
                          race == "black/african american/negro" ~ "black/african american/negro",
                          race == "american indian or alaska native" ~ "american indian or alaska native",
                          race == "other race, nec" ~ "other races or mixed-blood",
                          race == "chinese" ~ "chinese",
                          race == "japanese" ~ "japanese",
                          race == "other asian or pacific islander" ~ "other asian or pacific islander",
                          race == "two major races" ~ "other races or mixed-blood",
                          race == "three or more major races" ~ "other races or mixed-blood"))


#clean sex data
reduced_data <- 
  reduced_data %>%
  count(sex,race,age_group,empstat) %>%
  group_by(sex,race,age_group,empstat) 

#clean empstat data
reduced_data<-
  reduced_data %>%
  filter(empstat != "n/a")

names(reduced_data)[4] <- "employment"

#save as csv
write_csv(reduced_data, "census_data.csv")



