#### Preamble ####
# Purpose: Prediction of proportion of voters will vote Joe Biden in 2020 U.S.Election 
# Author: Shihan Wang 1005165063 Wenyu Shu 1004951082 Kefan Cai 1004819949
# Data: Nov.1st, 2020
# Contact: Shihan.wang@mail.utoronto.ca   Wenyu Shu@mail.utoronto.ca 
#          Kefan Cai@mail.utoronto.ca
# License:MIT 
#         Samantha-Jo Caetano

library(haven)
library(tidyverse)
setwd("/Users/helen/Desktop/ps3")

raw_data <- read_dta("ns20200625/ns20200625.dta")

raw_data <- labelled::to_factor(raw_data)

#select interesting data
reduced_data <- 
  raw_data %>% 
  select(employment,
         gender,
         race_ethnicity,
         age,
         vote_2020)

reduced_data<-
  reduced_data %>%
  mutate(vote_Biden = 
           ifelse(vote_2020=="Joe Biden", 1, 0))


#clean race data
reduced_data <- reduced_data %>% 
  mutate(race_ethnicity = as.character(race_ethnicity)) %>%
  mutate(race = case_when(race_ethnicity == "Asian (Asian Indian)" ~ "other asian or pacific islander",
                          race_ethnicity == "Asian (Vietnamese)" ~ "other asian or pacific islander",
                          race_ethnicity == "Asian (Chinese)" ~ "chinese",
                          race_ethnicity == "Asian (Korean)" ~ "other asian or pacific islander",
                          race_ethnicity == "Asian (Japanese)" ~ "japanese",
                          race_ethnicity == "Asian (Other)" ~ "other asian or pacific islander",
                          race_ethnicity == "Asian (Filipino)" ~ "other asian or pacific islander",
                          race_ethnicity == "Pacific Islander (Native Hawaiian)" ~ "other asian or pacific islander",
                          race_ethnicity == "Pacific Islander (Other)" ~ "other asian or pacific islander",
                          race_ethnicity == "Pacific Islander (Guamanian)" ~ "other asian or pacific islander",
                          race_ethnicity == "Pacific Islander (Samoan)" ~ "other asian or pacific islander",
                          race_ethnicity == "White" ~ "white",
                          race_ethnicity == "Black, or African American" ~ "black/african american/negro",
                          race_ethnicity == "American Indian or Alaska Native" ~ "american indian or alaska native",
                          race_ethnicity == "Some other race" ~ "other races or mixed-blood"))

#clean gender data
reduced_data <- reduced_data %>% 
  mutate(gender= as.character(gender)) %>%
  mutate( sex = case_when(gender == "Male" ~ "male",
                          gender == "Female" ~ "female",))

#clean age data
reduced_data <- reduced_data %>%
  mutate(age = as.character(age))%>%
  mutate(age_group = case_when(age <= '24' ~ 'youth',
                               '59' < age  ~'senior',
                               age >= '25'~ 'adult'))
#clean employment data
reduced_data <- reduced_data %>%
  mutate(employment = as.character(employment))%>%
  mutate(employment = case_when(employment == 'Full-time employed'~'employed',
                                employment == 'Unemployed or temporarily on layoff'~'unemployed',
                                employment == 'Retired'~'not in labor force',
                                employment == 'Student'~'not in labor force',
                                employment == 'Homemaker'~'employed',
                                employment == 'Part-time employed'~'employed',
                                employment == 'Self-employed'~'employed',
                                employment == 'Permanently disabled'~'not in labor force',
                                employment == 'Other:'~'not in labor force'))
reduced_data<-
  reduced_data %>%
  filter(employment != "n/a")


#save as csv
write_csv(reduced_data, "survey_data.csv")

