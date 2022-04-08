## Education and Health Data

# Import packages
library(tidyverse)
library(haven)
library(dplyr)
library(ggplot2)
library(corrplot)
library("ggcorrplot")    


# LOAD DATA
General_Education <- read_dta('Raw_data/glss4_new/glss4_new/sec2a.dta') # General Education file
Education_career <- read_dta('Raw_data/glss4_new/glss4_new/sec2b.dta') # Education Career file
Literacy_Apprenticeship <- read_dta('Raw_data/glss4_new/glss4_new/sec2c.dta') # Literacy /Apprenticeship file

Fertility_Prenatal_care_Contraceptive <- read_dta('Raw_data/glss4_new/glss4_new/sec3d.dta') # Fertility, Prenatal care, and Contraceptive use file
Preventive_health_Vaccination_past_12_months <- read_dta('Raw_data/glss4_new/glss4_new/sec3b.dta') # Preventive health, Vaccination, in the past 12 months file
postnatal_care <- read_dta('Raw_data/glss4_new/glss4_new/sec3c.dta') # postnatal care filw
Health_conditions <- read_dta('Raw_data/glss4_new/glss4_new/sec3a.dta') # Health condition in the past two weeks file


## EDUCATION VARIABLES ##

# Highest level of education completed per household
Educational_attainment  <- General_Education %>% 
  select(nh, clust, s2aq2) %>% 
  group_by(nh, clust) %>%
  summarise('Education_Attainment' = max(s2aq2))


# Get total number of people in household
f_pid <-  General_Education %>% select(nh, clust, pid) %>% 
  group_by(clust, nh) %>% 
  summarise(nh_num_people = max(pid))


# Max years of apprenticeship completed per household
Years_in_apprenticeship <- Literacy_Apprenticeship %>% 
  select(nh, clust, s2cq8a) %>% 
  group_by(nh, clust) %>%
  summarise('Years_in_apprenticeship' = max(s2cq8a)) 


# Can someone in the household read English (1-no, 2-yes)
Read_english <- Literacy_Apprenticeship %>% 
  select(nh, clust, s2cq1) %>% 
  group_by(nh, clust) %>%
  summarise('Read_English' = max(s2cq1))


# Can someone in the household write in English (1-no, 2-yes)
write_english <- Literacy_Apprenticeship %>% 
  select(nh, clust, s2cq3) %>% 
  group_by(nh, clust) %>%
  summarise('write_English' = max(s2cq3))


# Joining all education variables into one data set 
Education_joined <- Educational_attainment %>%
  full_join(Years_in_apprenticeship, by = c("clust", "nh")) %>% 
  full_join(Read_english, by = c("clust", "nh")) %>% 
  full_join(write_english, by = c("clust", "nh")) %>% 
  full_join(f_pid, by = c("clust", "nh"))

## HEALTH VARIABLES ##

# Number of children born in each household  #####!!!!!!
Number_of_children_born <- Fertility_Prenatal_care_Contraceptive %>% 
  select(nh, clust, s3dq5)
  #rename(Total_Number_of_children_born = s3dq5) 

Number_of_children_born <- aggregate(x= Number_of_children_born$s3dq5,
                                     by= list(Number_of_children_born$clust, 
                                              Number_of_children_born$nh),
                                     FUN=max)
colnames(Number_of_children_born) <- c("clust", "nh", "Total_Number_of_children_born")

# Is someone in the household vaccinated (1-no, 2-yes)
Vacination_status <- Preventive_health_Vaccination_past_12_months %>% 
  select(nh, clust, s3bq1) %>% 
  group_by(nh, clust) %>%
  summarise('Vaccination_Status' = max(s3bq1))

View(Health_conditions)
# Has someone in the household suffered injury or illness in the last two weeks (1-no, 2-yes)
Health_conditions_in_last_two_weeks <- Health_conditions %>% 
  select(nh, clust, s3aq1) %>% 
  group_by(nh, clust) %>%
  summarise(Health_Conditions_in_last_two_weeks = max(s3aq1)) %>% 
  mutate(b_Health_conditions_in_last_two_weeks = ifelse(Health_Conditions_in_last_two_weeks == 1, 1, 0)) %>% 
  select(nh, clust, b_Health_conditions_in_last_two_weeks)

# Amount paid for medicine per household
Amount_paid_for_medicine <- Health_conditions %>% 
  select(nh, clust, s3aq18) %>% 
  group_by(nh, clust) %>%
  summarise('Amount_paid_for_medicine' = sum(s3aq18))


# Joining all health variables into one data set 
Health_joined <- Number_of_children_born %>%
  full_join(Vacination_status, by = c("clust", "nh")) %>% 
  full_join(Health_conditions_in_last_two_weeks, by = c("clust", "nh")) %>% 
  full_join(Amount_paid_for_medicine  , by = c("clust", "nh"))

View(Health_joined)
# Joining all education and health variables into one data set
Education_health_joined <- Education_joined %>% 
  full_join(Health_joined, by = c("clust", "nh")) %>% 
  mutate(across('Education_Attainment':'Amount_paid_for_medicine', ~replace_na(., 0)))
