# Census variables for PAP - NJ analysis

rm(list=ls())
library(tidycensus)
library(tidyverse)
library(stringr)
library(optparse)
library(googledrive)

load_all_variables <- function(year){
  
  # get all the variable data
  basic <- load_variables(year, 'acs5', cache=TRUE)
  basic$table <- 'basic'
  table_name <- sapply(basic$name, strsplit, split="_")
  
  data_profiles <- load_variables(year, 'acs5/profile', cache=TRUE)
  data_profiles$table <- 'data_profiles'
  
  survey <- load_variables(year, 'acs5/subject', cache=TRUE)
  survey$table <- 'survey'
  
  all <- rbind(basic, data_profiles)
  all <- rbind(all, survey)
  
  # remove extraneous colons in the label field
  all$label <- sapply(all$label, gsub, pattern=':', replacement='')
  
  # separate aggregate columns
  all <- separate(all, name, into=c('table_name'), sep='_', remove=FALSE)
  all$year <- year
  
  return(all)
}

v19 <- load_all_variables(2019)
v18 <- load_all_variables(2018)
v17 <- load_all_variables(2017)

## Total population for table B01001 (Sex by Age)

tot_pop <- 'B01001_001'
tot_19 <- v19 %>% filter(name==tot_pop)
tot_18 <- v18 %>% filter(name==tot_pop)
tot_17 <- v17 %>% filter(name==tot_pop)

acs_dictionary <- bind_rows(tot_19, tot_18, tot_17)

## Percent Black
## Will need to divide by total population

tot_black <- 'B01001B_001'
tot_black_19 <- v19 %>% filter(name==tot_black)
tot_black_18 <- v18 %>% filter(name==tot_black)
tot_black_17 <- v17 %>% filter(name==tot_black)

acs_dictionary <- bind_rows(acs_dictionary, tot_black_19, tot_black_17, tot_black_18)

## Percent White (not Hispanic or Latino)
## Will need to divide by total population

tot_white <- 'B01001A_001'
tot_white_19 <- v19 %>% filter(name==tot_white)
tot_white_18 <- v18 %>% filter(name==tot_white)
tot_white_17 <- v17 %>% filter(name==tot_white)

acs_dictionary <- bind_rows(acs_dictionary, tot_white_19, tot_white_18, tot_white_17)

## Percent Hispanic or Latino
## Will need to divide by total population

tot_hisp <- 'B01001I_001'
tot_hisp_19 <- v19 %>% filter(name==tot_hisp)
tot_hisp_18 <- v18 %>% filter(name==tot_hisp)
tot_hisp_17 <- v17 %>% filter(name==tot_hisp)

acs_dictionary <- bind_rows(acs_dictionary, tot_hisp_19, tot_hisp_18, tot_hisp_17)

## Percent of children living in poverty

pov <- "Estimate!!Total!!POVERTY STATUS IN THE PAST 12 MONTHS!!Children in households for whom poverty status is determined!!Income in the past 12 months below poverty level"
tot_kids <- "Estimate!!Total!!POVERTY STATUS IN THE PAST 12 MONTHS!!Children in households for whom poverty status is determined"

tot_kids_19 <- v19 %>% filter(label==tot_kids)
tot_kids_18 <- v18 %>% filter(label==tot_kids)
tot_kids_17 <- v17 %>% filter(label==tot_kids)

pov_19 <- v19 %>% filter(label==pov)
pov_18 <- v18 %>% filter(label==pov)
pov_17 <- v17 %>% filter(label==pov)

acs_dictionary <- bind_rows(acs_dictionary,
                            tot_kids_19, tot_kids_18, tot_kids_17,
                            pov_19, pov_18, pov_17)

## Married households: 
## - Percent married
## - Percent female head-of-household

tot_hh <- "Estimate!!Total" # in table 'B09002'
married <- "Estimate!!Total!!In married-couple families"
female_headed_19 <- "Estimate!!Total!!In other families!!Female householder, no spouse present"
female_headed_18 <- "Estimate!!Total!!In other families!!Female householder, no husband present"
female_headed_17 <- "Estimate!!Total!!In other families!!Female householder no husband present"

tot_hh_19 <- v19 %>% filter(label==tot_hh & table_name=='B09002')
tot_hh_18 <- v18 %>% filter(label==tot_hh & table_name=='B09002')
tot_hh_17 <- v17 %>% filter(label==tot_hh & table_name=='B09002')

married_19 <- v19 %>% filter(label==married)
married_18 <- v18 %>% filter(label==married)
married_17 <- v17 %>% filter(label==married)

female_19 <- v19 %>% filter(label==female_headed_19)
female_18 <- v18 %>% filter(label==female_headed_18)
female_17 <- v17 %>% filter(label==female_headed_17)

acs_dictionary <- bind_rows(acs_dictionary,
                            tot_hh_19, tot_hh_18, tot_hh_17,
                            married_19, married_18, married_17,
                            female_19, female_18, female_17)

## Educational attainment
## - no high school diploma
## - college-educated

over24 <- "Estimate!!Total!!EDUCATIONAL ATTAINMENT!!Population 25 years and over"
nohsdp <- "Estimate!!Total!!EDUCATIONAL ATTAINMENT!!Population 25 years and over!!Less than high school graduate"
assoc <- "Estimate!!Total!!EDUCATIONAL ATTAINMENT!!Population 25 years and over!!Some college or associate's degree"
bach <- "Estimate!!Total!!EDUCATIONAL ATTAINMENT!!Population 25 years and over!!Bachelor's degree"
grad <- "Estimate!!Total!!EDUCATIONAL ATTAINMENT!!Population 25 years and over!!Graduate or professional degree"

over24_19 <- v19 %>% filter(label==over24 & table_name=='S0601')
over24_18 <- v18 %>% filter(label==over24 & table_name=='S0601')
over24_17 <- v17 %>% filter(label==over24 & table_name=='S0601')

nohsdp_19 <- v19 %>% filter(label==nohsdp & table_name=='S0601')
nohsdp_18 <- v18 %>% filter(label==nohsdp & table_name=='S0601')
nohsdp_17 <- v17 %>% filter(label==nohsdp & table_name=='S0601')

acs_dictionary <- bind_rows(acs_dictionary,
                            over24_19, over24_18, over24_17,
                            nohsdp_19, nohsdp_18, nohsdp_17)

assoc_19 <- v19 %>% filter(label==assoc & table_name=='S0601')
assoc_18 <- v18 %>% filter(label==assoc & table_name=='S0601')
assoc_17 <- v17 %>% filter(label==assoc & table_name=='S0601')

bach_19 <- v19 %>% filter(label==bach & table_name=='S0601')
bach_18 <- v18 %>% filter(label==bach & table_name=='S0601')
bach_17 <- v17 %>% filter(label==bach & table_name=='S0601')

grad_19 <- v19 %>% filter(label==grad & table_name=='S0601')
grad_18 <- v18 %>% filter(label==grad & table_name=='S0601')
grad_17 <- v17 %>% filter(label==grad & table_name=='S0601')

acs_dictionary <- bind_rows(acs_dictionary,
                            assoc_19, assoc_18, assoc_17,
                            bach_19, bach_18, bach_17,
                            grad_19, grad_18, grad_17)

## Uninsured population (direct measure of percentage)

ep_unins <- 'S2701_C05_001'

ep_unins_19 <- v19 %>% filter(name==ep_unins)
ep_unins_18 <- v18 %>% filter(name==ep_unins)
ep_unins_17 <- v17 %>% filter(name==ep_unins)

acs_dictionary <- bind_rows(acs_dictionary, ep_unins_19, ep_unins_18, ep_unins_17)

## Public health insurance (direct measure of percentage)

pub_hlth <- "Estimate!!Percent Public Coverage!!COVERAGE ALONE!!Public health insurance alone"

ph_19 <- v19 %>% filter(label==pub_hlth)
ph_18 <- v18 %>% filter(label==pub_hlth)
ph_17 <- v17 %>% filter(label==pub_hlth)

acs_dictionary <- bind_rows(acs_dictionary, ph_19, ph_18, ph_17)

## Home ownership

tot_hu <- 'B25106_001'
owner <- 'B25106_002'

owner_19 <- v19 %>% filter(name==owner)
owner_18 <- v18 %>% filter(name==owner)
owner_17 <- v17 %>% filter(name==owner)

tot_hu_19 <- v19 %>% filter(name==tot_hu)
tot_hu_18 <- v18 %>% filter(name==tot_hu)
tot_hu_17 <- v17 %>% filter(name==tot_hu)

acs_dictionary <- bind_rows(acs_dictionary, owner_19, owner_18, owner_17, tot_hu_19, tot_hu_18, tot_hu_17)

## "Struggling"
## It doesn't look like this maps on to a Census label

v19 %>% filter(grepl('Struggling', label))
v19 %>% filter(grepl('struggling', label))
v19 %>% filter(grepl('struggle', label))
v19 %>% filter(grepl('Struggle', label))

# FINALIZE
# calculations were manually input into a spreadsheet; this file is the crosswalk to recreate them

fix_label <- sapply(acs_dictionary$label, gsub, pattern='!!', replacement=', ')
acs_dictionary$label <- fix_label
write.csv(acs_dictionary, '~/PredictAlign/ACS_Census_Data_Dictionary.csv')


