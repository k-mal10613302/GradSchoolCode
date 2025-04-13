# STAT 515 Midterm Redesign Project
# Fall Semester 2022
# Team 1: Vidya Bhagwandin, Sukhdeep Singh, Keith Maly

# Overview: The team is redesigning select visualizations from the 2018 Crime in 
# Washington [State] report issued annually by the Washington Association of 
# Sheriffs and Police Chiefs to conform with principles learned in STAT515 using
# R Studio IDE.

# Data Source: The data for the report and redesign effort are drawn from the 
# National Incident-based Reporting System (NIBRS) and was extracted from the FBI's
# Crime Data Explorer tool, available via URL 
# https://crime-data-explorer.app.cloud.gov/pages/home
# FIPS Codes from Washington State Converted into as csv table drawn from
# WA State Department of Heath Website
# https://doh.wa.gov/sites/default/files/legacy/Documents/8370//SeerCountyFIPSCodeMap.pdf

# 0. Environment Setup: Library calls, clear existing Global Variables, set
# working directory

rm(list = ls())
library(tidyverse)
library(micromapST)
library(sp)
library(rgdal)
library(usethis)
library(devtools)
devtools::install_github('UrbanInstitute/urbnmapr')
library(urbnmapr)
library(micromap)
library(treemap)
#add others as needed

setwd("~/Documents/STAT515/midterm/nibrs/data") # adapt to your work path

# 1. NIBRS Tables Data Import
# please see the simplified entity-relationship diagram in the team's final 
# report for a visual depiction of the table interrelationships.
# goal is to create a data frame (or tibble) containing only the needed 

wa_fips <- read.csv(file='wa_fips.csv', header = TRUE, 
                     stringsAsFactors = FALSE)
agencies <- read.csv(file='agencies.csv', header = TRUE, 
                      stringsAsFactors = FALSE)
incident <- read.csv(file='incident.csv', header = TRUE, 
                      stringsAsFactors = FALSE)
offender <- read.csv(file='offender.csv', header = TRUE, 
                      stringsAsFactors = FALSE)
offense <- read.csv(file='offense.csv', header = TRUE, 
                      stringsAsFactors = FALSE)
victim <- read.csv(file='victim.csv', header = TRUE, 
                      stringsAsFactors = FALSE)
victim_injury <- read.csv(file='victim_injury.csv', header = TRUE, 
                      stringsAsFactors = FALSE)
ref_race <- read.csv(file = 'ref_race.csv', header = TRUE,
                     stringsAsFactors = FALSE)
victim_offender_rel <- read.csv(file='victim_offender_rel.csv', header = TRUE, 
                                             stringsAsFactors = FALSE)
location_type <- read.csv(file='location_type.csv', header = TRUE, 
                                    stringsAsFactors = FALSE)
offense_type <- read.csv(file='offense_type.csv', header = TRUE, 
                      stringsAsFactors = FALSE)
weapon <- read.csv(file='weapon.csv', header = TRUE, 
                      stringsAsFactors = FALSE)
criminal_act <- read.csv(file='criminal_act.csv', header = TRUE, 
                      stringsAsFactors = FALSE)
weapon_type <- read.csv(file='weapon_type.csv', header = TRUE, 
                                  stringsAsFactors = FALSE)
criminal_act_type <- read.csv(file='criminal_act_type.csv', header = TRUE, 
                      stringsAsFactors = FALSE)
injury <- read.csv(file='injury.csv', header = TRUE, 
                                     stringsAsFactors = FALSE)
relationship <- read.csv(file='relationship.csv', header = TRUE, 
                   stringsAsFactors = FALSE)

#1.1 Table Combination via join
# The objective of this code block is to combine the tables, beginning with
# the less complex tables. In each iteration, features that are not used in
# follow on analysis are dropped from the merged table

# 1.1.0 preprocessing
# 1.1.0.a force county_fips to type char and add FIPS code to agencies 
# table within NIBRS
wa_fips$county_fips <- as.character(wa_fips$county_fips)

agencies <- agencies %>% 
  left_join(wa_fips, c('COUNTY_NAME'='COUNTY'))

rm(wa_fips)

# 1.1.0.b filter ref_race to codes currently in use; add race codes to both
# the victim and offender tables; and relabel common feature names in each table
# for a future join (ie, age_id -> o_age_id)
ref_race <- ref_race %>%
  filter(RACE_ID <= 4 | RACE_ID == 8 | RACE_ID > 10) %>%
  select(-RACE_CODE, -SORT_ORDER, -START_YEAR, -END_YEAR, -NOTES)

victim <- victim %>%
  left_join(ref_race, by = 'RACE_ID') %>%
  rename('V_AGE_NUM' = 'AGE_NUM', 'V_AGE_ID'= 'AGE_ID', 'V_GENDER'= 'SEX_CODE',
         'V_RACE_ID' = 'RACE_ID', 'V_ETHNICITY' = 'ETHNICITY_ID',
         'V_AGE_RANGE_LO' = 'AGE_RANGE_LOW_NUM', 'V_AGE_RANGE_HI' = 'AGE_RANGE_HIGH_NUM')

victim <- victim %>%
  rename('V_RACE_DESC'='RACE_DESC')

offender <- offender %>%
  left_join(ref_race, by = 'RACE_ID')%>%
  rename('O_AGE_NUM' = 'AGE_NUM', 'O_AGE_ID'= 'AGE_ID', 'O_GENDER'= 'SEX_CODE',
         'O_RACE_ID' = 'RACE_ID', 'O_ETHNICITY' = 'ETHNICITY_ID',
         'O_AGE_RANGE_LO' = 'AGE_RANGE_LOW_NUM', 'O_AGE_RANGE_HI' = 'AGE_RANGE_HIGH_NUM')

offender <- offender %>%
  rename('O_RACE_DESC'='RACE_DESC')

rm(ref_race)

# 1.1.a.adding the type of crime to the table of criminal acts and drop unused features
new_criminal_act <- criminal_act %>% 
  left_join(criminal_act_type, by = 'CRIMINAL_ACT_ID') %>%
  select(-CRIMINAL_ACT_CODE, -CRIMINAL_ACT_DESC)

# show that the join worked as intended and remove other tables from the IDE
tail(new_criminal_act)
rm(criminal_act, criminal_act_type)

#1.1.b adding weapon type to the weapon table and drop unused features
new_weapon <- weapon %>%
  left_join(weapon_type, by = 'WEAPON_ID') %>%
  select(-WEAPON_CODE, -SHR_FLAG)

# show that the join worked as intended and remove other tables from the IDE
head(new_weapon)
rm(weapon, weapon_type)

#1.1.c adding victim injury to the victim table and drop unused features
new_victim_injury <- victim_injury %>%
  left_join(injury, by = 'INJURY_ID') %>%
  select(-INJURY_CODE)

# show that the join worked as intended and remove other tables from the IDE
head(new_victim_injury)
rm(victim_injury, injury)

#1.1.d fold location type into the offense table and drop unused features
offense1 <- offense %>%
  left_join(location_type, by = 'LOCATION_ID') %>%
  select(-LOCATION_CODE)

# show that the join worked as intended and remove other tables from the IDE
head(offense1)
rm(location_type, offense)

#1.1.e fold new_victim_injury into victim
new_victim <- victim %>%
  left_join(new_victim_injury, by = c('VICTIM_ID','DATA_YEAR'))

# show that the join worked as intended and remove other tables from the IDE
head(new_victim)
rm(new_victim_injury, victim)

#1.1.f fold offense_type into offense1 and drop unused features
offense2 <- offense1 %>%
  left_join(offense_type, by = 'OFFENSE_TYPE_ID') %>%
  select(-OFFENSE_CODE, -CT_FLAG)

# show that the join worked as intended and remove other tables from the IDE
head(offense2)
rm(offense_type, offense1)

#1.1.g fold new_criminal_act into offense2
offense3 <- offense2 %>%
  left_join(new_criminal_act, by = c('DATA_YEAR', 'OFFENSE_ID'))

# show that the join worked as intended and remove other tables from the IDE
head(offense3)
rm(new_criminal_act, offense2)

#1.1.h fold new_weapon into offense3
offense4 <- offense3 %>%
  left_join(new_weapon, by = c('DATA_YEAR', 'OFFENSE_ID'))

# show that the join worked as intended and remove other tables from the IDE
head(offense4)
rm(new_weapon, offense3)

#1.1.i fold relationship into victim_offender_rel, drop unused columns
victim_offender_rel1 <- victim_offender_rel %>%
  left_join(relationship, by = 'RELATIONSHIP_ID')

# show that the join worked as intended and remove other tables from the IDE
head(victim_offender_rel1)
rm(relationship, victim_offender_rel)

#1.1.j join agencies and incidents into a single table, but first necking down
# the features in the agencies table since we are mostly after the county info
agency_incident_rpt <- agencies %>%
  select(-LEGACY_ORI, -COVERED_BY_LEGACY_ORI, -DIRECT_CONTRIBUTOR_FLAG,
         -DORMANT_FLAG, -DORMANT_YEAR, -UCR_AGENCY_NAME, -STATE_ID, 
         -STATE_POSTAL_ABBR, -DIVISION_NAME, -REGION_CODE, -REGION_NAME,
         -REGION_DESC, -POPULATION_GROUP_CODE,-PUBLISHABLE_FLAG, -PARTICIPATED,
         -NIBRS_PARTICIPATED) %>%
  right_join(incident, by = c('DATA_YEAR', 'AGENCY_ID'))

# show that the join worked as intended and remove other tables from the IDE
head(agency_incident_rpt)
rm(agencies, incident)

#1.1.k join new_victim and victim_offender_rel1 into a single table, not 
# dropping any features
victim_offender_rel2 <- victim_offender_rel1 %>%
  right_join(new_victim, by = c('DATA_YEAR', 'VICTIM_ID'))

# show that the join worked as intended and remove other tables from the IDE
head(victim_offender_rel2)
rm(victim_offender_rel1, new_victim)

#1.1.l join offender and victim_offender_rel2 into a single table, not 
# dropping any features
victim_offender_rel3 <- victim_offender_rel2 %>%
  right_join(offender, by = c('DATA_YEAR', 'OFFENDER_ID', 'INCIDENT_ID'))

# show that the join worked as intended and remove other tables from the IDE
head(victim_offender_rel3)
rm(victim_offender_rel2, offender)

#1.1.m join agency_incident_rpt and offense4 into a single table, not 
# dropping any features
agency_incid_offense <- offense4 %>%
  right_join(agency_incident_rpt, by = c('DATA_YEAR', 'INCIDENT_ID'))

# show that the join worked as intended and remove other tables from the IDE
head(agency_incid_offense)
rm(offense4, agency_incident_rpt)

#1.1.n join agency_incident_offense and victim_offender_rel3 into a single table,
#not dropping any features. Final table name CIW2018 (Crime in Washington 2018)
CIW2018 <- agency_incid_offense %>%
  left_join(victim_offender_rel3, by = c('DATA_YEAR', 'INCIDENT_ID'))

# show that the join worked as intended and remove other tables from the IDE
head(CIW2018)
rm(victim_offender_rel3, agency_incid_offense)

# 1.1.o write CIW2018 into a csv file
write_csv(CIW2018, "CIW2018.csv")

#1.2. Transforming Map Data for Use in R
# Source: The source for map data are the ubrnmapr library; team used
# instructions from this Medium article as a go-by to import a map file with 
# integrated County FIPS Codes,
# https://urban-institute.medium.com/how-to-create-state-and-county-maps-easily-in-r-577d29300bb2

# 1.2.a importing county-level shape file data and attributes from the ubrnmapr library
wa_cty_map <- urbnmapr::counties %>%
  left_join(urbnmapr::countydata, by = "county_fips") %>%
  filter(state_name == 'Washington') %>%
  select(-medhhincome, -horate, -year, -hhpop, -fips_class, -state_abbv,
         -state_fips)

#test plot in ggplot of wa_cty_map
ggplot(data = wa_cty_map, mapping = aes(x=long, y=lat, group=group)) +
  geom_polygon(fill = 'white', color = 'black') +
  theme_bw()

# 2 Integrating Map Frame and Crime Data
# 2.1. Build a density plot of murders and negligent homicides by county 
# 2.1.a Create a data frame of the murders by county from CIW2018. This 
# df removes homicides on tribal lands, because they do not have FIPS codes that
# can be geospatially plotted
CIW2018_murder <- CIW2018 %>%
  filter(OFFENSE_TYPE_ID == 32) %>%
  select(county_fips) %>%
  count(county_fips) %>%
  filter(!is.na(county_fips))

# 2.1.b. Join this new data frame to the wa_cty_map df by FIPS code
map1 <- wa_cty_map %>%
  left_join(CIW2018_murder, by = 'county_fips')

# 2.1.c. Plot the resultant data frame using ggplot
map1 %>% ggplot(aes(x = long, y = lat, group = group, fill = n)) +
  geom_polygon(color = 'black') +
  scale_fill_gradient(low = '#EFD005', high = '#E73B16', guide = 'colorbar',
                      na.value = 'grey50') +
  coord_map(projection = 'albers', lat0 = 45, lat1 = 50) +
  theme_bw() +
  labs(title = '2018 Murder and Negligent Manslaughter by County',
       x = 'Longitude', y = 'Latitude',
       caption = 'Gray Represents No Reported Incidents',fill = 'Scale')

# 2.2. From CIW2018, create a new df that counts murder and negligent homicide
# by location type in the counties that these crimes occurred for use in a treemap.

# dataframes for the treemap, basic treemap is first
CIW2018_murder1 <- CIW2018 %>%
  filter(OFFENSE_TYPE_ID == 32) %>%
  select(COUNTY_NAME, county_fips) %>%
  count(COUNTY_NAME, county_fips) %>%
  filter(!is.na(county_fips))

#trial treemap, basic index
map2 <- treemap(CIW2018_murder1, 
                index = 'COUNTY_NAME',
                vSize= 'n',
                type = 'index')

CIW2018_murder2 <- CIW2018 %>%
  filter(OFFENSE_TYPE_ID == 32) %>%
  select(LOCATION_NAME) %>%
  count(LOCATION_NAME)

#trial treemap, basic index
treemap(CIW2018_murder2, index = 'LOCATION_NAME',
        vSize= 'n', type = 'index',
        title = '2018 Murders and Negligent Homicides by Location Type',
        fontsize.title = 14,
        fontsize.labels= 14,
        fontcolor.labels= "black",
        overlap.labels=0.5,                      
        inflate.labels=F)

#data frames for treemaps with subindexing
CIW2018_m_loc <- CIW2018 %>%
  filter(OFFENSE_TYPE_ID==32) %>%
  select(COUNTY_NAME, LOCATION_ID, LOCATION_NAME) %>%
  count(COUNTY_NAME, LOCATION_NAME)

CIW2018_weap_loc <- CIW2018 %>%
  filter(OFFENSE_TYPE_ID == 32) %>%
  select (LOCATION_NAME, WEAPON_NAME) %>%
  count(LOCATION_NAME, WEAPON_NAME)

tm_cty_loc <- treemap(CIW2018_m_loc,
                index = c('COUNTY_NAME', 'LOCATION_NAME'),
                vSize = 'n',
                type = 'index')

tm_loc_weap <- treemap(CIW2018_weap_loc,
                      index = c('LOCATION_NAME', 'WEAPON_NAME'),
                      vSize = 'n',
                      type = 'index')

#2.3 Create a Polar Coordinate Plot of Murder and Negligent Manslaughter as a 
# function of the hour of day

CIW2018_murnm_hr <- CIW2018 %>%
  filter(OFFENSE_TYPE_ID == 32) %>%
  select(INCIDENT_HOUR) %>%
  count(INCIDENT_HOUR) %>%
  arrange(INCIDENT_HOUR) %>%
  filter(!is.na(INCIDENT_HOUR))

ggplot(CIW2018_murnm_hr, aes(x = INCIDENT_HOUR, y = n)) +
  geom_col(fill = 'blue', alpha = 0.75) + 
  coord_polar(theta = 'x', start = -0.12) +
  geom_vline(aes(xintercept=5.5)) +   
  geom_vline(aes(xintercept=17.5)) +
  annotate(geom='text', x=6, y=50, label ='Sunrise') +
  annotate(geom='text', x=18, y=50, label ='Sunset') +
  theme_bw()+
  labs(title = '2018 Murder and Negligent Homicide by Incident Hour',
       subtitle = 'Black Lines depicts nominal sunrise-sunset times', 
       y = 'Count', x = 'Incident Hour')

CIW2018_murnm_hr_loc <- CIW2018 %>%
  filter(OFFENSE_TYPE_ID == 32) %>%
  select(INCIDENT_HOUR, LOCATION_NAME) %>%
  count(INCIDENT_HOUR, LOCATION_NAME) %>%
  arrange(INCIDENT_HOUR) %>%
  filter(!is.na(INCIDENT_HOUR))

ggplot(CIW2018_murnm_hr_loc, aes(x = INCIDENT_HOUR, y = n, fill = LOCATION_NAME)) +
  geom_col(alpha = 0.75) + 
  coord_polar(theta = 'x', start = -0.12) +
  geom_vline(aes(xintercept=5.5)) +   
  geom_vline(aes(xintercept=17.5)) +
  annotate(geom='text', x=6, y=50, label ='Sunrise') +
  annotate(geom='text', x=18, y=50, label ='Sunset') +
  theme_bw()+
  labs(title = '2018 Murder/Neg. Homicides',
       subtitle = 'By Incident Hour',
       caption  = 'Black Lines depicts nom. sunrise-sunset', 
       y = 'Count', x = 'Incident Hour', fill='Location Name')

# create stacked bar chart 
# get a ranked list of top 5 counties 
CIW2018_top_cty <- CIW2018 %>%
  filter(OFFENSE_TYPE_ID == 32) %>%
  select(county_fips) %>%
  filter(!is.na(county_fips)) %>%
  count(county_fips) %>%
  arrange(-n) %>%
  filter(rank(desc(n))< 5)

CIW2018_cty_wep <- CIW2018 %>%
  filter(OFFENSE_TYPE_ID==32) %>%
  select(county_fips, COUNTY_NAME, WEAPON_NAME) %>%
  filter(county_fips %in% CIW2018_top_cty$county_fips) %>%
  count(COUNTY_NAME, WEAPON_NAME)%>%
  arrange(COUNTY_NAME, -n)

ggplot(data = CIW2018_cty_wep, aes(x=COUNTY_NAME, y = n, fill = WEAPON_NAME)) +
  geom_bar(stat="identity",position="fill", width=.3000)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))+
  labs(x = "County",
       y = "Proportion",
       title = "Weapons Used by County, Murder and Negligent Homicide, 2018",
       fill = 'Weapon Name')
  

