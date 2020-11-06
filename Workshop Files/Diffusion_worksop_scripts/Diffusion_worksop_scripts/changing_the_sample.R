
## Diffusion Workshop A05 2020 Data Wrangling script 1: changing the sample

# Expanding/reducing the dataset to the full sample with Years


#------------------------------------------------------------------------------------------------ 
#  This script contains: 
  
# 1. Loading sample country set

# 2. adding/removing countries and changing the time frame

# 3. creating full sample dataframe

# 4. merging onto own dataset

# 5. substituting data


# The script creates a sample dataframe which can be merged onto the actual data and then used for analysis. 

#------------------------------------------------------------------------------------------------ 
  
## Prep

#install.packages("countrycode")
#install.packages("tidyverse")

library(countrycode)
library(tidyverse)

rm(list = ls())

# setwd

# ------------------------------------------------------------------------------------------------ 
  
## 1. Loading sample country set
  
# Read in "official" country list

country_sample <- readr::read_delim("country_sample_diff_workshop.csv", 
                                    ";", escape_double = FALSE, trim_ws = TRUE)

# ------------------------------------------------------------------------------------------------ 

## 2.a Changing the time frame
  
# First, define the years for your analysis:
  
years <- 1880:2010

#Then create a vector of ISO3 codes by extracting the countries from country_sample dataframe (apply filtering to remove countries if nessessary). Adding countries will be done later. 

# ------------------------------------------------------------------------------------------------ 

## 2.b removing countries


# If all countries in the original sample should be kept: 

iso3 <- unique(country_sample$iso3)

# Here, Taiwan, DDR and Kosovo are removed for the creation of the sample dataframe. The workshop sample does NOT include these
# countries, so please remove them for your analysis!! THis is just an example for futher analyses!!

iso3 <- country_sample %>% filter(!iso3 %in% c("TWN", "DDR", "KOS")) %>% pull(iso3)


# ------------------------------------------------------------------------------------------------ 

## 2.c adding countries depending on what is known (iso code, country name, cow code)

# Adding countries to the sample set manually before creating the example dataset:
  
# 1. option: add them in excel in the country sample dataframe, rerun steps 1, 2.a and 2.b. 
# 2. option: add them here to the iso3 vector: 
  
#  The append function adds strings to a vector.

iso3 <-  append(iso3, c("ISL", "SSD"))

# Checking if there are duplicates because there was something added already in the set?
  
iso3[duplicated(iso3)]

# remove those duplicates

iso3 <- iso3[!duplicated(iso3)]


# 3. option: Add rows to the country_sample dataframe and !! rerun afterwards from step 2.b !! to create the ISO3 vector
# Recreate the iso3 vector from the now changed country_sample dataframe!

country_sample <- country_sample %>% 
  add_row(country_name = c("Iceland", "Something"), iso3 = c("ISL", "SMT")) 

# !! rerun  from step 2.b !! to create the ISO3 vector

# ------------------------------------------------------------------------------------------------ 

##  3. creating full sample dataframe

# creating data frame that has one value for every country each year

df <- data.frame(iso3 = rep(iso3, times = length(years)),
                 year = rep(years, each = length(iso3)))

# Match country names to it if nessessary: the custom match allows for the specification of 
# codes not in the official list of the countrycode package. The package also has a list of historical codes
# this can be downloded by : countrycode::codelist_panel

df$country_names <- countrycode(df$iso3,          # origin variable
                                "iso3c",          # origin
                                "country.name",   # destination
                                 origin_regex = T
                                 ,custom_match = c("DDR" = "German Democratic Republic", 
                                                  "KOS" = "Kosovo"))


anyNA(df$country_names) # FALSE

# ------------------------------------------------------------------------------------------------ 

# Create directed (!!) dyadic sample dataframe if nessessary
# This creates a dataframe that includes all pairs of iso_o and iso_d for each year
# names are arbitrary on this step

df_dyad <- data.frame(iso3_o = rep(iso3, times = length(years)),
                      year = rep(years, each = length(iso3)),
                      iso3_d = rep(iso3, times = length(years)))

# expand the dataset, so there is one value for each pair in !! each direction !! per year

df_dyad <- data.frame(df_dyad %>% tidyr::expand(iso3_o, iso3_d, year))

# same number of occurrences of each country

table(df_dyad$iso3_o)

head(df_dyad)

# remove self ties

df_dyad <- df_dyad %>% filter(iso3_d != iso3_o)

head(df_dyad)

# If we read in this type of dataframe into NetdiffusR it adds the values. So if you have an unriected network
# you need to set the empty values to 0, the you can use this. 

# ------------------------------------------------------------------------------------------------ 

# here load own dataset

edu_intro <-  read_delim("edu_intr_comp_toa_only.CSV", 
                         ";", escape_double = FALSE, trim_ws = TRUE, locale = locale(encoding = "Latin1"))


edu_intro[,3] <- NULL

# load dyadic dataframe for demonstration

capdist_demo <- read_csv("capdist_demo.csv")

# remove self ties

capdist_demo <- capdist_demo %>% filter(iso_d != iso_o)

head(capdist_demo)

# to add your own country codes see custom_match line 111

edu_intro$iso3 <- countrycode(edu_intro$country, "country.name", 
                                         "iso3c", 
                                         origin_regex = T)


# Testing differences: 
# this is in own dataset and NOT in iso3 - will be solved via merge - these countries will be thrown out

setdiff(edu_intro$iso3, iso3)

# this is in iso3 and not in own dataset (will be solved via merge) - here you will have NAs

setdiff(iso3, edu_intro$iso3)


# this dataset does not have all countries in both columns

setdiff(iso3, capdist_demo$iso_o)

setdiff(iso3, capdist_demo$iso_d)

# too many countries in our dataset

setdiff(capdist_demo$iso_d, iso3)


# ------------------------------------------------------------------------------------------------ 

# censoring
# We want all nodes in the Network, even if the introduced something before our time frame

min(edu_intro$toa, na.rm = T) # left cencored

max(edu_intro$toa, na.rm = T) # right cencored

## censoring - introductions pre 1880 are set to 1880

edu_intro <- edu_intro %>% mutate(toa = if_else(toa < 1880, 1880, toa))

# everything later than 2010 (outside our time-frame) is right cencored and will need to be set to NA or get thrown out

edu_intro <- edu_intro %>% mutate(toa = if_else(toa > 2010, NA_real_, toa))


# ------------------------------------------------------------------------------------------------ 

# merge datasets together - specify correct colum names!! and all.y = T includes all rows from sample dataset

names(edu_intro)

# adapt variable names if necessary

full <- merge(edu_intro, df, by.x = c("country", "iso3"), by.y = c("country_names", "iso3"), all.y = T)

# the same works with dyadic data


names(own_dataset)

# make sure the order of origin and destination in the by functino is correct

full_dyadic<- merge(capdist_demo, df_dyad, by.x =c("iso_o","iso_d"), by.y = c("iso3_o", "iso3_d"), all.y = T)

# ------------------------------------------------------------------------------------------------ 

# order and sort - first group by year so within each year the countries are sorted
# can be changed, also works with dyadic data (by including origin and destination onto the order)

full <- data.frame(full %>%
                     group_by(year) %>%
                     do( data.frame(with(data=., .[order(iso3),] )) ))

# order dyadic

# make sure the order of the variable to sort by is correct

full_dyadic <- data.frame(full_dyadic %>%
                     group_by(iso_o, iso_d) %>%
                     do( data.frame(with(data=., .[order(year),] )) ))


# ------------------------------------------------------------------------------------------------ 


# filling constant variables is only necessary if the dataset was longitudinal in the beginning but had a smalller
# time frame than the example dataframe 
full_dyadic <- full_dyadic %>% group_by(iso_o, iso_d) %>% 
     fill(col, .direction = "updown") # down or up is also an option


# - end -
