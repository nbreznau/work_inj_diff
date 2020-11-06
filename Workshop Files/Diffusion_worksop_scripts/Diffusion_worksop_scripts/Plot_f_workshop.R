
install.packages(c("tidyverse", "ggplot2", "maptools", "maps",
                   "cowplot", "countrycode", "gridextra")) 

library(tidyverse)
library(ggplot2)
library(maptools) 
library(maps)
library(cowplot)
library(countrycode)
library(gridExtra)


# Diffusion plotting #####################################################################         
# Author: Helen Seitzer ##################################################################
# Global Dynamics of Social Policy CRC 1342 ##############################################
# Project A05 The Globlal Development, Diffusion and Transformation of Education Systems #

######################################## Plotting country characteristics diffusing over time ###########################

rm(list=ls())
####################### reading in Dataset

data <- read_delim("edu_intr_comp_toa_only.csv", ";", escape_double = FALSE, trim_ws = TRUE)


data$X3 <- NULL
####################### reshaping Dataset

data<- data %>% 
  filter(!is.na(toa))

# Fix Ivory Coasts label
data[37, 1] <- "Ivory Coast"


####################### display Country names

data %>% distinct(country) %>% pull() 

class(data$country)


####################### loading full worldmap

map.world <- map_data('world')

# subset Antarctica for visuals

map.world <- map.world %>% filter(region != "Antarctica")

####################### Pulling distinct region names

# Retrievethe map data long and lat of "our" countries

maps <- map_data("world", region = data$country)

# which countries are in our dataset but not in the full map (names are wrong)

data$country[!(data$country %in% maps$region)] # 14

maps$region[!(maps$region %in% data$country)] # 0 - we just subsetted this, so there has to be 0

data$country[data$country == "Brunei Darussalam"] <- "Brunei"
data$country[data$country == "Congo"] <- "Republic of Congo"
data$country[data$country == "China, Hong Kong Special Administrative Region"] <- "Hong Kong"
data$country[data$country == "East-Timor"] <- "Timor-Leste"
data$country[data$country == "Eswatini"] <- "Swaziland"
data$country[data$country == "Lao People's Democratic Republic"] <- "Laos"
data$country[data$country == "Republic of Korea"] <- "South Korea"
data$country[data$country == "The former Yugoslav Republic of Macedonia"] <- "Macedonia"
data$country[data$country == "Trinidad and Tobago"] <- "Trinidad"
data$country[data$country == "United Kingdom of Great Britain and Northern Ireland"] <- "UK"
data$country[data$country == "United States of America"] <- "USA"
data$country[data$country == "Russian Federation"] <- "Russia"


# rerun the subset
maps <- map_data("world", region = data$country)

data$country[!(data$country %in% maps$region)] # 3

####################### Dealing with Historical units

# Combine Czechia and Slovakia to Czechoslovakia. 
# The only problem could occur, if czechica and Czechoslovakia have data in the same plot
# then it will not be distinguished. 
# Otherwise just adding territories and plotting them in different plots should not be a problem

CS <- map.world %>% filter(region %in% c("Czech Republic", "Slovakia"))

CS$region[CS$region == "Czech Republic"] <- "Czechoslovakia"
CS$region[CS$region == "Slovakia"] <- "Czechoslovakia"

maps2 <- rbind(maps, CS)

####################### Pulling regions like Hong Kong to "region" 

# mutate Hong Kong: It is only in subregion, not in region, so we rename China, 
# where the subregion is Hong Kong to Hong Kong (thereby making it a country)

maps2 <- maps2 %>% mutate(region = case_when(subregion == "Hong Kong"~ "Hong Kong", 
                                                    is.na(subregion) ~ region,
                                                    subregion != "Hong Kong"~ region))


# Example base plot - we do not have data for the light spots (these were
# countries not in our dataset) that is the reason we have map.world 

ggplot(maps2, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill="lightgray", colour = "darkgrey") 

# not existent - ignore

unique(map.world$region[!(map.world$region %in% maps2$region)]) #  83 countries were not in our sample

####################### merge Datasets

plotting <- full_join(data, maps2,  by = c('country' = 'region' ))

summary(plotting$toa)

min(plotting$toa, na.rm = T)
max(plotting$toa, na.rm = T)

plotting$toa[plotting$toa == 9999] <- NA

plotting <- plotting %>% mutate(new_var = case_when(toa <= 1800 ~ "1800", 
                                                toa > 1800 & toa <= 1850 ~ "1850",
                                                toa > 1850 & toa <= 1900 ~ "1900",
                                                toa > 1900 & toa <= 1950 ~ "1950",
                                                toa > 1950 & toa <= 2010 ~ "2010")) %>%
  mutate(fst = ifelse(toa <= 1850, 1, NA)) %>%
  mutate(sec = ifelse(toa <= 1900, 1, NA))  %>%
  mutate(trd = ifelse(toa <= 1950, 1, NA))  %>%
  mutate(frth = ifelse(toa <= 2010, 1, NA)) 


### 


plot1 <- ggplot() +
  # first layer: all countries, no fill, no white outlines
  geom_polygon(data = map.world, 
               aes(x = long, y = lat, group = group), fill="grey", colour = "black", show.legend = F, size = 0.2) +
  # second layer: only countries with a fill
  geom_polygon(data = subset(plotting, !is.na(fst)), 
               aes(x = long, y = lat, group = group, fill = fst), fill = "red", colour = "black", size = 0.2)+
  ggtitle("Compulsory Education introduced before 1850")+  
  theme(plot.title = element_text(hjust = 0.5, size = 6),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        rect = element_blank())+
  coord_fixed(1.3)+
  theme(plot.margin=unit(c(0.5,0.5,0.5,0.5), "cm"))


plot2 <- ggplot() +
  # first layer: all countries, no fill, no white outlines
  geom_polygon(data = map.world, 
               aes(x = long, y = lat, group = group), fill="grey", colour = "black", show.legend = F, size = 0.2) +
  # second layer: only countries with a fill
  geom_polygon(data = subset(plotting, !is.na(sec)), 
               aes(x = long, y = lat, group = group, fill = sec), fill = "red", colour = "black", size = 0.2)+
  ggtitle("Compulsory Education introduced before 1900")+  
  theme(plot.title = element_text(hjust = 0.5, size = 6),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        rect = element_blank())+
  coord_fixed(1.3)+  
  theme(plot.margin=unit(c(0.5,0.5,0.5,0.5), "cm"))


plot3 <- ggplot() +
  # first layer: all countries, no fill, no white outlines
  geom_polygon(data = map.world, 
               aes(x = long, y = lat, group = group), fill="grey", colour = "black", show.legend = F, size = 0.2) +
  # second layer: only countries with a fill
  geom_polygon(data = subset(plotting, !is.na(trd)), 
               aes(x = long, y = lat, group = group, fill = trd), fill = "red", colour = "black", size = 0.2)+
  ggtitle("Compulsory Education introduced before 1950")+  
  theme(plot.title = element_text(hjust = 0.5, size = 6),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        rect = element_blank())+
  coord_fixed(1.3)+
  theme(plot.margin=unit(c(0.5,0.5,0.5,0.5), "cm"))


plot4 <- ggplot() +
  # first layer: all countries, no fill, no white outlines
  geom_polygon(data = map.world, 
               aes(x = long, y = lat, group = group), fill="grey", colour = "black", show.legend = F, size = 0.2) +
  # second layer: only countries with a fill
  geom_polygon(data = subset(plotting, !is.na(frth)), 
               aes(x = long, y = lat, group = group, fill = frth), fill = "red", colour = "black", size = 0.2)+
  ggtitle("Compulsory Education introduced before 2010")+  
  theme(plot.title = element_text(hjust = 0.5, size = 6),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
              rect = element_blank())+
  coord_fixed(1.3)+
  theme(plot.margin=unit(c(0.5,0.5,0.5,0.5), "cm"))


plot_grid(plot1, plot2, plot3, plot4, 
             ncol = 2, nrow = 2)
###
ggplot() +
  geom_polygon(data = map.world, 
               aes(x = long, y = lat, group = group), fill="grey", colour = "black", show.legend = F, size = 0.2) +
  geom_polygon(data= plotting, aes(x = long, y = lat, group = group,  fill = new_var), colour = "black", show.legend = T)+
  scale_fill_manual(values = c('1800' = 'darkred', '1850' = 'red', '1900' = 'orange', '1950' = 'goldenrod1', '2010' = 'yellow'), na.value = "darkgrey",  
                    breaks = c('1800', '1850', '1900', '1950', '2010', NA))+ 
  ggtitle("Introduction of Compulsory Education")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        rect = element_blank(),
        legend.title = element_text("Introduced before"))+
  coord_fixed(1.3)+
  theme(plot.margin=unit(c(0.5,0.5,0.5,0.5), "cm"))
