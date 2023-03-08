


###### 30-yr runoff from TerraClimate


setwd("C:/Users/uryem/OneDrive - University of Waterloo/Desktop/LEB_Wetland_P/Datasets")
library(ggplot2)
library(tidyverse)
library(sf)
library(cowplot)


data <- read.csv("runoff_timeseries_London.csv")

lab <- c("Jan", "Feb", "March", "Apr", "May", "Jun", "Jul", "Aug", "Sep", 
           "Oct", "Nov", "Dec")

ggplot(data, aes(x = as.factor(month), y = monthly_runoff_mm)) +
  geom_boxplot() +
  ylab("Monthly Runoff (mm)") +
  xlab(" ") +
  scale_x_discrete(labels = lab)
  

data <- read.csv("runoff_regions.csv") %>%
  select(!(LEB)) %>%
  pivot_longer(cols = c("StClair", "Eastern", "Southern", "Western", "Northern"),
               names_to = "Region",
               values_to = "Runoff")


lab2 <- c("Jan", " ", " ", "Apr", " ", " ", "Jul", " ", " ", 
         "Oct", " ", " ")

ggplot(data, aes(x = as.factor(Month), y = Runoff)) +
  geom_boxplot() +
  facet_wrap(~Region, nrow = 2) +
  ylab("Monthly Runoff (mm)") +
  xlab(" ") +
  scale_x_discrete(labels = lab2)



##### with map
data <- read.csv("runoff_regions.csv") 

N <- ggplot(data, aes(x = as.factor(Month), y = Northern)) +
  geom_boxplot(fill = "#8da0cb") +
  labs(title = "Northern Lake Erie", y = "Monthly Runoff (mm)", x = " ") +
  scale_x_discrete(labels = lab2) +
  ylim(0,400) +
  theme_classic()

SC <- ggplot(data, aes(x = as.factor(Month), y = StClair)) +
  geom_boxplot(fill = "#fc8d62") +
  labs(title = "St. Claire - Detroit", y = "Monthly Runoff (mm)", x = " ") +
  scale_x_discrete(labels = lab2) +
  ylim(0,400) +
  theme_classic()

W <- ggplot(data, aes(x = as.factor(Month), y = Western)) +
  geom_boxplot(fill = "#66c2a5") +
  labs(title = "Western Lake Erie", y = "Monthly Runoff (mm)", x = " ") +
  scale_x_discrete(labels = lab2) +
  ylim(0,400) +
  theme_classic()

S <- ggplot(data, aes(x = as.factor(Month), y = Southern)) +
  geom_boxplot(fill = "#e78ac3") +
  labs(title = "Southern Lake Erie", y = "Monthly Runoff (mm)", x = " ") +
  scale_x_discrete(labels = lab2)+
  ylim(0,400) +
  theme_classic()

E <- ggplot(data, aes(x = as.factor(Month), y = Eastern)) +
  geom_boxplot(fill = "#a6d854") +
  labs(title = "Eastern Lake Erie", y = "Monthly Runoff (mm)", x = " ") +
  scale_x_discrete(labels = lab2) +
  ylim(0,400) +
  theme_classic()


plot_grid(SC, N, NULL, W, S, E, nrow = 2)

HUC4 <- st_read("GISdata/HUC4_boundaries.shp", quiet = TRUE) %>%
  select(c(geometry, name))


# map version 1

plot(HUC4["name"], main = " ", key.pos = NULL)


## map version 2
colors <- c("#a6d854", "#8da0cb", "#e78ac3", "#fc8d62", "#66c2a5")


ggplot() + geom_sf(data = HUC4, aes(fill = name)) + 
  theme_bw() +
  scale_fill_manual(values = colors)


map <- ggplot() + geom_sf(data = HUC4, aes(fill = name), show.legend = FALSE) + 
  theme_map() +
  scale_fill_manual(values = colors) +
  labs(title = "Lake Erie Basin")




plot_grid(SC, map, N, W, S, E, nrow = 2)


