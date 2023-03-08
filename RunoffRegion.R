


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
  geom_boxplot() +
  ylab("Monthly Runoff (mm)") +
  xlab(" ") +
  scale_x_discrete(labels = lab2)
SC <- ggplot(data, aes(x = as.factor(Month), y = StClair)) +
  geom_boxplot() +
  ylab("Monthly Runoff (mm)") +
  xlab(" ") +
  scale_x_discrete(labels = lab2)
W <- ggplot(data, aes(x = as.factor(Month), y = Western)) +
  geom_boxplot() +
  ylab("Monthly Runoff (mm)") +
  xlab(" ") +
  scale_x_discrete(labels = lab2)
S <- ggplot(data, aes(x = as.factor(Month), y = Southern)) +
  geom_boxplot() +
  ylab("Monthly Runoff (mm)") +
  xlab(" ") +
  scale_x_discrete(labels = lab2)
E <- ggplot(data, aes(x = as.factor(Month), y = Eastern)) +
  geom_boxplot() +
  ylab("Monthly Runoff (mm)") +
  xlab(" ") +
  scale_x_discrete(labels = lab2)

HUC4 <- st_read("GISdata/HUC4_boundaries.shp", quiet = TRUE) %>%
  select(c(geometry, name))

plot(HUC4["name"], main = " ", key.pos = NULL)

  
top <- plot_grid(map, SC, N)
  
  
  
  
  

