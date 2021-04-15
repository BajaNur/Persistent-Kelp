###Written by -----Nur Arafeh Dalmau----2021
## The University of Queensland
##Paper in Communications Earth & Environment
### Code for plotting a box plot for persistence data in each region (Supplementary figure 2)


###Load packages

library(tidyverse)
library(ggplot2)


#read my data 

Persistence <- read.csv(file = "kelp_occupancy_07282020.csv") 


#Convert  into factor
#Persistence$Ocupancy <- as.factor(Persistence$Ocupancy)

#Filter by region

CE_CA <- Persistence %>% filter(Lat > 34.45, Long < -120.47)
CE_CA$Region <- "01_Central_CA"

SO_CA <- Persistence %>% filter (Lat > 32.54, Lat < 34.475, Long > -120.47)
SO_CA$Region <- "02_Southern_CA"

NO_BA <- Persistence %>% filter (Lat < 32.54, Lat > 29.7205)
NO_BA$Region <- "03_Northern_BA"

CE_BA <- Persistence %>% filter (Lat < 29.7205)
CE_BA$Region <- "04_Central_BA"

All <- rbind(CE_CA, SO_CA, NO_BA, CE_BA)

#Summarise stats 

PER_ALL <- All %>% group_by(Region) %>% summarise(m = mean (Ocupancy), sd = sd(Ocupancy))

#Plot


p1 <- ggplot(All) + aes (x = Region, y = Ocupancy, color= Region) +
  geom_boxplot(data = All, mapping = aes(x = Region, y = Ocupancy, color=Region)) + theme_bw() 
p1


