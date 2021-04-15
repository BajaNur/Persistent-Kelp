###Written by -----Nur Arafeh Dalmau----2021
## The University of Queensland
##Paper in Communications Earth & Environment
  #In this code we plot the time series (dynamics) of giant kelp area for the 
  #past 35 years for each of the four regions in the Northeast Pacific Ocean

#Load packages

library (ggplot2)
library (tidyverse)

#Central California

dynamics_CC <- read.csv("Time_Series_Kelp.csv")


p1 <- ggplot(dynamics_CC, aes(x = year, y = Northern_CA)) +
  geom_path(size = 1) + 
 scale_x_continuous(breaks=seq(1984,2018.5,2)) + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) 
  
p1
 
  
#Southern California


dynamics_SC <- read.csv("Time_Series_Kelp.csv") %>% select (year, month, day, Southern_CA)


p2 <- ggplot(dynamics_SC, aes(x = year, y = Southern_CA)) +
  geom_path(size = 1) + 
  scale_x_continuous(breaks=seq(1984,2018.5,2)) + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) 

p2


#Northern Baja


dynamics_NBA <- read.csv("Time_Series_Kelp.csv") %>% select (year, month, day, Northern_BA)


p3 <- ggplot(dynamics_NBA, aes(x = year, y = Northern_BA)) +
  geom_path(size = 1) + 
  scale_x_continuous(breaks=seq(1984,2018.5,2)) + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) 

p3


#Central Baja


dynamics_CBA <- read.csv("Time_Series_Kelp.csv") %>% select (year, month, day, Central_BA)


p4 <- ggplot(dynamics_CBA, aes(x = year, y = Central_BA)) +
  geom_path(size = 1) + 
  scale_x_continuous(breaks=seq(1984,2018.5,2)) + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) 

p4
