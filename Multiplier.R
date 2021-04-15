
###Written by -----Nur Arafeh Dalmau----2021
## The University of Queensland
##Paper in Communications Earth & Environment
  #In this code we estimate the probability of present kelp for the Northeast Pacific Ocean and for the four regions
  #We then calculate the adjusted representation targets that account for present kelp, not just kelp

###Load packages

library(tidyverse)

# read my data   

Persistence<- read.csv ("kelp_occupancy_07282020.csv") 

###Classify by region

CE_CA <- Persistence %>% filter(Lat > 34.45, Long < -120.47)
CE_CA$Region <- "01_Central_CA"

SO_CA <- Persistence %>% filter (Lat > 32.54, Lat < 34.475, Long > -120.47)
SO_CA$Region <- "02_Southern_CA"

NO_BA <- Persistence %>% filter (Lat < 32.54, Lat > 29.7205)
NO_BA$Region <- "03_Northern_BA"

CE_BA <- Persistence %>% filter (Lat < 29.7205)
CE_BA$Region <- "04_Central_BA"

All_region <- rbind(CE_CA, SO_CA, NO_BA, CE_BA)


#######Classify by persistence

Low <- All_region %>% filter (Ocupancy < quantile (Persistence$Ocupancy, 0.25))
Low$Persistence <- "Low"

Mid <- All_region %>% filter (Ocupancy >= quantile (Persistence$Ocupancy, 0.25)) %>% 
  filter (Ocupancy <= quantile (Persistence$Ocupancy, 0.75))
Mid$Persistence <- "Mid"

High <- All_region%>% filter (Ocupancy > quantile (Persistence$Ocupancy, 0.75))
High$Persistence <- "High"

All <- rbind(Low, Mid, High) 

####Probability of present Kelp and multiplier (See paper, equations 1-3))

All_mean_count <- All %>% count 
All_mean_per <- All %>% summarise(per = mean(Ocupancy)) %>% mutate (Mul = 1/per)
All_mean <- cbind (All_mean_count, All_mean_per) %>% mutate (Mul = 1/per) 


All_mean_region_count <- All %>% group_by(Region) %>% count 
All_mean_region_per <- All %>% group_by(Region) %>% summarise (per = mean(Ocupancy)) 
All_mean_region <- cbind (All_mean_region_count, All_mean_region_per) %>% mutate (Mul = 1/per) %>% select(-3) 


All_mean_persistence_count <- All %>% group_by(Persistence) %>% count
All_mean_persistence_per <-All %>% group_by(Persistence) %>% summarise(per = mean(Ocupancy))
All_mean_persistence <- cbind (All_mean_persistence_count, All_mean_persistence_per) %>% mutate (Mul = 1/per) %>% select(-3) 

All_mean_region_persistence_count <- All %>% group_by(Region, Persistence) %>% count 
All_mean_region_persistence_per <- All %>% group_by(Region, Persistence) %>% summarise (per = mean(Ocupancy)) 
All_mean_region_persistence <- cbind (All_mean_region_persistence_count, All_mean_region_persistence_per) %>% select(-4, -5)




                      ########### Northeast Pacific Ocean ##################


#Formula to adjust representation target for present kelp: fixed targets (See paper, equations 4)

# Subset variables by persistence category 

NEPO_l <- All_mean_persistence %>% filter(Persistence...1 == "Low")

NEPO_m <- All_mean_persistence %>% filter(Persistence...1 == "Mid")

NEPO_h <- All_mean_persistence %>% filter(Persistence...1 == "High")

#Define Target. In this case we used Aichi 11 Target, 10% representation. Any other target can be used.

T_Aichi <- c(0.1)

T_adj__NEPO <- T_Aichi*All_mean$Mul*100
 

#Formula to get the target and multiplier using only persistence for Unfixed targets (See paper, equation 5)

Th_NEPO <- ((T_Aichi* ((All_mean$n) - (NEPO_l$per*NEPO_l$n) - (NEPO_m$per*NEPO_m$n))) /  (NEPO_h$per*NEPO_h$n))

Mh_NEPO <- Th_NEPO/T_Aichi

####Area new target
A_NEPO <-((T_Aichi*NEPO_l$n) + (T_Aichi*NEPO_m$n) + (Th_NEPO*NEPO_h$n))/(All_mean$n)*100



                                    ############Central California #######



# Subset variables by persistence category 

CE_CA_All <- All_mean_region %>% filter (Region...1 == "01_Central_CA")

CE_CA_l <- All_mean_region_persistence %>% filter (Persistence...2 == "Low", Region...1 == "01_Central_CA")

CE_CA_m <- All_mean_region_persistence %>% filter (Persistence...2 == "Mid", Region...1 == "01_Central_CA")

CE_CA_h <- All_mean_region_persistence %>% filter (Persistence...2 == "High", Region...1 == "01_Central_CA")


#Formula to adjust representation target for present kelp: fixed targets (See paper, equations 4)

T_adj_CE_CA <- T_Aichi*CE_CA_All$Mul*100


#Formula to get the target and multiplier using only persistence for Unfixed targets (See paper, equation 5)

Th_CE_CA <- ((T_Aichi* ((CE_CA_All$n) - (CE_CA_l$per*CE_CA_l$n) - (CE_CA_m$per*CE_CA_m$n))) /  (CE_CA_h$per*CE_CA_h$n))

Mh_CE_CA <- Th_CE_CA/T_Aichi

####Area new target
A_CE_CA <-((T_Aichi*CE_CA_l$n) + (T_Aichi*CE_CA_m$n) + (Th_CE_CA*CE_CA_h$n))/(CE_CA_All$n)*100


                            
                                  ############Southern California #######



# Subset variables by persistence category 

SO_CA_All <- All_mean_region %>% filter (Region...1 == "02_Southern_CA")

SO_CA_l <- All_mean_region_persistence %>% filter (Persistence...2 == "Low", Region...1 == "02_Southern_CA")

SO_CA_m <- All_mean_region_persistence %>% filter (Persistence...2 == "Mid", Region...1 == "02_Southern_CA")

SO_CA_h <- All_mean_region_persistence %>% filter (Persistence...2 == "High", Region...1 == "02_Southern_CA")


#Formula to adjust representation target for present kelp: fixed targets (See paper, equations 4)

T_adj_SO_CA <- T_Aichi*SO_CA_All$Mul*100


#Formula to get the target and multiplier using only persistence for Unfixed targets (See paper, equation 5)

Th_SO_CA <- ((T_Aichi* ((SO_CA_All$n) - (SO_CA_l$per*SO_CA_l$n) - (SO_CA_m$per*SO_CA_m$n))) /  (SO_CA_h$per*SO_CA_h$n))

Mh_SO_CA <- Th_SO_CA/T_Aichi

####Area new target
A_SO_CA <-((T_Aichi*SO_CA_l$n) + (T_Aichi*SO_CA_m$n) + (Th_SO_CA*SO_CA_h$n))/(SO_CA_All$n)*100




                        ############Northern Baja California #######



# Subset variables by persistence category 

NO_BA_All <- All_mean_region %>% filter (Region...1 == "03_Northern_BA")

NO_BA_l <- All_mean_region_persistence %>% filter (Persistence...2 == "Low", Region...1 == "03_Northern_BA")

NO_BA_m <- All_mean_region_persistence %>% filter (Persistence...2 == "Mid", Region...1 == "03_Northern_BA")

NO_BA_h <- All_mean_region_persistence %>% filter (Persistence...2 == "High", Region...1 == "03_Northern_BA")


#Formula to adjust representation target for present kelp: fixed targets (See paper, equations 4)

T_adj_NO_BA <- T_Aichi*NO_BA_All$Mul*100


#Formula to get the target and multiplier using only persistence for Unfixed targets (See paper, equation 5)

Th_NO_BA <- ((T_Aichi* ((NO_BA_All$n) - (NO_BA_l$per*NO_BA_l$n) - (NO_BA_m$per*NO_BA_m$n))) /  (NO_BA_h$per*NO_BA_h$n))

Mh_NO_BA <- Th_NO_BA/T_Aichi

####Area new target
A_NO_BA <- ((T_Aichi*NO_BA_l$n) + (T_Aichi*NO_BA_m$n) + (Th_NO_BA*NO_BA_h$n))/(NO_BA_All$n)*100



                          ############Central Baja California #######



# Subset variables by persistence category 

CE_BA_All <- All_mean_region %>% filter (Region...1 == "04_Central_BA")

CE_BA_l <- All_mean_region_persistence %>% filter (Persistence...2 == "Low", Region...1 == "04_Central_BA")

CE_BA_m <- All_mean_region_persistence %>% filter (Persistence...2 == "Mid", Region...1 == "04_Central_BA")

CE_BA_h <- All_mean_region_persistence %>% filter (Persistence...2 == "High", Region...1 == "04_Central_BA")


#Formula to adjust representation target for present kelp: fixed targets (See paper, equations 4)

T_adj_CE_BA <- T_Aichi*CE_BA_All$Mul*100


#Formula to get the target and multiplier using only persistence for Unfixed targets (See paper, equation 5)

Th_CE_BA <- ((T_Aichi* ((CE_BA_All$n) - (CE_BA_l$per*CE_BA_l$n) - (CE_BA_m$per*CE_BA_m$n))) /  (CE_BA_h$per*CE_BA_h$n))

Mh_CE_BA <- Th_CE_BA/T_Aichi

####Area new target
A_CE_BA <- ((T_Aichi*CE_BA_l$n) + (T_Aichi*CE_BA_m$n) + (Th_CE_BA*CE_BA_h$n))/(CE_BA_All$n)*100

