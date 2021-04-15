###Written by -----Nur Arafeh Dalmau----2021
## The University of Queensland
##Paper in Communications Earth & Environment
### Code for plotting barplots for protection/persistence data in the Northeast Pacific Ocean and the four regions 
###(Barplots for Figure 1-2)


###Load packages

library(ggplot2)
library(tidyverse)


##All_Figure 1a

All_MPA <- read.csv("Kelp_MPAs_Revised.csv") %>% filter(Region == "All", Status == "Partial" | Status == "Full") 

All_MPA<-All_MPA[!(All_MPA$Status=="None"),]
All_MPA<-All_MPA[!(All_MPA$Persistence=="All_Kelp"),]

All_MPA$Persistence <- factor(All_MPA$Persistence, levels = c("High", "Mid", "Low", "All"))
All_MPA$Status <- factor(All_MPA$Status, levels = c("Partial", "Full"))

p1 <- ggplot(All_MPA, aes(x =Persistence, y = Percentage)) + 
  geom_col(aes(fill = Status), width = 0.7)  


p1 + theme_bw() +  ylim(0, 20)+  
  theme(
        axis.text=element_text(size=16),
        axis.title=element_text(size=16, face="bold")) +
  scale_fill_manual(values=c("#003366", "#FF6666"))


##Just Kelp by region-Figure 1b


Kelp <- read.csv("Kelp_All.csv")


Kelp$Persistence <- factor(Kelp$Persistence, levels = c("Low", "Mid", "High"))
Kelp$Region <- factor(Kelp$Region, levels = c("Central_CA" , "Southern_CA" , "Northern_BA" , "Central_BA"))

p2 <- ggplot(Kelp, aes(x =Region, y = Percentage))+
  geom_col(aes(fill = Persistence), width = 0.7)


p2 + theme_bw() +
  theme(
    axis.text=element_text(size=16),
    axis.title=element_text(size=16, face="bold")) +
  scale_fill_manual(values=c("#99CC66","#009966","#006633"))


#Kelp contribution_ Figure 1c

Kelp_Con <- read.csv("Kelp_MPAs_Contribution.csv") %>% filter(Status == 'Full')  

Kelp_Con$Persistence <- factor(Kelp_Con$Persistence, levels = c("High", "Mid", "Low", "All",))
Kelp_Con$Region <- factor(Kelp_Con$Region, levels = c("Central_CA" , "Southern_CA" , "Northern_BA" , "Central_BA"))

p3 <- ggplot(Kelp_Con, aes(x = Persistence, y = Percentage))+
  geom_col(aes(fill = Region), width = 0.7)


p3 + theme_bw() +
 theme(axis.text=element_text(size=16),
                       axis.title=element_text(size=16, face="bold")) +
  ylim(0, 15) + 
  scale_fill_manual(values=c("#000033", "#99CCFF", "#3399FF", "#006699"))




#####Figures2 regions 
##Central_California

C_CA_MPA <- read.csv("Kelp_MPAs_Revised.csv") %>% filter(Region == 'Central_CA')  
C_CA_MPA<-C_CA_MPA[!(C_CA_MPA$Persistence =="All"),]

C_CA_MPA$Persistence <- factor(C_CA_MPA$Persistence, levels = c("Low", "Mid", "High"))
C_CA_MPA$Status <- factor(C_CA_MPA$Status, levels = c("None", "Partial", "Full"))

p4 <- ggplot(C_CA_MPA, aes(x =Persistence, y = Percentage))+
  geom_col(aes(fill = Status), width = 0.7)


p4 + theme_bw() +
  coord_flip() + theme(legend.position = "none", 
                       axis.text=element_text(size=16),
                       axis.title=element_text(size=16, face="bold")) +
  scale_fill_manual(values=c("#CCCCCC", "#003366", "#FF6666"))
  

#Southern_California
S_CA_MPA <- read.csv("Kelp_MPAs_Revised.csv") %>% filter(Region == 'Southern_CA')  
S_CA_MPA<-S_CA_MPA[!(S_CA_MPA$Persistence=="All"),]

S_CA_MPA$Persistence <- factor(S_CA_MPA$Persistence, levels = c("Low", "Mid", "High"))
S_CA_MPA$Status <- factor(S_CA_MPA$Status, levels = c("None", "Partial", "Full"))

p5 <- ggplot(S_CA_MPA, aes(x =Persistence, y = Percentage))+
  geom_col(aes(fill = Status), width = 0.7)

p5 + theme_bw() +
  coord_flip() + theme(legend.position = "none", 
                       axis.text=element_text(size=16),
                       axis.title=element_text(size=16, face="bold")) +
  scale_fill_manual(values=c("#CCCCCC", "#003366", "#FF6666"))

#NortherBaja California
N_BA_MPA <- read.csv("Kelp_MPAs_Revised.csv") %>% filter(Region == 'Northern_BA')  
N_BA_MPA<-N_BA_MPA[!(N_BA_MPA$Persistence=="All"),]

N_BA_MPA$Persistence <- factor(N_BA_MPA$Persistence, levels = c("Low", "Mid", "High"))
N_BA_MPA$Status <- factor(N_BA_MPA$Status, levels = c("None", "Partial", "Full"))

p6 <- ggplot(N_BA_MPA, aes(x =Persistence, y = Percentage))+
  geom_col(aes(fill = Status), width = 0.7)

p6 + theme_bw() +
  coord_flip() + theme(legend.position = "none", 
                       axis.text=element_text(size=16),
                       axis.title=element_text(size=16, face="bold")) +
  scale_fill_manual(values=c("#CCCCCC", "#003366", "#FF6666"))

#Central Baja California
C_BA_MPA <- read.csv("Kelp_MPAs_Revised.csv") %>% filter(Region == 'Central_BA')  
C_BA_MPA<-C_BA_MPA[!(C_BA_MPA$Persistence=="All"),]

C_BA_MPA$Persistence <- factor(C_BA_MPA$Persistence, levels = c("Low", "Mid", "High"))
C_BA_MPA$Status <- factor(C_BA_MPA$Status, levels = c("None", "Partial", "Full"))

p7 <- ggplot(C_BA_MPA, aes(x =Persistence, y = Percentage))+
  geom_col(aes(fill = Status), width = 0.7)

p7 + theme_bw() +
  coord_flip() + theme(legend.position = "none", 
                       axis.text=element_text(size=16),
                       axis.title=element_text(size=16, face="bold")) +
  scale_fill_manual(values=c("#CCCCCC", "#003366", "#FF6666"))

