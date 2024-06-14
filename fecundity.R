library(tidyverse)
library(lme4)
library(ggplot2)
library(lmerTest)
library(glmmTMB)

fecundity<-read.csv("Fecundity.csv")

Density<-read.csv("Population_estimate_calculations.csv")

PopD<-Density %>% select (DeerYear,Hinds,Adults,Total,LU_Total)

fecundity <- fecundity %>%
  left_join(PopD, by = "DeerYear")

fecundity$AgeSquared<-(fecundity$Age)*(fecundity$Age)

fecundity<-fecundity%>% filter(Age > 2)


fecundity$S_Hinds<-scale(fecundity$Hinds)
fecundity$S_Adults<-scale(fecundity$Adults)
fecundity$S_Total<-scale(fecundity$Total)
fecundity$S_LU_Total<-scale(fecundity$LU_Total)
fecundity$S_Age<-scale(fecundity$Age)
fecundity$S_AgeSquared<-scale(fecundity$AgeSquared)

rut <- fecundity %>% filter(SeenInRut==-1)

milk <- rut %>% filter(ReprodStatus=="Milk")

milk_mod<-glmmTMB(Fecundity~S_Hinds
                         +(1|DeerYear)
                         +(1|Female),
                         family = binomial(link = "logit"),data=milk)
summary(milk_mod)


yeld <- rut %>% filter(ReprodStatus %in% c("Winter yeld", "Summer yeld","True yeld"))

yeld_mod<-glmmTMB(Fecundity~S_Hinds
                  +ReprodStatus
                  +(1|DeerYear)
                  +(1|Female),
                  family = binomial(link = "logit"),data=yeld)
summary(yeld_mod)

naive<- rut %>% filter(!ReprodStatus %in% c("Milk","Winter yeld", "Summer yeld","True yeld") )
naive_mod<-glmmTMB(Fecundity~S_Hinds
                  +(1|DeerYear)
                  +(1|Female),
                  family = binomial(link = "logit"),data=naive)
summary(naive_mod)


Hinds_fecundity<-glmmTMB(Fecundity~S_Hinds
                         +ReprodStatus
                         +(1|DeerYear)
                         +(1|Female),
                         family = binomial(link = "logit"),data=rut)
summary(Hinds_fecundity)

Adults_fecundity<-glmmTMB(Fecundity~S_Adults
                         +ReprodStatus
                         +(1|DeerYear)
                         +(1|Female),
                         family = binomial(link = "logit"),data=rut)
summary(Adults_fecundity)

Total_fecundity<-glmmTMB(Fecundity~S_Total
                         +ReprodStatus
                         +(1|DeerYear)
                         +(1|Female),
                         family = binomial(link = "logit"),data=rut)
summary(Total_fecundity)

LU_Total_fecundity<-glmmTMB(Fecundity~S_LU_Total
                         +ReprodStatus
                         +(1|DeerYear)
                         +(1|Female),
                         family = binomial(link = "logit"),data=rut)
summary(LU_Total_fecundity)
