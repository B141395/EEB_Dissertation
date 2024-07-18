library(tidyverse)
library(lme4)
library(ggplot2)
library(lmerTest)
library(glmmTMB)

Spike_Mum<-read.csv("First_Year_Spike_Mum.csv")
Density<-read.csv("Population_estimate_calculations.csv")
PopD<-Density %>% select (DeerYear,Hinds,Stags,Calves,Calves_M,Calves_F,Adults,Total,LU_Total)
Spike_Mum<-Spike_Mum%>% filter(!DeerYear %in% c(1969))

Spike_Mum <- Spike_Mum %>%
  left_join(PopD, by = "DeerYear")


Hinds_spike_Mum<-glmmTMB(AvgSpike~Hinds
                     +DeerYear
                     +ReprodStatus
                     +(1|DeerYear)
                     +(1|MumCode),data=Spike_Mum)

Adults_spike_Mum<-glmmTMB(AvgSpike~Adults
                         +DeerYear
                         +ReprodStatus
                         +(1|DeerYear)
                         +(1|MumCode),data=Spike_Mum)
Total_spike_Mum<-glmmTMB(AvgSpike~Total
                          +DeerYear
                          +ReprodStatus
                          +(1|DeerYear)
                          +(1|MumCode),data=Spike_Mum)
LU_Total_spike_Mum<-glmmTMB(AvgSpike~LU_Total
                          +DeerYear
                          +ReprodStatus
                          +(1|DeerYear)
                          +(1|MumCode),data=Spike_Mum)
summary(Hinds_spike_Mum)
summary(Adults_spike_Mum)
summary(Total_spike_Mum)
summary(LU_Total_spike_Mum)

Hinds_spike_Mum<-glmmTMB(AvgSpike~Hinds
                     +DeerYear
                     +ReprodStatus
                     +(1|DeerYear)
                     +(1|MumCode),data=Spike_Mum)

Adults_spike_Mum<-glmmTMB(AvgSpike~Adults
                         +DeerYear
                         +ReprodStatus
                         +(1|DeerYear)
                         +(1|MumCode),data=Spike_Mum)
Total_spike_Mum<-glmmTMB(AvgSpike~Total
                          +DeerYear
                          +ReprodStatus
                          +(1|DeerYear)
                          +(1|MumCode),data=Spike_Mum)
LU_Total_spike_Mum<-glmmTMB(AvgSpike~LU_Total
                          +DeerYear
                          +ReprodStatus
                          +(1|DeerYear)
                          +(1|MumCode),data=Spike_Mum)
summary(Hinds_spike_Mum)
summary(Adults_spike_Mum)
summary(Total_spike_Mum)
summary(LU_Total_spike_Mum)
