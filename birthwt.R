library(tidyverse)
library(lme4)
library(ggplot2)
library(lmerTest)
library(glmmTMB)
birthwt<-read.csv("birth_wt_mum_age.csv")

Density<-read.csv("Population_estimate_calculations.csv")
PopD<-Density %>% select (DeerYear,Hinds,Stags,Calves,Calves_M,Calves_F,Adults,Total,LU_Total)

birthwt$MumAgeSquared<-(birthwt$MumAge)*(birthwt$MumAge)
birthwt$DeerYear<-birthwt$BirthYear
birthwt <- birthwt %>%
  left_join(PopD, by = "DeerYear")

Hinds_bw<-glmmTMB(BirthWt~Hinds
               +Sex
               +DaysFrom1May
               +DeerYear
               +(1|DeerYear)
               +(1|MumCode),
               family = gaussian(),data=birthwt)

Adults_bw<-glmmTMB(BirthWt~Adults
                  +Sex
                  +DaysFrom1May
                  +DeerYear
                  +(1|DeerYear)
                  +(1|MumCode),
                  family = gaussian(),data=birthwt)

Total_bw<-glmmTMB(BirthWt~Total
                  +Sex
                  +DaysFrom1May
                  +DeerYear
                  +(1|DeerYear)
                  +(1|MumCode),
                  family = gaussian(),data=birthwt)

LU_Total_bw<-glmmTMB(BirthWt~LU_Total
                   +Sex
                   +DaysFrom1May
                   +DeerYear
                   +(1|DeerYear)
                   +(1|MumCode),
                   family = gaussian(),data=birthwt)

summary(Hinds_bw)
summary(Adults_bw)
summary(Total_bw)
summary(LU_Total_bw)


Age_Hinds_bw<-glmmTMB(BirthWt~Hinds
                      +Sex
                      +DaysFrom1May
                      +DeerYear
                      +MumAge
                      +MumAgeSquared
                      +(1|DeerYear)
                      +(1|MumCode),
                      family = gaussian(),data=birthwt)

Age_Adults_bw<-glmmTMB(BirthWt~Adults
                       +Sex
                       +DaysFrom1May
                       +DeerYear
                       +MumAge
                       +MumAgeSquared
                       +(1|DeerYear)
                       +(1|MumCode),
                       family = gaussian(),data=birthwt)

Age_Total_bw<-glmmTMB(BirthWt~Total
                      +Sex
                      +DaysFrom1May
                      +DeerYear
                      +MumAge
                      +MumAgeSquared
                      +(1|DeerYear)
                      +(1|MumCode),
                      family = gaussian(),data=birthwt)

Age_LU_Total_bw<-glmmTMB(BirthWt~LU_Total
                         +Sex
                         +DaysFrom1May
                         +DeerYear
                         +MumAge
                         +MumAgeSquared
                         +(1|DeerYear)
                         +(1|MumCode),
                         family = gaussian(),data=birthwt)

summary(Age_Hinds_bw)
summary(Age_Adults_bw)
summary(Age_Total_bw)
summary(Age_LU_Total_bw)

Status_Hinds_bw<-glmmTMB(BirthWt~Hinds
                  +Sex
                  +DaysFrom1May
                  +DeerYear
                  +MotherStatus
                  +(1|DeerYear)
                  +(1|MumCode),
                  family = gaussian(),data=birthwt)

Status_Adults_bw<-glmmTMB(BirthWt~Adults
                   +Sex
                   +DaysFrom1May
                   +DeerYear
                   +MotherStatus
                   +(1|DeerYear)
                   +(1|MumCode),
                   family = gaussian(),data=birthwt)

Status_Total_bw<-glmmTMB(BirthWt~Total
                  +Sex
                  +DaysFrom1May
                  +DeerYear
                  +MotherStatus
                  +(1|DeerYear)
                  +(1|MumCode),
                  family = gaussian(),data=birthwt)

Status_LU_Total_bw<-glmmTMB(BirthWt~LU_Total
                     +Sex
                     +DaysFrom1May
                     +DeerYear
                     +MotherStatus
                     +(1|DeerYear)
                     +(1|MumCode),
                     family = gaussian(),data=birthwt)

summary(Status_Hinds_bw)
summary(Status_Adults_bw)
summary(Status_Total_bw)
summary(Status_LU_Total_bw)

Mum_Hinds_bw<-glmmTMB(BirthWt~Hinds
                  +Sex
                  +DaysFrom1May
                  +DeerYear
                  +MumAge
                  +MumAgeSquared
                  +MotherStatus
                  +(1|DeerYear)
                  +(1|MumCode),
                  family = gaussian(),data=birthwt)

Mum_Adults_bw<-glmmTMB(BirthWt~Adults
                   +Sex
                   +DaysFrom1May
                   +DeerYear
                   +MumAge
                   +MumAgeSquared
                   +MotherStatus
                   +(1|DeerYear)
                   +(1|MumCode),
                   family = gaussian(),data=birthwt)

Mum_Total_bw<-glmmTMB(BirthWt~Total
                  +Sex
                  +DaysFrom1May
                  +DeerYear
                  +MumAge
                  +MumAgeSquared
                  +MotherStatus
                  +(1|DeerYear)
                  +(1|MumCode),
                  family = gaussian(),data=birthwt)

Mum_LU_Total_bw<-glmmTMB(BirthWt~LU_Total
                     +Sex
                     +DaysFrom1May
                     +DeerYear
                     +MumAge
                     +MumAgeSquared
                     +MotherStatus
                     +(1|DeerYear)
                     +(1|MumCode),
                     family = gaussian(),data=birthwt)

summary(Mum_Hinds_bw)
summary(Mum_Adults_bw)
summary(Mum_Total_bw)
summary(Mum_LU_Total_bw)


int_Hinds_bw<-glmmTMB(BirthWt~Hinds*MotherStatus
                      +Sex
                      +DaysFrom1May
                      +DeerYear
                      +MumAge
                      +MumAgeSquared
                      +(1|DeerYear)
                      +(1|MumCode),
                      family = gaussian(),data=birthwt)

int_Adults_bw<-glmmTMB(BirthWt~Adults*MotherStatus
                       +Sex
                       +DaysFrom1May
                       +DeerYear
                       +MumAge
                       +MumAgeSquared
                       +(1|DeerYear)
                       +(1|MumCode),
                       family = gaussian(),data=birthwt)

int_Total_bw<-glmmTMB(BirthWt~Total*MotherStatus
                      +Sex
                      +DaysFrom1May
                      +DeerYear
                      +MumAge
                      +MumAgeSquared
                      +(1|DeerYear)
                      +(1|MumCode),
                      family = gaussian(),data=birthwt)

int_LU_Total_bw<-glmmTMB(BirthWt~LU_Total*MotherStatus
                         +Sex
                         +DaysFrom1May
                         +DeerYear
                         +MumAge
                         +MumAgeSquared
                         +(1|DeerYear)
                         +(1|MumCode),
                         family = gaussian(),data=birthwt)

summary(int_Hinds_bw)
summary(int_Adults_bw)
summary(int_Total_bw)
summary(int_LU_Total_bw)




M_birthwt<-birthwt%>% filter(!Sex %in% c(1,3))
F_birthwt<-birthwt%>% filter(!Sex %in% c(2,3))

M_Hinds_bw<-glmmTMB(BirthWt~Hinds
                  +DaysFrom1May
                  +DeerYear
                  +MumAge
                  +MumAgeSquared
                  +MotherStatus
                  +(1|DeerYear)
                  +(1|MumCode),
                  family = gaussian(),data=M_birthwt)

M_Adults_bw<-glmmTMB(BirthWt~Adults
                   +DaysFrom1May
                   +DeerYear
                   +MumAge
                   +MumAgeSquared
                   +MotherStatus
                   +(1|DeerYear)
                   +(1|MumCode),
                   family = gaussian(),data=M_birthwt)

M_Total_bw<-glmmTMB(BirthWt~Total
                  +DaysFrom1May
                  +DeerYear
                  +MumAge
                  +MumAgeSquared
                  +MotherStatus
                  +(1|DeerYear)
                  +(1|MumCode),
                  family = gaussian(),data=M_birthwt)

M_LU_Total_bw<-glmmTMB(BirthWt~LU_Total
                     +DaysFrom1May
                     +DeerYear
                     +MumAge
                     +MumAgeSquared
                     +MotherStatus
                     +(1|DeerYear)
                     +(1|MumCode),
                     family = gaussian(),data=M_birthwt)

summary(M_Hinds_bw)
summary(M_Adults_bw)
summary(M_Total_bw)
summary(M_LU_Total_bw)

F_Hinds_bw<-glmmTMB(BirthWt~Hinds
                  +DaysFrom1May
                  +DeerYear
                  +MumAge
                  +MumAgeSquared
                  +MotherStatus
                  +(1|DeerYear)
                  +(1|MumCode),
                  family = gaussian(),data=F_birthwt)

F_Adults_bw<-glmmTMB(BirthWt~Adults
                   +DaysFrom1May
                   +DeerYear
                   +MumAge
                   +MumAgeSquared
                   +MotherStatus
                   +(1|DeerYear)
                   +(1|MumCode),
                   family = gaussian(),data=F_birthwt)

F_Total_bw<-glmmTMB(BirthWt~Total
                  +DaysFrom1May
                  +DeerYear
                  +MumAge
                  +MumAgeSquared
                  +MotherStatus
                  +(1|DeerYear)
                  +(1|MumCode),
                  family = gaussian(),data=F_birthwt)

F_LU_Total_bw<-glmmTMB(BirthWt~LU_Total
                     +DaysFrom1May
                     +DeerYear
                     +MumAge
                     +MumAgeSquared
                     +MotherStatus
                     +(1|DeerYear)
                     +(1|MumCode),
                     family = gaussian(),data=F_birthwt)

summary(F_Hinds_bw)
summary(F_Adults_bw)
summary(F_Total_bw)
summary(F_LU_Total_bw)


#Simple models

simple_Hinds_bw<-glmmTMB(BirthWt~Hinds
                          +Sex
                          +DaysFrom1May
                          +DeerYear
                          +(1|DeerYear)
                          +(1|MumCode),
                          family = gaussian(),data=birthwt)

simple_Adults_bw<-glmmTMB(BirthWt~Adults
                           +Sex
                           +DaysFrom1May
                           +DeerYear
                           +(1|DeerYear)
                           +(1|MumCode),
                           family = gaussian(),data=birthwt)

simple_Total_bw<-glmmTMB(BirthWt~Total
                          +Sex
                          +DaysFrom1May
                          +DeerYear
                          +(1|DeerYear)
                          +(1|MumCode),
                          family = gaussian(),data=birthwt)

simple_LU_Total_bw<-glmmTMB(BirthWt~LU_Total
                             +Sex
                             +DaysFrom1May
                             +DeerYear
                             +(1|DeerYear)
                             +(1|MumCode),
                             family = gaussian(),data=birthwt)

summary(simple_Hinds_bw)
summary(simple_Adults_bw)
summary(simple_Total_bw)
summary(simple_LU_Total_bw)


Msimple_Hinds_bw<-glmmTMB(BirthWt~Hinds
                          +DaysFrom1May
                          +DeerYear
                          +(1|DeerYear)
                          +(1|MumCode),
                          family = gaussian(),data=M_birthwt)

Msimple_Adults_bw<-glmmTMB(BirthWt~Adults
                           +DaysFrom1May
                           +DeerYear
                           +(1|DeerYear)
                           +(1|MumCode),
                           family = gaussian(),data=M_birthwt)

Msimple_Total_bw<-glmmTMB(BirthWt~Total
                          +DaysFrom1May
                          +DeerYear
                          +(1|DeerYear)
                          +(1|MumCode),
                          family = gaussian(),data=M_birthwt)

Msimple_LU_Total_bw<-glmmTMB(BirthWt~LU_Total
                             +DaysFrom1May
                             +DeerYear
                             +(1|DeerYear)
                             +(1|MumCode),
                             family = gaussian(),data=M_birthwt)

summary(Msimple_Hinds_bw)
summary(Msimple_Adults_bw)
summary(Msimple_Total_bw)
summary(Msimple_LU_Total_bw)


Fsimple_Hinds_bw<-glmmTMB(BirthWt~Hinds
                          +DaysFrom1May
                          +DeerYear
                          +(1|DeerYear)
                          +(1|MumCode),
                          family = gaussian(),data=F_birthwt)

Fsimple_Adults_bw<-glmmTMB(BirthWt~Adults
                           +DaysFrom1May
                           +DeerYear
                           +(1|DeerYear)
                           +(1|MumCode),
                           family = gaussian(),data=F_birthwt)

Fsimple_Total_bw<-glmmTMB(BirthWt~Total
                          +DaysFrom1May
                          +DeerYear
                          +(1|DeerYear)
                          +(1|MumCode),
                          family = gaussian(),data=F_birthwt)

Fsimple_LU_Total_bw<-glmmTMB(BirthWt~LU_Total
                             +DaysFrom1May
                             +DeerYear
                             +(1|DeerYear)
                             +(1|MumCode),
                             family = gaussian(),data=F_birthwt)

summary(Fsimple_Hinds_bw)
summary(Fsimple_Adults_bw)
summary(Fsimple_Total_bw)
summary(Fsimple_LU_Total_bw)
