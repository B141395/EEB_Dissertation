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

fecundity_mod<-glmmTMB(Fecundity~S_Adults
                  +ReprodStatus
                  +(1|DeerYear)
                  +(1|Female),
                  family = binomial(link = "logit"),data=fecundity)
summary(fecundity_mod)

rut <- fecundity %>% filter(SeenInRut==-1)

milk <- rut %>% filter(ReprodStatus=="Milk")

summer_yeld <- rut %>% filter(ReprodStatus =="Summer yeld")

winter_yeld <- rut %>% filter(ReprodStatus =="Winter yeld")

true_yeld <- rut %>% filter(ReprodStatus =="True yeld")

naive<- rut %>% filter(!ReprodStatus %in% c("Milk","Winter yeld", "Summer yeld","True yeld") )

#prelim model to test----

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

#Milk----
milk_Hinds<-glmmTMB(Fecundity~S_Hinds+Age+AgeSquared+DeerYear
                  +(1|DeerYear)
                  +(1|Female),
                  family = binomial(link = "logit"),data=milk)
milk_Adults<-glmmTMB(Fecundity~S_Adults+Age+AgeSquared+DeerYear
                  +(1|DeerYear)
                  +(1|Female),
                  family = binomial(link = "logit"),data=milk)
milk_Total<-glmmTMB(Fecundity~S_Total+Age+AgeSquared+DeerYear
                  +(1|DeerYear)
                  +(1|Female),
                  family = binomial(link = "logit"),data=milk)
milk_LU_Total<-glmmTMB(Fecundity~S_LU_Total+Age+AgeSquared+DeerYear
                  +(1|DeerYear)
                  +(1|Female),
                  family = binomial(link = "logit"),data=milk)
summary(milk_Hinds)
summary(milk_Adults)
summary(milk_Total)
summary(milk_LU_Total)

milk_Hinds_noage<-glmmTMB(Fecundity~S_Hinds+DeerYear
                    +(1|DeerYear)
                    +(1|Female),
                    family = binomial(link = "logit"),data=milk)
milk_Adults_noage<-glmmTMB(Fecundity~S_Adults+DeerYear
                     +(1|DeerYear)
                     +(1|Female),
                     family = binomial(link = "logit"),data=milk)
milk_Total_noage<-glmmTMB(Fecundity~S_Total+DeerYear
                    +(1|DeerYear)
                    +(1|Female),
                    family = binomial(link = "logit"),data=milk)
milk_LU_Total_noage<-glmmTMB(Fecundity~S_LU_Total+DeerYear
                       +(1|DeerYear)
                       +(1|Female),
                       family = binomial(link = "logit"),data=milk)
summary(milk_Hinds_noage)
summary(milk_Adults_noage)
summary(milk_Total_noage)
summary(milk_LU_Total_noage)

milk_Hinds_noage_yr<-glmmTMB(Fecundity~S_Hinds
                          +(1|DeerYear)
                          +(1|Female),
                          family = binomial(link = "logit"),data=milk)
milk_Adults_noage_yr<-glmmTMB(Fecundity~S_Adults
                           +(1|DeerYear)
                           +(1|Female),
                           family = binomial(link = "logit"),data=milk)
milk_Total_noage_yr<-glmmTMB(Fecundity~S_Total
                          +(1|DeerYear)
                          +(1|Female),
                          family = binomial(link = "logit"),data=milk)
milk_LU_Total_noage_yr<-glmmTMB(Fecundity~S_LU_Total
                             +(1|DeerYear)
                             +(1|Female),
                             family = binomial(link = "logit"),data=milk)
summary(milk_Hinds_noage_yr)
summary(milk_Adults_noage_yr)
summary(milk_Total_noage_yr)
summary(milk_LU_Total_noage_yr)

#Summer yeld----
summer_Hinds<-glmmTMB(Fecundity~S_Hinds+Age+AgeSquared+DeerYear
                   +(1|DeerYear)
                   +(1|Female),
                   family = binomial(link = "logit"),data=summer_yeld)
summer_Adults<-glmmTMB(Fecundity~S_Adults+Age+AgeSquared+DeerYear
                      +(1|DeerYear)
                      +(1|Female),
                      family = binomial(link = "logit"),data=summer_yeld)
summer_Total<-glmmTMB(Fecundity~S_Total+Age+AgeSquared+DeerYear
                      +(1|DeerYear)
                      +(1|Female),
                      family = binomial(link = "logit"),data=summer_yeld)
summer_LU_Total<-glmmTMB(Fecundity~S_LU_Total+Age+AgeSquared+DeerYear
                      +(1|DeerYear)
                      +(1|Female),
                      family = binomial(link = "logit"),data=summer_yeld)
summary(summer_Hinds)
summary(summer_Adults)
summary(summer_Total)
summary(summer_LU_Total)

summer_Hinds_noage<-glmmTMB(Fecundity~S_Hinds+DeerYear
                      +(1|DeerYear)
                      +(1|Female),
                      family = binomial(link = "logit"),data=summer_yeld)
summer_Adults_noage<-glmmTMB(Fecundity~S_Adults+DeerYear
                       +(1|DeerYear)
                       +(1|Female),
                       family = binomial(link = "logit"),data=summer_yeld)
summer_Total_noage<-glmmTMB(Fecundity~S_Total+DeerYear
                      +(1|DeerYear)
                      +(1|Female),
                      family = binomial(link = "logit"),data=summer_yeld)
summer_LU_Total_noage<-glmmTMB(Fecundity~S_LU_Total+DeerYear
                         +(1|DeerYear)
                         +(1|Female),
                         family = binomial(link = "logit"),data=summer_yeld)
summary(summer_Hinds_noage)
summary(summer_Adults_noage)
summary(summer_Total_noage)
summary(summer_LU_Total_noage)

summer_Hinds_noage_yr<-glmmTMB(Fecundity~S_Hinds
                            +(1|DeerYear)
                            +(1|Female),
                            family = binomial(link = "logit"),data=summer_yeld)
summer_Adults_noage_yr<-glmmTMB(Fecundity~S_Adults
                             +(1|DeerYear)
                             +(1|Female),
                             family = binomial(link = "logit"),data=summer_yeld)
summer_Total_noage_yr<-glmmTMB(Fecundity~S_Total
                            +(1|DeerYear)
                            +(1|Female),
                            family = binomial(link = "logit"),data=summer_yeld)
summer_LU_Total_noage_yr<-glmmTMB(Fecundity~S_LU_Total
                               +(1|DeerYear)
                               +(1|Female),
                               family = binomial(link = "logit"),data=summer_yeld)
summary(summer_Hinds_noage_yr)
summary(summer_Adults_noage_yr)
summary(summer_Total_noage_yr)
summary(summer_LU_Total_noage_yr)

#Winter yeld----
winter_Hinds<-glmmTMB(Fecundity~S_Hinds+Age+AgeSquared+DeerYear
                    +(1|DeerYear)
                    +(1|Female),
                    family = binomial(link = "logit"),data=winter_yeld)
winter_Adults<-glmmTMB(Fecundity~S_Adults+Age+AgeSquared+DeerYear
                      +(1|DeerYear)
                      +(1|Female),
                      family = binomial(link = "logit"),data=winter_yeld)
winter_Total<-glmmTMB(Fecundity~S_Total+Age+AgeSquared+DeerYear
                      +(1|DeerYear)
                      +(1|Female),
                      family = binomial(link = "logit"),data=winter_yeld)
winter_LU_Total<-glmmTMB(Fecundity~S_LU_Total+Age+AgeSquared+DeerYear
                      +(1|DeerYear)
                      +(1|Female),
                      family = binomial(link = "logit"),data=winter_yeld)
summary(winter_Hinds)
summary(winter_Adults)
summary(winter_Total)
summary(winter_LU_Total)

winter_Hinds_noage<-glmmTMB(Fecundity~S_Hinds+DeerYear
                      +(1|DeerYear)
                      +(1|Female),
                      family = binomial(link = "logit"),data=winter_yeld)
winter_Adults_noage<-glmmTMB(Fecundity~S_Adults+DeerYear
                       +(1|DeerYear)
                       +(1|Female),
                       family = binomial(link = "logit"),data=winter_yeld)
winter_Total_noage<-glmmTMB(Fecundity~S_Total+DeerYear
                      +(1|DeerYear)
                      +(1|Female),
                      family = binomial(link = "logit"),data=winter_yeld)
winter_LU_Total_noage<-glmmTMB(Fecundity~S_LU_Total+DeerYear
                         +(1|DeerYear)
                         +(1|Female),
                         family = binomial(link = "logit"),data=winter_yeld)
summary(winter_Hinds_noage)
summary(winter_Adults_noage)
summary(winter_Total_noage)
summary(winter_LU_Total_noage)

winter_Hinds_noage_yr<-glmmTMB(Fecundity~S_Hinds
                            +(1|DeerYear)
                            +(1|Female),
                            family = binomial(link = "logit"),data=winter_yeld)
winter_Adults_noage_yr<-glmmTMB(Fecundity~S_Adults
                             +(1|DeerYear)
                             +(1|Female),
                             family = binomial(link = "logit"),data=winter_yeld)
winter_Total_noage_yr<-glmmTMB(Fecundity~S_Total
                            +(1|DeerYear)
                            +(1|Female),
                            family = binomial(link = "logit"),data=winter_yeld)
winter_LU_Total_noage_yr<-glmmTMB(Fecundity~S_LU_Total
                               +(1|DeerYear)
                               +(1|Female),
                               family = binomial(link = "logit"),data=winter_yeld)
summary(winter_Hinds_noage_yr)
summary(winter_Adults_noage_yr)
summary(winter_Total_noage_yr)
summary(winter_LU_Total_noage_yr)

#True yeld----
true_Hinds<-glmmTMB(Fecundity~S_Hinds+Age+AgeSquared+DeerYear
                    +(1|DeerYear)
                    +(1|Female),
                    family = binomial(link = "logit"),data=true_yeld)
true_Adults<-glmmTMB(Fecundity~S_Adults+Age+AgeSquared+DeerYear
                  +(1|DeerYear)
                  +(1|Female),
                  family = binomial(link = "logit"),data=true_yeld)
true_Total<-glmmTMB(Fecundity~S_Total+Age+AgeSquared+DeerYear
                  +(1|DeerYear)
                  +(1|Female),
                  family = binomial(link = "logit"),data=true_yeld)
true_LU_Total<-glmmTMB(Fecundity~S_LU_Total+Age+AgeSquared+DeerYear
                  +(1|DeerYear)
                  +(1|Female),
                  family = binomial(link = "logit"),data=true_yeld)
summary(true_Hinds)
summary(true_Adults)
summary(true_Total)
summary(true_LU_Total)

true_Hinds_noage<-glmmTMB(Fecundity~S_Hinds+DeerYear
                    +(1|DeerYear)
                    +(1|Female),
                    family = binomial(link = "logit"),data=true_yeld)
true_Adults_noage<-glmmTMB(Fecundity~S_Adults+DeerYear
                     +(1|DeerYear)
                     +(1|Female),
                     family = binomial(link = "logit"),data=true_yeld)
true_Total_noage<-glmmTMB(Fecundity~S_Total+DeerYear
                    +(1|DeerYear)
                    +(1|Female),
                    family = binomial(link = "logit"),data=true_yeld)
true_LU_Total_noage<-glmmTMB(Fecundity~S_LU_Total+DeerYear
                       +(1|DeerYear)
                       +(1|Female),
                       family = binomial(link = "logit"),data=true_yeld)
summary(true_Hinds_noage)
summary(true_Adults_noage)
summary(true_Total_noage)
summary(true_LU_Total_noage)

true_Hinds_noage_yr<-glmmTMB(Fecundity~S_Hinds
                          +(1|DeerYear)
                          +(1|Female),
                          family = binomial(link = "logit"),data=true_yeld)
true_Adults_noage_yr<-glmmTMB(Fecundity~S_Adults
                           +(1|DeerYear)
                           +(1|Female),
                           family = binomial(link = "logit"),data=true_yeld)
true_Total_noage_yr<-glmmTMB(Fecundity~S_Total
                          +(1|DeerYear)
                          +(1|Female),
                          family = binomial(link = "logit"),data=true_yeld)
true_LU_Total_noage_yr<-glmmTMB(Fecundity~S_LU_Total
                             +(1|DeerYear)
                             +(1|Female),
                             family = binomial(link = "logit"),data=true_yeld)
summary(true_Hinds_noage_yr)
summary(true_Adults_noage_yr)
summary(true_Total_noage_yr)
summary(true_LU_Total_noage_yr)

#naive models----
naive_Hinds<-glmmTMB(Fecundity~S_Hinds+Age+AgeSquared+DeerYear
                     +(1|DeerYear)
                     +(1|Female),
                     family = binomial(link = "logit"),data=naive)
naive_Adults<-glmmTMB(Fecundity~S_Adults+Age+AgeSquared+DeerYear
                      +(1|DeerYear)
                      +(1|Female),
                      family = binomial(link = "logit"),data=naive)
naive_Total<-glmmTMB(Fecundity~S_Total+Age+AgeSquared+DeerYear
                     +(1|DeerYear)
                     +(1|Female),
                     family = binomial(link = "logit"),data=naive)
naive_LU_Total<-glmmTMB(Fecundity~S_LU_Total+Age+AgeSquared+DeerYear
                        +(1|DeerYear)
                        +(1|Female),
                        family = binomial(link = "logit"),data=naive)
summary(naive_Hinds)
summary(naive_Adults)
summary(naive_Total)
summary(naive_LU_Total)

naive_Hinds_noage<-glmmTMB(Fecundity~S_Hinds+DeerYear
                           +(1|DeerYear)
                           +(1|Female),
                           family = binomial(link = "logit"),data=naive)
naive_Adults_noage<-glmmTMB(Fecundity~S_Adults+DeerYear
                            +(1|DeerYear)
                            +(1|Female),
                            family = binomial(link = "logit"),data=naive)
naive_Total_noage<-glmmTMB(Fecundity~S_Total+DeerYear
                           +(1|DeerYear)
                           +(1|Female),
                           family = binomial(link = "logit"),data=naive)
naive_LU_Total_noage<-glmmTMB(Fecundity~S_LU_Total+DeerYear
                              +(1|DeerYear)
                              +(1|Female),
                              family = binomial(link = "logit"),data=naive)
summary(naive_Hinds_noage)
summary(naive_Adults_noage)
summary(naive_Total_noage)
summary(naive_LU_Total_noage)

naive_Hinds_noage_yr<-glmmTMB(Fecundity~S_Hinds
                              +(1|DeerYear)
                              +(1|Female),
                              family = binomial(link = "logit"),data=naive)
naive_Adults_noage_yr<-glmmTMB(Fecundity~S_Adults
                               +(1|DeerYear)
                               +(1|Female),
                               family = binomial(link = "logit"),data=naive)
naive_Total_noage_yr<-glmmTMB(Fecundity~S_Total
                              +(1|DeerYear)
                              +(1|Female),
                              family = binomial(link = "logit"),data=naive)
naive_LU_Total_noage_yr<-glmmTMB(Fecundity~S_LU_Total
                                 +(1|DeerYear)
                                 +(1|Female),
                                 family = binomial(link = "logit"),data=naive)
summary(naive_Hinds_noage_yr)
summary(naive_Adults_noage_yr)
summary(naive_Total_noage_yr)
summary(naive_LU_Total_noage_yr)

#with age----

milk_mod_age<-glmmTMB(Fecundity~S_Hinds+Age+AgeSquared
                  +(1|DeerYear)
                  +(1|Female),
                  family = binomial(link = "logit"),data=milk)
summary(milk_mod_age)

yeld_mod_age<-glmmTMB(Fecundity~S_Hinds+Age+AgeSquared
                  +ReprodStatus
                  +(1|DeerYear)
                  +(1|Female),
                  family = binomial(link = "logit"),data=yeld)
summary(yeld_mod_age)

naive_mod_age<-glmmTMB(Fecundity~S_Hinds+Age+AgeSquared
                   +(1|DeerYear)
                   +(1|Female),
                   family = binomial(link = "logit"),data=naive)
summary(naive_mod_age)

#models----
Hinds_fecundity<-glmmTMB(Fecundity~S_Hinds+ReprodStatus+(1|DeerYear)+(1|Female),family = binomial(link = "logit"),data=rut)

Adults_fecundity<-glmmTMB(Fecundity~S_Adults+ReprodStatus+(1|DeerYear)+(1|Female),family = binomial(link = "logit"),data=rut)

Total_fecundity<-glmmTMB(Fecundity~S_Total+ReprodStatus+(1|DeerYear)+(1|Female),family = binomial(link = "logit"),data=rut)

LU_Total_fecundity<-glmmTMB(Fecundity~S_LU_Total+ReprodStatus+(1|DeerYear)+(1|Female),family = binomial(link = "logit"),data=rut)

summary(Hinds_fecundity)
summary(Adults_fecundity)
summary(Total_fecundity)
summary(LU_Total_fecundity)

#models with additional year
yr_Hinds_fecundity<-glmmTMB(Fecundity~S_Hinds+ReprodStatus+DeerYear+(1|DeerYear)+(1|Female),family = binomial(link = "logit"),data=rut)

yr_Adults_fecundity<-glmmTMB(Fecundity~S_Adults+ReprodStatus+DeerYear+(1|DeerYear)+(1|Female),family = binomial(link = "logit"),data=rut)

yr_Total_fecundity<-glmmTMB(Fecundity~S_Total+ReprodStatus+DeerYear+(1|DeerYear)+(1|Female),family = binomial(link = "logit"),data=rut)

yr_LU_Total_fecundity<-glmmTMB(Fecundity~S_LU_Total+ReprodStatus+DeerYear+(1|DeerYear)+(1|Female),family = binomial(link = "logit"),data=rut)

summary(yr_Hinds_fecundity)
summary(yr_Adults_fecundity)
summary(yr_Total_fecundity)
summary(yr_LU_Total_fecundity)

#with age no year
Hinds_fecundity_age<-glmmTMB(Fecundity~S_Hinds+Age+AgeSquared+ReprodStatus+(1|DeerYear)+(1|Female),family = binomial(link = "logit"),data=rut)

Adults_fecundity_age<-glmmTMB(Fecundity~S_Adults+Age+AgeSquared+ReprodStatus+(1|DeerYear)+(1|Female),family = binomial(link = "logit"),data=rut)

Total_fecundity_age<-glmmTMB(Fecundity~S_Total+Age+AgeSquared+ReprodStatus+(1|DeerYear)+(1|Female),family = binomial(link = "logit"),data=rut)

LU_Total_fecundity_age<-glmmTMB(Fecundity~S_LU_Total+Age+AgeSquared+ReprodStatus+(1|DeerYear)+(1|Female),family = binomial(link = "logit"),data=rut)

summary(Hinds_fecundity_age)
summary(Adults_fecundity_age)
summary(Total_fecundity_age)
summary(LU_Total_fecundity_age)

#models with age and additional year
yr_Hinds_fecundity_age<-glmmTMB(Fecundity~S_Hinds+Age+AgeSquared+ReprodStatus+DeerYear+(1|DeerYear)+(1|Female),family = binomial(link = "logit"),data=rut)

yr_Adults_fecundity_age<-glmmTMB(Fecundity~S_Adults+Age+AgeSquared+ReprodStatus+DeerYear+(1|DeerYear)+(1|Female),family = binomial(link = "logit"),data=rut)

yr_Total_fecundity_age<-glmmTMB(Fecundity~S_Total+Age+AgeSquared+ReprodStatus+DeerYear+(1|DeerYear)+(1|Female),family = binomial(link = "logit"),data=rut)

yr_LU_Total_fecundity_age<-glmmTMB(Fecundity~S_LU_Total+Age+AgeSquared+ReprodStatus+DeerYear+(1|DeerYear)+(1|Female),family = binomial(link = "logit"),data=rut)

summary(yr_Hinds_fecundity_age)
summary(yr_Adults_fecundity_age)
summary(yr_Total_fecundity_age)
summary(yr_LU_Total_fecundity_age)


portion<- rut %>%
  group_by(DeerYear) %>%
  summarize(
    count = n(),                         
    reproduced = sum(Fecundity, na.rm = TRUE),
    portion = reproduced/count,
  )

portion<- portion %>% filter(!DeerYear %in% c(1961, 1962,1963,1964,1965,1966,1967,1968,1969))

portion <- portion %>%
  left_join(PopD, by = "DeerYear")

ggplot(portion, aes(x = DeerYear, y = portion)) +
  geom_point()+
  geom_smooth(method = "lm",color = "black", size = 0.75) +
  labs(title="Proportion of females seen in rut reproduced over Year")+
  ylab("Proportion of females reproduced") + xlab("Year")+
  theme_minimal() +
  geom_text(aes(y = portion+ 0.03, label = paste0("(", DeerYear, ")")), size = 2.5, color = "black") 

ggplot(portion, aes(x = Hinds, y = portion)) +
  geom_point()+
  geom_smooth(method = "lm",color = "black", size = 0.75) +
  labs(title="Proportion of females seen in rut reproduced over Density")+
  ylab("Proportion of females reproduced") + xlab("Hind Density")+
  theme_minimal() +
  geom_text(aes(y = portion+ 0.03, label = paste0("(", DeerYear, ")")), size = 2.5, color = "black") 

ggplot(portion, aes(x = Adults, y = portion)) +
  geom_point()+
  geom_smooth(method = "lm",color = "black", size = 0.75) +
  labs(title="Proportion of females seen in rut reproduced over Density")+
  ylab("Proportion of females reproduced") + xlab("Adult Density")+
  theme_minimal() +
  geom_text(aes(y = portion+ 0.03, label = paste0("(", DeerYear, ")")), size = 2.5, color = "black") 

ggplot(portion, aes(x = Total, y = portion)) +
  geom_point()+
  geom_smooth(method = "lm",color = "black", size = 0.75) +
  labs(title="Proportion of females seen in rut reproduced over Density")+
  ylab("Proportion of females reproduced") + xlab("Total Density")+
  theme_minimal() +
  geom_text(aes(y = portion+ 0.03, label = paste0("(", DeerYear, ")")), size = 2.5, color = "black") 

ggplot(portion, aes(x = LU_Total, y = portion)) +
  geom_point()+
  geom_smooth(method = "lm",color = "black", size = 0.75) +
  labs(title="Proportion of females seen in rut reproduced over Density")+
  ylab("Proportion of females reproduced") + xlab("Livestock units")+
  theme_minimal() +
  geom_text(aes(y = portion+ 0.03, label = paste0("(", DeerYear, ")")), size = 2.5, color = "black") 

portion_milk<- milk %>%
  group_by(DeerYear) %>%
  summarize(
    count = n(),                         
    reproduced = sum(Fecundity, na.rm = TRUE),
    milk = reproduced/count,
  )

portion_milk<-portion_milk %>% select(DeerYear,milk)

portion_summer_yeld<- summer_yeld %>%
  group_by(DeerYear) %>%
  summarize(
    count = n(),                         
    reproduced = sum(Fecundity, na.rm = TRUE),
    summer_yeld = reproduced/count,
  )

portion_summer_yeld<-portion_summer_yeld %>% select(DeerYear,summer_yeld)

portion_winter_yeld<- winter_yeld %>%
  group_by(DeerYear) %>%
  summarize(
    count = n(),                         
    reproduced = sum(Fecundity, na.rm = TRUE),
    winter_yeld = reproduced/count,
  )

portion_winter_yeld<-portion_winter_yeld %>% select(DeerYear,winter_yeld)


portion_true_yeld<- true_yeld %>%
  group_by(DeerYear) %>%
  summarize(
    count = n(),                         
    reproduced = sum(Fecundity, na.rm = TRUE),
    true_yeld = reproduced/count,
  )

portion_true_yeld<-portion_true_yeld %>% select(DeerYear,true_yeld)

portion_naive<- naive %>%
  group_by(DeerYear) %>%
  summarize(
    count = n(),                         
    reproduced = sum(Fecundity, na.rm = TRUE),
    naive = reproduced/count,
  )

portion_naive<-portion_naive %>% select(DeerYear,naive)

portion<-portion%>%
  left_join(portion_milk, by = "DeerYear")

portion<-portion%>%
  left_join(portion_summer_yeld, by = "DeerYear")

portion<-portion%>%
  left_join(portion_winter_yeld, by = "DeerYear")

portion<-portion%>%
  left_join(portion_true_yeld, by = "DeerYear")

portion<-portion%>%
  left_join(portion_naive, by = "DeerYear")


ggplot(portion, aes(x = DeerYear)) +
  geom_point(aes(y = milk, color = "milk"))+
  geom_point(aes(y = summer_yeld, color = "summer_yeld"))+
  geom_point(aes(y = winter_yeld, color = "winter_yeld"))+
  geom_point(aes(y = true_yeld, color = "true_yeld"))+
  geom_point(aes(y = naive, color = "naive"))+
  geom_smooth(method = "lm",aes(y = milk, color = "milk"), size = 2) +
  geom_smooth(method = "lm",aes(y = summer_yeld, color = "summer_yeld"), size = 2) +
  geom_smooth(method = "lm",aes(y = winter_yeld, color = "winter_yeld"), size = 2) + 
  geom_smooth(method = "lm",aes(y = true_yeld, color = "true_yeld"), size = 2) +
  geom_smooth(method = "lm",aes(y = naive, color = "naive"), size = 2) +
  labs(title = "Proportion of females seen in rut reproduced per Year",
       x = "Year",
       y = "Proportion of females reproduced") +
  scale_color_manual(values = c("milk" = "red", "summer_yeld" = "blue", "winter_yeld" = "black", 
                                "true_yeld" = "green", "naive" = "orange"),
                     breaks = c("milk", "summer_yeld", "winter_yeld", "true_yeld", "naive"),
                     labels = c("milk", "summer_yeld", "winter_yeld", "true_yeld", "naive"))


ggplot(portion, aes(x = Hinds)) +
  geom_point(aes(y = milk, color = "milk"))+
  geom_point(aes(y = summer_yeld, color = "summer_yeld"))+
  geom_point(aes(y = winter_yeld, color = "winter_yeld"))+
  geom_point(aes(y = true_yeld, color = "true_yeld"))+
  geom_point(aes(y = naive, color = "naive"))+
  geom_smooth(method = "lm",aes(y = milk, color = "milk"), size = 2) +
  geom_smooth(method = "lm",aes(y = summer_yeld, color = "summer_yeld"), size = 2) +
  geom_smooth(method = "lm",aes(y = winter_yeld, color = "winter_yeld"), size = 2) + 
  geom_smooth(method = "lm",aes(y = true_yeld, color = "true_yeld"), size = 2) +
  geom_smooth(method = "lm",aes(y = naive, color = "naive"), size = 2) +
  labs(title = "Proportion of females seen in rut reproduced over Density",
       x = "Hind Density",
       y = "Proportion of females reproduced") +
  scale_color_manual(values = c("milk" = "red", "summer_yeld" = "blue", "winter_yeld" = "black", 
                                "true_yeld" = "green", "naive" = "orange"),
                     breaks = c("milk", "summer_yeld", "winter_yeld", "true_yeld", "naive"),
                     labels = c("milk", "summer_yeld", "winter_yeld", "true_yeld", "naive"))

ggplot(portion, aes(x = Adults)) +
  geom_point(aes(y = milk, color = "milk"))+
  geom_point(aes(y = summer_yeld, color = "summer_yeld"))+
  geom_point(aes(y = winter_yeld, color = "winter_yeld"))+
  geom_point(aes(y = true_yeld, color = "true_yeld"))+
  geom_point(aes(y = naive, color = "naive"))+
  geom_smooth(method = "lm",aes(y = milk, color = "milk"), size = 2) +
  geom_smooth(method = "lm",aes(y = summer_yeld, color = "summer_yeld"), size = 2) +
  geom_smooth(method = "lm",aes(y = winter_yeld, color = "winter_yeld"), size = 2) + 
  geom_smooth(method = "lm",aes(y = true_yeld, color = "true_yeld"), size = 2) +
  geom_smooth(method = "lm",aes(y = naive, color = "naive"), size = 2) +
  labs(title = "Proportion of females seen in rut reproduced over Density",
       x = "Adult Density",
       y = "Proportion of females reproduced") +
  scale_color_manual(values = c("milk" = "red", "summer_yeld" = "blue", "winter_yeld" = "black", 
                                "true_yeld" = "green", "naive" = "orange"),
                     breaks = c("milk", "summer_yeld", "winter_yeld", "true_yeld", "naive"),
                     labels = c("milk", "summer_yeld", "winter_yeld", "true_yeld", "naive"))

ggplot(portion, aes(x = Total)) +
  geom_point(aes(y = milk, color = "milk"))+
  geom_point(aes(y = summer_yeld, color = "summer_yeld"))+
  geom_point(aes(y = winter_yeld, color = "winter_yeld"))+
  geom_point(aes(y = true_yeld, color = "true_yeld"))+
  geom_point(aes(y = naive, color = "naive"))+
  geom_smooth(method = "lm",aes(y = milk, color = "milk"), size = 2) +
  geom_smooth(method = "lm",aes(y = summer_yeld, color = "summer_yeld"), size = 2) +
  geom_smooth(method = "lm",aes(y = winter_yeld, color = "winter_yeld"), size = 2) + 
  geom_smooth(method = "lm",aes(y = true_yeld, color = "true_yeld"), size = 2) +
  geom_smooth(method = "lm",aes(y = naive, color = "naive"), size = 2) +
  labs(title = "Proportion of females seen in rut reproduced over Density",
       x = "Total Density",
       y = "Proportion of females reproduced") +
  scale_color_manual(values = c("milk" = "red", "summer_yeld" = "blue", "winter_yeld" = "black", 
                                "true_yeld" = "green", "naive" = "orange"),
                     breaks = c("milk", "summer_yeld", "winter_yeld", "true_yeld", "naive"),
                     labels = c("milk", "summer_yeld", "winter_yeld", "true_yeld", "naive"))

ggplot(portion, aes(x = LU_Total)) +
  geom_point(aes(y = milk, color = "milk"))+
  geom_point(aes(y = summer_yeld, color = "summer_yeld"))+
  geom_point(aes(y = winter_yeld, color = "winter_yeld"))+
  geom_point(aes(y = true_yeld, color = "true_yeld"))+
  geom_point(aes(y = naive, color = "naive"))+
  geom_smooth(method = "lm",aes(y = milk, color = "milk"), size = 2) +
  geom_smooth(method = "lm",aes(y = summer_yeld, color = "summer_yeld"), size = 2) +
  geom_smooth(method = "lm",aes(y = winter_yeld, color = "winter_yeld"), size = 2) + 
  geom_smooth(method = "lm",aes(y = true_yeld, color = "true_yeld"), size = 2) +
  geom_smooth(method = "lm",aes(y = naive, color = "naive"), size = 2) +
  labs(title = "Proportion of females seen in rut reproduced over Density",
       x = "Livestock units",
       y = "Proportion of females reproduced") +
  scale_color_manual(values = c("milk" = "red", "summer_yeld" = "blue", "winter_yeld" = "black", 
                                "true_yeld" = "green", "naive" = "orange"),
                     breaks = c("milk", "summer_yeld", "winter_yeld", "true_yeld", "naive"),
                     labels = c("milk", "summer_yeld", "winter_yeld", "true_yeld", "naive"))
