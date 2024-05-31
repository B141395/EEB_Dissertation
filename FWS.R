library(tidyverse)
library(lme4)
library(ggplot2)
library(lmerTest)

FWS<-read.csv("FWS_data.csv")

Density<-read.csv("Population_estimate_calculations.csv")

FWS<-FWS%>% filter(!DeerYear %in% c(2022, 2023))
FWS$MumAgeSquared<-(FWS$MumAge)*(FWS$MumAge)

PopD<-Density %>% select (DeerYear,Hinds,Stags,Calves,Adults,Total,LU_Total)

FWS <- FWS %>%
  left_join(PopD, by = "DeerYear")

cor(PopD[, c("Hinds", "Adults", "Total", "LU_Total")])

cor(Density[, c("Total", "Adults", "Hinds", "Stags","Calves","Calves_M","Calves_F","LU_Total")])


str(FWS)

plot(tapply(FWS$Hinds, FWS$DeerYear, mean),tapply(FWS$Total, FWS$DeerYear, mean))
plot(tapply(FWS$Hinds, FWS$DeerYear, mean),tapply(FWS$Adults, FWS$DeerYear, mean))
plot(tapply(FWS$Hinds, FWS$DeerYear, mean),tapply(FWS$LU_Total, FWS$DeerYear, mean))
plot(tapply(FWS$LU_Total, FWS$DeerYear, mean),tapply(FWS$Total, FWS$DeerYear, mean))


library(glmmTMB)

Hinds<-glmmTMB(FWSurvival~Hinds
               +Sex
               +MotherStatus
               +MumAgeSquared
               +BirthWt
               +(1|DeerYear)
               +(1|MumCode),
               family = binomial(link = "logit"),data=FWS)

Separate<-glmmTMB(FWSurvival~Hinds+Stags+Calves+Sex+MotherStatus+MumAgeSquared+BirthWt+(1|DeerYear)+(1|MumCode),family = binomial,data=FWS)
Separate

Adults<-glmmTMB(FWSurvival~Adults+Sex+MotherStatus+MumAgeSquared+BirthWt+(1|DeerYear)+(1|MumCode),family = binomial,data=FWS)
Adults

Total<-glmmTMB(FWSurvival~Total+Sex+MotherStatus+MumAgeSquared+BirthWt+(1|DeerYear)+(1|MumCode),family = binomial,data=FWS)
Total

LU_Total<-glmmTMB(FWSurvival~LU_Total+Sex+MotherStatus+MumAgeSquared+BirthWt+(1|DeerYear)+(1|MumCode),family = binomial,data=FWS)
LU_Total

summary(Hinds)
summary(Adults)
summary(Total)
summary(LU_Total)

AIC(Hinds,Adults,Total,LU_Total)

Hinds_noWt<-glmmTMB(FWSurvival~Hinds+Sex+MotherStatus+MumAgeSquared+(1|DeerYear)+(1|MumCode),family = binomial,data=FWS)
Hinds_noWt

Adults_noWt<-glmmTMB(FWSurvival~Adults+Sex+MotherStatus+MumAgeSquared+(1|DeerYear)+(1|MumCode),family = binomial,data=FWS)
Adults_noWt

Total_noWt<-glmmTMB(FWSurvival~Total+Sex+MotherStatus+MumAgeSquared+(1|DeerYear)+(1|MumCode),family = binomial,data=FWS)
Total_noWt

LU_Total_noWt<-glmmTMB(FWSurvival~LU_Total+Sex+MotherStatus+MumAgeSquared+(1|DeerYear)+(1|MumCode),family = binomial,data=FWS)
LU_Total_noWt

summary(Hinds_noWt)
summary(Adults_noWt)
summary(Total_noWt)
summary(LU_Total_noWt)

AIC(Hinds_noWt,Adults_noWt,Total_noWt,LU_Total_noWt)


male_FWS<- FWS %>% filter(!Sex %in% c(1,3))
fem_FWS<- FWS %>% filter(!Sex %in% c(2,3))

#Male FWS
MHinds<-glmmTMB(FWSurvival~Hinds+MotherStatus+MumAgeSquared+BirthWt+(1|DeerYear)+(1|MumCode),family = binomial,data=male_FWS)

MAdults<-glmmTMB(FWSurvival~Adults+MotherStatus+MumAgeSquared+BirthWt+(1|DeerYear)+(1|MumCode),family = binomial,data=male_FWS)

MTotal<-glmmTMB(FWSurvival~Total+MotherStatus+MumAgeSquared+BirthWt+(1|DeerYear)+(1|MumCode),family = binomial,data=male_FWS)

MLU_Total<-glmmTMB(FWSurvival~LU_Total+MotherStatus+MumAgeSquared+BirthWt+(1|DeerYear)+(1|MumCode),family = binomial,data=male_FWS)

summary(MHinds)
summary(MAdults)
summary(MTotal)
summary(MLU_Total)

#Female FWS
FHinds<-glmmTMB(FWSurvival~Hinds+MotherStatus+MumAgeSquared+BirthWt+(1|DeerYear)+(1|MumCode),family = binomial,data=fem_FWS)

FAdults<-glmmTMB(FWSurvival~Adults+MotherStatus+MumAgeSquared+BirthWt+(1|DeerYear)+(1|MumCode),family = binomial,data=fem_FWS)

FTotal<-glmmTMB(FWSurvival~Total+MotherStatus+MumAgeSquared+BirthWt+(1|DeerYear)+(1|MumCode),family = binomial,data=fem_FWS)

FLU_Total<-glmmTMB(FWSurvival~LU_Total+MotherStatus+MumAgeSquared+BirthWt+(1|DeerYear)+(1|MumCode),family = binomial,data=fem_FWS)

summary(FHinds)
summary(FAdults)
summary(FTotal)
summary(FLU_Total)


#plots

rate<- FWS %>%
  group_by(DeerYear) %>%
  summarize(
    count = n(),                         
    survived = sum(FWSurvival, na.rm = TRUE),
    rate = survived/count,
    Mrate = 1-rate
  )

rate <- rate %>%
  left_join(PopD, by = "DeerYear")

male_rate<-male_FWS %>%
  group_by(DeerYear) %>%
  summarize(
    count = n(),                         
    survived = sum(FWSurvival, na.rm = TRUE),
    rate = survived/count,
    Mrate = 1-rate
  ) %>%
  left_join(PopD, by = "DeerYear")

fem_rate<-fem_FWS %>%
  group_by(DeerYear) %>%
  summarize(
    count = n(),                         
    survived = sum(FWSurvival, na.rm = TRUE),
    rate = survived/count,
    Mrate = 1-rate
  ) %>%
  left_join(PopD, by = "DeerYear")

#Total
ggplot(rate, aes(x = Hinds, y = rate)) +
  geom_point()+
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  ylab("First Winter Survival Rate") + xlab("Number of Hinds")


ggplot(rate, aes(x = Adults, y = rate)) +
  geom_point()+
  geom_smooth(method = "lm", se = FALSE, color = "blue")+
  ylab("First Winter Survival Rate") + xlab("Number of Hinds and Stags")


ggplot(rate, aes(x = Total, y = rate)) +
  geom_point()+
  geom_smooth(method = "lm", se = FALSE, color = "blue")+
  ylab("First Winter Survival Rate") + xlab("Number of Hinds, Stags and Calves")


ggplot(rate, aes(x = LU_Total, y = rate)) +
  geom_point()+
  geom_smooth(method = "lm", se = FALSE, color = "blue")+
  ylab("First Winter Survival Rate") + xlab("Livestock Unit")


#Fem calves

ggplot(fem_rate, aes(x = Hinds, y = rate)) +
  geom_point()+
  geom_smooth(method = "lm", se = FALSE, color = "blue")+
  ylab("Female First Winter Survival Rate") + xlab("Number of Hinds")


ggplot(fem_rate, aes(x = Adults, y = rate)) +
  geom_point()+
  geom_smooth(method = "lm", se = FALSE, color = "blue")+
  ylab("Female First Winter Survival Rate") + xlab("Number of Hinds and Stags")


ggplot(fem_rate, aes(x = Total, y = rate)) +
  geom_point()+
  geom_smooth(method = "lm", se = FALSE, color = "blue")+
  ylab("Female First Winter Survival Rate") + xlab("Number of Hinds, Stags and Calves")



ggplot(fem_rate, aes(x = LU_Total, y = rate)) +
  geom_point()+
  geom_smooth(method = "lm", se = FALSE, color = "blue")+
  ylab("Female First Winter Survival Rate") + xlab("Livestock Unit")


#Male calves
ggplot(male_rate, aes(x = Hinds, y = rate)) +
  geom_point()+
  geom_smooth(method = "lm", se = FALSE, color = "blue")+
  ylab("Male First Winter Survival Rate") + xlab("Number of Hinds")


ggplot(male_rate, aes(x = Adults, y = rate)) +
  geom_point()+
  geom_smooth(method = "lm", se = FALSE, color = "blue")+
  ylab("Male First Winter Survival Rate") + xlab("Number of Hinds and Stags")


ggplot(male_rate, aes(x = Total, y = rate)) +
  geom_point()+
  geom_smooth(method = "lm", se = FALSE, color = "blue")+
  ylab("Male First Winter Survival Rate") + xlab("Number of Hinds, Stags and Calves")


ggplot(male_rate, aes(x = LU_Total, y = rate)) +
  geom_point()+
  geom_smooth(method = "lm", se = FALSE, color = "blue")+
  ylab("Male First Winter Survival Rate") + xlab("Livestock Unit")

