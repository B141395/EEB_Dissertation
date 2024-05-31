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

FWS$S_Hinds<-scale(FWS$Hinds)
FWS$S_Adults<-scale(FWS$Adults)
FWS$S_Total<-scale(FWS$Total)
FWS$S_LU_Total<-scale(FWS$LU_Total)
FWS$S_BirthWt<-scale(FWS$BirthWt)
FWS$S_MumAgeSquared<-scale(FWS$MumAgeSquared)

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
Hinds

Separate<-glmmTMB(FWSurvival~Hinds+Stags+Calves+Sex+MotherStatus+MumAgeSquared+BirthWt+(1|DeerYear)+(1|MumCode),family = binomial,data=FWS)
Separate

Adults<-glmmTMB(FWSurvival~S_Adults+Sex+MotherStatus+S_MumAgeSquared+S_BirthWt+(1|DeerYear)+(1|MumCode),family = binomial,data=FWS)
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

#Scaled
S_Hinds<-glmmTMB(FWSurvival~S_Hinds
               +Sex
               +MotherStatus
               +S_MumAgeSquared
               +S_BirthWt
               +(1|DeerYear)
               +(1|MumCode),
               family = binomial(link = "logit"),data=FWS)
S_Hinds

S_Adults<-glmmTMB(FWSurvival~S_Adults+Sex+MotherStatus+S_MumAgeSquared+S_BirthWt+(1|DeerYear)+(1|MumCode),family = binomial,data=FWS)
S_Adults


S_Total<-glmmTMB(FWSurvival~S_Total+Sex+MotherStatus+S_MumAgeSquared+S_BirthWt+(1|DeerYear)+(1|MumCode),family = binomial,data=FWS)
S_Total

S_LU_Total<-glmmTMB(FWSurvival~S_LU_Total+Sex+MotherStatus+S_MumAgeSquared+S_BirthWt+(1|DeerYear)+(1|MumCode),family = binomial,data=FWS)
S_LU_Total

summary(S_Hinds)
summary(S_Adults)
summary(S_Total)
summary(S_LU_Total)

AIC(S_Hinds,S_Adults,S_Total,S_LU_Total)

#glmer
Hinds_glmer<-glmer(FWSurvival~Hinds
               +Sex
               +MotherStatus
               +MumAgeSquared
               +BirthWt
               +(1|DeerYear)
               +(1|MumCode),
               family = binomial(link = "logit"),data=FWS)
Hinds_glmer
summary(Hinds_glmer)


Adults_glmer<-glmer(FWSurvival~Adults+Sex+MotherStatus+MumAgeSquared+BirthWt+(1|DeerYear)+(1|MumCode),family = binomial,data=FWS)
Adults_glmer
summary(Adults_glmer)


Total_glmer<-glmer(FWSurvival~Total+Sex+MotherStatus+MumAgeSquared+BirthWt+(1|DeerYear)+(1|MumCode),family = binomial,data=FWS)
Total_glmer
summary(Total_glmer)

LU_Total_glmer<-glmer(FWSurvival~LU_Total+Sex+MotherStatus+MumAgeSquared+BirthWt+(1|DeerYear)+(1|MumCode),family = binomial,data=FWS)
LU_Total_glmer
summary(LU_Total_glmer)

AIC(Hinds_glmer,Adults_glmer,Total_glmer,LU_Total_glmer)

#lmer scaled
S_Hinds_glmer<-glmer(FWSurvival~S_Hinds
                   +Sex
                   +MotherStatus
                   +S_MumAgeSquared
                   +S_BirthWt
                   +(1|DeerYear)
                   +(1|MumCode),
                   family = binomial(link = "logit"),data=FWS)
summary(S_Hinds_glmer)


S_Adults_glmer<-glmer(FWSurvival~S_Adults+Sex+MotherStatus+S_MumAgeSquared+S_BirthWt+(1|DeerYear)+(1|MumCode),family = binomial,data=FWS)
summary(S_Adults_glmer)


S_Total_glmer<-glmer(FWSurvival~S_Total+Sex+MotherStatus+S_MumAgeSquared+S_BirthWt+(1|DeerYear)+(1|MumCode),family = binomial,data=FWS)
summary(S_Total_glmer)

S_LU_Total_glmer<-glmer(FWSurvival~S_LU_Total+Sex+MotherStatus+S_MumAgeSquared+S_BirthWt+(1|DeerYear)+(1|MumCode),family = binomial,data=FWS)
summary(S_LU_Total_glmer)

AIC(S_Hinds_glmer,S_Adults_glmer,S_Total_glmer,S_LU_Total_glmer)



Hinds_noWt<-glmer(FWSurvival~Hinds+Sex+MotherStatus+MumAgeSquared+(1|DeerYear)+(1|MumCode),family = binomial,data=FWS)
Hinds_noWt

Adults_noWt<-glmer(FWSurvival~Adults+Sex+MotherStatus+MumAgeSquared+(1|DeerYear)+(1|MumCode),family = binomial,data=FWS)
Adults_noWt

Total_noWt<-glmer(FWSurvival~Total+Sex+MotherStatus+MumAgeSquared+(1|DeerYear)+(1|MumCode),family = binomial,data=FWS)
Total_noWt

LU_Total_noWt<-glmer(FWSurvival~LU_Total+Sex+MotherStatus+MumAgeSquared+(1|DeerYear)+(1|MumCode),family = binomial,data=FWS)
LU_Total_noWt

summary(Hinds_noWt)
summary(Adults_noWt)
summary(Total_noWt)
summary(LU_Total_noWt)

AIC(Hinds_noWt,Adults_noWt,Total_noWt,LU_Total_noWt)

#Scaled
S_Hinds_noWt<-glmmTMB(FWSurvival~S_Hinds+Sex+MotherStatus+S_MumAgeSquared+(1|DeerYear)+(1|MumCode),family = binomial,data=FWS)

S_Adults_noWt<-glmmTMB(FWSurvival~S_Adults+Sex+MotherStatus+S_MumAgeSquared+(1|DeerYear)+(1|MumCode),family = binomial,data=FWS)

S_Total_noWt<-glmmTMB(FWSurvival~S_Total+Sex+MotherStatus+S_MumAgeSquared+(1|DeerYear)+(1|MumCode),family = binomial,data=FWS)

S_LU_Total_noWt<-glmmTMB(FWSurvival~S_LU_Total+Sex+MotherStatus+S_MumAgeSquared+(1|DeerYear)+(1|MumCode),family = binomial,data=FWS)

summary(S_Hinds_noWt)
summary(S_Adults_noWt)
summary(S_Total_noWt)
summary(S_LU_Total_noWt)

AIC(S_Hinds_noWt,S_Adults_noWt,S_Total_noWt,S_LU_Total_noWt)
#Scaled glmer
S_Hinds_noWt_glmer<-glmer(FWSurvival~S_Hinds+Sex+MotherStatus+S_MumAgeSquared+(1|DeerYear)+(1|MumCode),family = binomial,data=FWS)

S_Adults_noWt_glmer<-glmer(FWSurvival~S_Adults+Sex+MotherStatus+S_MumAgeSquared+(1|DeerYear)+(1|MumCode),family = binomial,data=FWS)

S_Total_noWt_glmer<-glmer(FWSurvival~S_Total+Sex+MotherStatus+S_MumAgeSquared+(1|DeerYear)+(1|MumCode),family = binomial,data=FWS)

S_LU_Total_noWt_glmer<-glmer(FWSurvival~S_LU_Total+Sex+MotherStatus+S_MumAgeSquared+(1|DeerYear)+(1|MumCode),family = binomial,data=FWS)

summary(S_Hinds_noWt_glmer)
summary(S_Adults_noWt_glmer)
summary(S_Total_noWt_glmer)
summary(S_LU_Total_noWt_glmer)

AIC(S_Hinds_noWt_glmer,S_Adults_noWt_glmer,S_Total_noWt_glmer,S_LU_Total_noWt_glmer)






#sexed

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

