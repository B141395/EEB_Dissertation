library(tidyverse)
library(lme4)
library(ggplot2)
library(lmerTest)
library(glmmTMB)


FWS<-read.csv("FWS_data.csv")

Density<-read.csv("Population_estimate_calculations.csv")

FWS<-FWS%>% filter(!DeerYear %in% c(2022, 2023))
FWS$MumAgeSquared<-(FWS$MumAge)*(FWS$MumAge)

PopD<-Density %>% select (DeerYear,Hinds,Stags,Calves,Calves_M,Calves_F,Adults,Total,LU_Total)

str(FWS)
FWS <- FWS %>%
  left_join(PopD, by = "DeerYear")

cor(PopD[, c("Hinds", "Adults", "Total", "LU_Total")])

cor(Density[, c("Total", "Adults", "Hinds", "Stags","Calves","Calves_M","Calves_F","LU_Total")])

#With BirthWt Without Year
Hinds<-glmmTMB(FWSurvival~Hinds
                 +Sex
                 +MotherStatus
                 +MumAge
                 +MumAgeSquared
                 +BirthWt
                 +(1|DeerYear)
                 +(1|MumCode),
                 family = binomial(link = "logit"),data=FWS)

Adults<-glmmTMB(FWSurvival~Adults+Sex+MotherStatus+MumAge+MumAgeSquared+BirthWt+(1|DeerYear)+(1|MumCode),family = binomial,data=FWS)

Total<-glmmTMB(FWSurvival~Total+Sex+MotherStatus+MumAge+MumAgeSquared+BirthWt+(1|DeerYear)+(1|MumCode),family = binomial,data=FWS)

LU_Total<-glmmTMB(FWSurvival~LU_Total+Sex+MotherStatus+MumAge+MumAgeSquared+BirthWt+(1|DeerYear)+(1|MumCode),family = binomial,data=FWS)

summary(Hinds)
summary(Adults)
summary(Total)
summary(LU_Total)


#Without BirthWt & Year
Hinds_noWt<-glmmTMB(FWSurvival~Hinds+Sex+MotherStatus+MumAge+MumAgeSquared+(1|DeerYear)+(1|MumCode),family = binomial,data=FWS)

Adults_noWt<-glmmTMB(FWSurvival~Adults+Sex+MotherStatus+MumAge+MumAgeSquared+(1|DeerYear)+(1|MumCode),family = binomial,data=FWS)

Total_noWt<-glmmTMB(FWSurvival~Total+Sex+MotherStatus+MumAge+MumAgeSquared+(1|DeerYear)+(1|MumCode),family = binomial,data=FWS)

LU_Total_noWt<-glmmTMB(FWSurvival~LU_Total+Sex+MotherStatus+MumAge+MumAgeSquared+(1|DeerYear)+(1|MumCode),family = binomial,data=FWS)

summary(Hinds_noWt)
summary(Adults_noWt)
summary(Total_noWt)
summary(LU_Total_noWt)

AIC(Hinds_noWt,Adults_noWt,Total_noWt,LU_Total_noWt)


#Without BirthWt With Year
yr_Hinds_noWt<-glmmTMB(FWSurvival~Hinds+Sex+MotherStatus+MumAge+MumAgeSquared+DeerYear+(1|DeerYear)+(1|MumCode),family = binomial,data=FWS)

yr_Adults_noWt<-glmmTMB(FWSurvival~Adults+Sex+MotherStatus+MumAge+MumAgeSquared+DeerYear+(1|DeerYear)+(1|MumCode),family = binomial,data=FWS)

yr_Total_noWt<-glmmTMB(FWSurvival~Total+Sex+MotherStatus+MumAge+MumAgeSquared+DeerYear+(1|DeerYear)+(1|MumCode),family = binomial,data=FWS)

yr_LU_Total_noWt<-glmmTMB(FWSurvival~LU_Total+Sex+MotherStatus+MumAge+MumAgeSquared+DeerYear+(1|DeerYear)+(1|MumCode),family = binomial,data=FWS)

summary(yr_Hinds_noWt)
summary(yr_Adults_noWt)
summary(yr_Total_noWt)
summary(yr_LU_Total_noWt)

AIC(Hinds_noWt,Adults_noWt,Total_noWt,LU_Total_noWt)

#With BirthWt and Year
yr_Hinds<-glmmTMB(FWSurvival~Hinds
                    +Sex
                    +MotherStatus
                    +MumAge
                    +MumAgeSquared
                    +BirthWt
                    +DeerYear
                    +(1|DeerYear)
                    +(1|MumCode),
                    family = binomial(link = "logit"),data=FWS)

yr_Adults<-glmmTMB(FWSurvival~Adults+Sex+MotherStatus+MumAge+MumAgeSquared+BirthWt+DeerYear+(1|DeerYear)+(1|MumCode),family = binomial,data=FWS)

yr_Total<-glmmTMB(FWSurvival~Total+Sex+MotherStatus+MumAge+MumAgeSquared+BirthWt+DeerYear+(1|DeerYear)+(1|MumCode),family = binomial,data=FWS)

yr_LU_Total<-glmmTMB(FWSurvival~LU_Total+Sex+MotherStatus+MumAge+MumAgeSquared+BirthWt+DeerYear+(1|DeerYear)+(1|MumCode),family = binomial,data=FWS)

summary(yr_Hinds)
summary(yr_Adults)
summary(yr_Total)
summary(yr_LU_Total)

#sexed

male_FWS<- FWS %>% filter(!Sex %in% c(1,3))
fem_FWS<- FWS %>% filter(!Sex %in% c(2,3))

#Male FWS
MHinds<-glmmTMB(FWSurvival~Hinds+MotherStatus+MumAge+MumAgeSquared+BirthWt+(1|DeerYear)+(1|MumCode),family = binomial,data=male_FWS)

MAdults<-glmmTMB(FWSurvival~Adults+MotherStatus+MumAge+MumAgeSquared+BirthWt+(1|DeerYear)+(1|MumCode),family = binomial,data=male_FWS)

MTotal<-glmmTMB(FWSurvival~Total+MotherStatus+MumAge+MumAgeSquared+BirthWt+(1|DeerYear)+(1|MumCode),family = binomial,data=male_FWS)

MLU_Total<-glmmTMB(FWSurvival~LU_Total+MotherStatus+MumAge+MumAgeSquared+BirthWt+(1|DeerYear)+(1|MumCode),family = binomial,data=male_FWS)

summary(MHinds)
summary(MAdults)
summary(MTotal)
summary(MLU_Total)



MCalves_M<-glmmTMB(FWSurvival~Calves_M
                   +MotherStatus
                   +MumAge
                   +MumAgeSquared
                   +BirthWt
                   +(1|DeerYear)
                   +(1|MumCode),
                   family = binomial(link = "logit"),data=male_FWS)

MCalves_F<-glmmTMB(FWSurvival~Calves_F
                   +MotherStatus
                   +MumAge
                   +MumAgeSquared
                   +BirthWt
                   +(1|DeerYear)
                   +(1|MumCode),
                   family = binomial(link = "logit"),data=male_FWS)


MStags<-glmmTMB(FWSurvival~Stags+MotherStatus+MumAge+MumAgeSquared+BirthWt+(1|DeerYear)+(1|MumCode),family = binomial,data=male_FWS)
MCalves<-glmmTMB(FWSurvival~Calves+MotherStatus+MumAge+MumAgeSquared+BirthWt+(1|DeerYear)+(1|MumCode),family = binomial,data=male_FWS)
summary(MStags)
summary(MCalves)
summary(MCalves_M)
summary(MCalves_F)

#Female FWS
FHinds<-glmmTMB(FWSurvival~Hinds+MotherStatus+MumAge+MumAgeSquared+BirthWt+(1|DeerYear)+(1|MumCode),family = binomial,data=fem_FWS)

FAdults<-glmmTMB(FWSurvival~Adults+MotherStatus+MumAge+MumAgeSquared+BirthWt+(1|DeerYear)+(1|MumCode),family = binomial,data=fem_FWS)

FTotal<-glmmTMB(FWSurvival~Total+MotherStatus+MumAge+MumAgeSquared+BirthWt+(1|DeerYear)+(1|MumCode),family = binomial,data=fem_FWS)

FLU_Total<-glmmTMB(FWSurvival~LU_Total+MotherStatus+MumAge+MumAgeSquared+BirthWt+(1|DeerYear)+(1|MumCode),family = binomial,data=fem_FWS)

summary(FHinds)
summary(FAdults)
summary(FTotal)
summary(FLU_Total)

FCalves_M<-glmmTMB(FWSurvival~Calves_M
                   +MotherStatus
                   +MumAge
                   +MumAgeSquared
                   +BirthWt
                   +(1|DeerYear)
                   +(1|MumCode),
                   family = binomial(link = "logit"),data=fem_FWS)

FCalves_F<-glmmTMB(FWSurvival~Calves_F
                   +MotherStatus
                   +MumAge
                   +MumAgeSquared
                   +BirthWt
                   +(1|DeerYear)
                   +(1|MumCode),
                   family = binomial(link = "logit"),data=fem_FWS)


FStags<-glmmTMB(FWSurvival~Stags+MotherStatus+MumAge+MumAgeSquared+BirthWt+(1|DeerYear)+(1|MumCode),family = binomial,data=fem_FWS)
FCalves<-glmmTMB(FWSurvival~Calves+MotherStatus+MumAge+MumAgeSquared+BirthWt+(1|DeerYear)+(1|MumCode),family = binomial,data=fem_FWS)
summary(FStags)
summary(FCalves)
summary(FCalves_M)
summary(FCalves_F)
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

ggplot(rate, aes(x = DeerYear, y = rate)) +
  geom_point()+
  geom_smooth(method = "lm",color = "black", size = 0.75) +
  labs(title="First Winter Survival Rate over Year")+
  ylab("First Winter Survival Rate") + xlab("Year")+
  theme_minimal() +
  geom_text(aes(y = rate+ 0.03, label = paste0("(", DeerYear, ")")), size = 2.5, color = "black") 




ggplot(rate, aes(x = Hinds, y = rate)) +
  geom_point()+
  geom_smooth(method = "lm",color = "black", size = 0.75) +
  labs(title="First Winter Survival Rate over Density")+
  ylab("First Winter Survival Rate") + xlab("Number of Hinds")+
  theme_minimal() +
  geom_text(aes(y = rate+ 0.03, label = paste0("(", DeerYear, ")")), size = 2.5, color = "black") 



ggplot(rate, aes(x = Adults, y = rate)) +
  geom_point()+
  geom_smooth(method = "lm",color = "black", size = 0.75) +
  labs(title="First Winter Survival Rate over Density")+
  ylab("First Winter Survival Rate") + xlab("Number of Hinds and Stags")+
  theme_minimal() +
  geom_text(aes(y = rate+ 0.03, label = paste0("(", DeerYear, ")")), size = 2.5, color = "black") 



ggplot(rate, aes(x = Total, y = rate)) +
  geom_point()+
  geom_smooth(method = "lm",color = "black", size = 0.75) +
  labs(title="First Winter Survival Rate over Density")+
  ylab("First Winter Survival Rate") + xlab("Number of Hinds, Stags and Calves")+
  theme_minimal() +
  geom_text(aes(y = rate+ 0.03, label = paste0("(", DeerYear, ")")), size = 2.5, color = "black") 



ggplot(rate, aes(x = LU_Total, y = rate)) +
  geom_point()+
  geom_smooth(method = "lm",color = "black", size = 0.75) +
  labs(title="First Winter Survival Rate over Density")+
  ylab("First Winter Survival Rate") + xlab("Livestock Unit")+
  theme_minimal() +
  geom_text(aes(y = rate+ 0.03, label = paste0("(", DeerYear, ")")), size = 2.5, color = "black") 



#Fem calves

ggplot(fem_rate, aes(x = Hinds, y = rate)) +
  geom_point()+
  geom_smooth(method = "lm",color = "black", size = 0.75) +
  labs(title="Female First Winter Survival Rate over Density")+
  ylab("Female First Winter Survival Rate") + xlab("Number of Hinds")+
  theme_minimal() +
  geom_text(aes(y = rate+ 0.03, label = paste0("(", DeerYear, ")")), size = 2.5, color = "black") 



ggplot(fem_rate, aes(x = Adults, y = rate)) +
  geom_point()+
  geom_smooth(method = "lm",color = "black", size = 0.75) +
  labs(title="Female First Winter Survival Rate over Density")+
  ylab("Female First Winter Survival Rate") + xlab("Number of Hinds and Stags")+
  theme_minimal() +
  geom_text(aes(y = rate+ 0.03, label = paste0("(", DeerYear, ")")), size = 2.5, color = "black") 



ggplot(fem_rate, aes(x = Total, y = rate)) +
  geom_point()+
  geom_smooth(method = "lm",color = "black", size = 0.75) +
  labs(title="Female First Winter Survival Rate over Density")+
  ylab("Female First Winter Survival Rate") + xlab("Number of Hinds, Stags and Calves")+
  theme_minimal() +
  geom_text(aes(y = rate+ 0.03, label = paste0("(", DeerYear, ")")), size = 2.5, color = "black") 




ggplot(fem_rate, aes(x = LU_Total, y = rate)) +
  geom_point()+
  geom_smooth(method = "lm",color = "black", size = 0.75) +
  labs(title="Female First Winter Survival Rate over Density")+
  ylab("Female First Winter Survival Rate") + xlab("Livestock Unit")+
  theme_minimal() +
  geom_text(aes(y = rate+ 0.03, label = paste0("(", DeerYear, ")")), size = 2.5, color = "black") 



#Male calves
ggplot(male_rate, aes(x = Hinds, y = rate)) +
  geom_point()+
  geom_smooth(method = "lm",color = "black", size = 0.75) +
  labs(title="Male First Winter Survival Rate over Density")+
  ylab("Male First Winter Survival Rate") + xlab("Number of Hinds")+
  theme_minimal() +
  geom_text(aes(y = rate+ 0.03, label = paste0("(", DeerYear, ")")), size = 2.5, color = "black") 



ggplot(male_rate, aes(x = Adults, y = rate)) +
  geom_point()+
  geom_smooth(method = "lm",color = "black", size = 0.75) +
  labs(title="Male First Winter Survival Rate over Density")+
  ylab("Male First Winter Survival Rate") + xlab("Number of Hinds and Stags")+
  theme_minimal() +
  geom_text(aes(y = rate+ 0.03, label = paste0("(", DeerYear, ")")), size = 2.5, color = "black") 



ggplot(male_rate, aes(x = Total, y = rate)) +
  geom_point()+
  geom_smooth(method = "lm",color = "black", size = 0.75) +
  labs(title="Male First Winter Survival Rate over Density")+
  ylab("Male First Winter Survival Rate") + xlab("Number of Hinds, Stags and Calves")+
  theme_minimal() +
  geom_text(aes(y = rate+ 0.03, label = paste0("(", DeerYear, ")")), size = 2.5, color = "black") 



ggplot(male_rate, aes(x = LU_Total, y = rate)) +
  geom_point()+
  geom_smooth(method = "lm",color = "black", size = 0.75) +
  labs(title="Male First Winter Survival Rate over Density")+
  ylab("Male First Winter Survival Rate") + xlab("Livestock Unit")+
  theme_minimal() +
  geom_text(aes(y = rate+ 0.03, label = paste0("(", DeerYear, ")")), size = 2.5, color = "black") 


