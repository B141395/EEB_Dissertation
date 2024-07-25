library(tidyverse)
library(lme4)
library(ggplot2)
library(lmerTest)
library(glmmTMB)
library(ggeffects)

fecundity<-read.csv("Fecundity_yr-1.csv")

Density<-read.csv("Population_estimate_calculations.csv")

PopD<-Density %>% select (DeerYear,Hinds,Adults,Total,LU_Total)

fecundity <- fecundity %>%
  left_join(PopD, by = "DeerYear")

fecundity$AgeSquared<-(fecundity$Age)*(fecundity$Age)

fecundity<-fecundity%>% filter(Age > 2)


fecundity_mod<-glmmTMB(Fecundity~Adults
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

milk_mod<-glmmTMB(Fecundity~Hinds
                  +(1|DeerYear)
                  +(1|Female),
                  family = binomial(link = "logit"),data=milk)
summary(milk_mod)


yeld <- rut %>% filter(ReprodStatus %in% c("Winter yeld", "Summer yeld","True yeld"))

yeld_mod<-glmmTMB(Fecundity~Hinds
                  +ReprodStatus
                  +(1|DeerYear)
                  +(1|Female),
                  family = binomial(link = "logit"),data=yeld)
summary(yeld_mod)

#Milk----
milk_Hinds<-glmmTMB(Fecundity~Hinds+Age+AgeSquared+DeerYear
                    +(1|DeerYear)
                    +(1|Female),
                    family = binomial(link = "logit"),data=milk)
milk_Adults<-glmmTMB(Fecundity~Adults+Age+AgeSquared+DeerYear
                     +(1|DeerYear)
                     +(1|Female),
                     family = binomial(link = "logit"),data=milk)
milk_Total<-glmmTMB(Fecundity~Total+Age+AgeSquared+DeerYear
                    +(1|DeerYear)
                    +(1|Female),
                    family = binomial(link = "logit"),data=milk)
milk_LU_Total<-glmmTMB(Fecundity~LU_Total+Age+AgeSquared+DeerYear
                       +(1|DeerYear)
                       +(1|Female),
                       family = binomial(link = "logit"),data=milk)
summary(milk_Hinds)
summary(milk_Adults)
summary(milk_Total)
summary(milk_LU_Total)

milk_Hinds_noage<-glmmTMB(Fecundity~Hinds+DeerYear
                          +(1|DeerYear)
                          +(1|Female),
                          family = binomial(link = "logit"),data=milk)
milk_Adults_noage<-glmmTMB(Fecundity~Adults+DeerYear
                           +(1|DeerYear)
                           +(1|Female),
                           family = binomial(link = "logit"),data=milk)
milk_Total_noage<-glmmTMB(Fecundity~Total+DeerYear
                          +(1|DeerYear)
                          +(1|Female),
                          family = binomial(link = "logit"),data=milk)
milk_LU_Total_noage<-glmmTMB(Fecundity~LU_Total+DeerYear
                             +(1|DeerYear)
                             +(1|Female),
                             family = binomial(link = "logit"),data=milk)
summary(milk_Hinds_noage)
summary(milk_Adults_noage)
summary(milk_Total_noage)
summary(milk_LU_Total_noage)

milk_Hinds_noage_yr<-glmmTMB(Fecundity~Hinds
                             +(1|DeerYear)
                             +(1|Female),
                             family = binomial(link = "logit"),data=milk)
milk_Adults_noage_yr<-glmmTMB(Fecundity~Adults
                              +(1|DeerYear)
                              +(1|Female),
                              family = binomial(link = "logit"),data=milk)
milk_Total_noage_yr<-glmmTMB(Fecundity~Total
                             +(1|DeerYear)
                             +(1|Female),
                             family = binomial(link = "logit"),data=milk)
milk_LU_Total_noage_yr<-glmmTMB(Fecundity~LU_Total
                                +(1|DeerYear)
                                +(1|Female),
                                family = binomial(link = "logit"),data=milk)
summary(milk_Hinds_noage_yr)
summary(milk_Adults_noage_yr)
summary(milk_Total_noage_yr)
summary(milk_LU_Total_noage_yr)

#Summer yeld----
summer_Hinds<-glmmTMB(Fecundity~Hinds+Age+AgeSquared+DeerYear
                      +(1|DeerYear)
                      +(1|Female),
                      family = binomial(link = "logit"),data=summer_yeld)
summer_Adults<-glmmTMB(Fecundity~Adults+Age+AgeSquared+DeerYear
                       +(1|DeerYear)
                       +(1|Female),
                       family = binomial(link = "logit"),data=summer_yeld)
summer_Total<-glmmTMB(Fecundity~Total+Age+AgeSquared+DeerYear
                      +(1|DeerYear)
                      +(1|Female),
                      family = binomial(link = "logit"),data=summer_yeld)
summer_LU_Total<-glmmTMB(Fecundity~LU_Total+Age+AgeSquared+DeerYear
                         +(1|DeerYear)
                         +(1|Female),
                         family = binomial(link = "logit"),data=summer_yeld)
summary(summer_Hinds)
summary(summer_Adults)
summary(summer_Total)
summary(summer_LU_Total)

summer_Hinds_noage<-glmmTMB(Fecundity~Hinds+DeerYear
                            +(1|DeerYear)
                            +(1|Female),
                            family = binomial(link = "logit"),data=summer_yeld)
summer_Adults_noage<-glmmTMB(Fecundity~Adults+DeerYear
                             +(1|DeerYear)
                             +(1|Female),
                             family = binomial(link = "logit"),data=summer_yeld)
summer_Total_noage<-glmmTMB(Fecundity~Total+DeerYear
                            +(1|DeerYear)
                            +(1|Female),
                            family = binomial(link = "logit"),data=summer_yeld)
summer_LU_Total_noage<-glmmTMB(Fecundity~LU_Total+DeerYear
                               +(1|DeerYear)
                               +(1|Female),
                               family = binomial(link = "logit"),data=summer_yeld)
summary(summer_Hinds_noage)
summary(summer_Adults_noage)
summary(summer_Total_noage)
summary(summer_LU_Total_noage)

summer_Hinds_noage_yr<-glmmTMB(Fecundity~Hinds
                               +(1|DeerYear)
                               +(1|Female),
                               family = binomial(link = "logit"),data=summer_yeld)
summer_Adults_noage_yr<-glmmTMB(Fecundity~Adults
                                +(1|DeerYear)
                                +(1|Female),
                                family = binomial(link = "logit"),data=summer_yeld)
summer_Total_noage_yr<-glmmTMB(Fecundity~Total
                               +(1|DeerYear)
                               +(1|Female),
                               family = binomial(link = "logit"),data=summer_yeld)
summer_LU_Total_noage_yr<-glmmTMB(Fecundity~LU_Total
                                  +(1|DeerYear)
                                  +(1|Female),
                                  family = binomial(link = "logit"),data=summer_yeld)
summary(summer_Hinds_noage_yr)
summary(summer_Adults_noage_yr)
summary(summer_Total_noage_yr)
summary(summer_LU_Total_noage_yr)

#Winter yeld----
winter_Hinds<-glmmTMB(Fecundity~Hinds+Age+AgeSquared+DeerYear
                      +(1|DeerYear)
                      +(1|Female),
                      family = binomial(link = "logit"),data=winter_yeld)
winter_Adults<-glmmTMB(Fecundity~Adults+Age+AgeSquared+DeerYear
                       +(1|DeerYear)
                       +(1|Female),
                       family = binomial(link = "logit"),data=winter_yeld)
winter_Total<-glmmTMB(Fecundity~Total+Age+AgeSquared+DeerYear
                      +(1|DeerYear)
                      +(1|Female),
                      family = binomial(link = "logit"),data=winter_yeld)
winter_LU_Total<-glmmTMB(Fecundity~LU_Total+Age+AgeSquared+DeerYear
                         +(1|DeerYear)
                         +(1|Female),
                         family = binomial(link = "logit"),data=winter_yeld)
summary(winter_Hinds)
summary(winter_Adults)
summary(winter_Total)
summary(winter_LU_Total)

winter_Hinds_noage<-glmmTMB(Fecundity~Hinds+DeerYear
                            +(1|DeerYear)
                            +(1|Female),
                            family = binomial(link = "logit"),data=winter_yeld)
winter_Adults_noage<-glmmTMB(Fecundity~Adults+DeerYear
                             +(1|DeerYear)
                             +(1|Female),
                             family = binomial(link = "logit"),data=winter_yeld)
winter_Total_noage<-glmmTMB(Fecundity~Total+DeerYear
                            +(1|DeerYear)
                            +(1|Female),
                            family = binomial(link = "logit"),data=winter_yeld)
winter_LU_Total_noage<-glmmTMB(Fecundity~LU_Total+DeerYear
                               +(1|DeerYear)
                               +(1|Female),
                               family = binomial(link = "logit"),data=winter_yeld)
summary(winter_Hinds_noage)
summary(winter_Adults_noage)
summary(winter_Total_noage)
summary(winter_LU_Total_noage)

winter_Hinds_noage_yr<-glmmTMB(Fecundity~Hinds
                               +(1|DeerYear)
                               +(1|Female),
                               family = binomial(link = "logit"),data=winter_yeld)
winter_Adults_noage_yr<-glmmTMB(Fecundity~Adults
                                +(1|DeerYear)
                                +(1|Female),
                                family = binomial(link = "logit"),data=winter_yeld)
winter_Total_noage_yr<-glmmTMB(Fecundity~Total
                               +(1|DeerYear)
                               +(1|Female),
                               family = binomial(link = "logit"),data=winter_yeld)
winter_LU_Total_noage_yr<-glmmTMB(Fecundity~LU_Total
                                  +(1|DeerYear)
                                  +(1|Female),
                                  family = binomial(link = "logit"),data=winter_yeld)
summary(winter_Hinds_noage_yr)
summary(winter_Adults_noage_yr)
summary(winter_Total_noage_yr)
summary(winter_LU_Total_noage_yr)

#True yeld----
true_Hinds<-glmmTMB(Fecundity~Hinds+Age+AgeSquared+DeerYear
                    +(1|DeerYear)
                    +(1|Female),
                    family = binomial(link = "logit"),data=true_yeld)
true_Adults<-glmmTMB(Fecundity~Adults+Age+AgeSquared+DeerYear
                     +(1|DeerYear)
                     +(1|Female),
                     family = binomial(link = "logit"),data=true_yeld)
true_Total<-glmmTMB(Fecundity~Total+Age+AgeSquared+DeerYear
                    +(1|DeerYear)
                    +(1|Female),
                    family = binomial(link = "logit"),data=true_yeld)
true_LU_Total<-glmmTMB(Fecundity~LU_Total+Age+AgeSquared+DeerYear
                       +(1|DeerYear)
                       +(1|Female),
                       family = binomial(link = "logit"),data=true_yeld)
summary(true_Hinds)
summary(true_Adults)
summary(true_Total)
summary(true_LU_Total)

true_Hinds_noage<-glmmTMB(Fecundity~Hinds+DeerYear
                          +(1|DeerYear)
                          +(1|Female),
                          family = binomial(link = "logit"),data=true_yeld)
true_Adults_noage<-glmmTMB(Fecundity~Adults+DeerYear
                           +(1|DeerYear)
                           +(1|Female),
                           family = binomial(link = "logit"),data=true_yeld)
true_Total_noage<-glmmTMB(Fecundity~Total+DeerYear
                          +(1|DeerYear)
                          +(1|Female),
                          family = binomial(link = "logit"),data=true_yeld)
true_LU_Total_noage<-glmmTMB(Fecundity~LU_Total+DeerYear
                             +(1|DeerYear)
                             +(1|Female),
                             family = binomial(link = "logit"),data=true_yeld)
summary(true_Hinds_noage)
summary(true_Adults_noage)
summary(true_Total_noage)
summary(true_LU_Total_noage)

true_Hinds_noage_yr<-glmmTMB(Fecundity~Hinds
                             +(1|DeerYear)
                             +(1|Female),
                             family = binomial(link = "logit"),data=true_yeld)
true_Adults_noage_yr<-glmmTMB(Fecundity~Adults
                              +(1|DeerYear)
                              +(1|Female),
                              family = binomial(link = "logit"),data=true_yeld)
true_Total_noage_yr<-glmmTMB(Fecundity~Total
                             +(1|DeerYear)
                             +(1|Female),
                             family = binomial(link = "logit"),data=true_yeld)
true_LU_Total_noage_yr<-glmmTMB(Fecundity~LU_Total
                                +(1|DeerYear)
                                +(1|Female),
                                family = binomial(link = "logit"),data=true_yeld)
summary(true_Hinds_noage_yr)
summary(true_Adults_noage_yr)
summary(true_Total_noage_yr)
summary(true_LU_Total_noage_yr)

#naive models----
naive_Hinds<-glmmTMB(Fecundity~Hinds+Age+AgeSquared+DeerYear
                     +(1|DeerYear)
                     +(1|Female),
                     family = binomial(link = "logit"),data=naive)
naive_Adults<-glmmTMB(Fecundity~Adults+Age+AgeSquared+DeerYear
                      +(1|DeerYear)
                      +(1|Female),
                      family = binomial(link = "logit"),data=naive)
naive_Total<-glmmTMB(Fecundity~Total+Age+AgeSquared+DeerYear
                     +(1|DeerYear)
                     +(1|Female),
                     family = binomial(link = "logit"),data=naive)
naive_LU_Total<-glmmTMB(Fecundity~LU_Total+Age+AgeSquared+DeerYear
                        +(1|DeerYear)
                        +(1|Female),
                        family = binomial(link = "logit"),data=naive)
summary(naive_Hinds)
summary(naive_Adults)
summary(naive_Total)
summary(naive_LU_Total)

naive_Hinds_noage<-glmmTMB(Fecundity~Hinds+DeerYear
                           +(1|DeerYear)
                           +(1|Female),
                           family = binomial(link = "logit"),data=naive)
naive_Adults_noage<-glmmTMB(Fecundity~Adults+DeerYear
                            +(1|DeerYear)
                            +(1|Female),
                            family = binomial(link = "logit"),data=naive)
naive_Total_noage<-glmmTMB(Fecundity~Total+DeerYear
                           +(1|DeerYear)
                           +(1|Female),
                           family = binomial(link = "logit"),data=naive)
naive_LU_Total_noage<-glmmTMB(Fecundity~LU_Total+DeerYear
                              +(1|DeerYear)
                              +(1|Female),
                              family = binomial(link = "logit"),data=naive)
summary(naive_Hinds_noage)
summary(naive_Adults_noage)
summary(naive_Total_noage)
summary(naive_LU_Total_noage)

naive_Hinds_noage_yr<-glmmTMB(Fecundity~Hinds
                              +(1|DeerYear)
                              +(1|Female),
                              family = binomial(link = "logit"),data=naive)
naive_Adults_noage_yr<-glmmTMB(Fecundity~Adults
                               +(1|DeerYear)
                               +(1|Female),
                               family = binomial(link = "logit"),data=naive)
naive_Total_noage_yr<-glmmTMB(Fecundity~Total
                              +(1|DeerYear)
                              +(1|Female),
                              family = binomial(link = "logit"),data=naive)
naive_LU_Total_noage_yr<-glmmTMB(Fecundity~LU_Total
                                 +(1|DeerYear)
                                 +(1|Female),
                                 family = binomial(link = "logit"),data=naive)
summary(naive_Hinds_noage_yr)
summary(naive_Adults_noage_yr)
summary(naive_Total_noage_yr)
summary(naive_LU_Total_noage_yr)

#with age----

milk_mod_age<-glmmTMB(Fecundity~Hinds+Age+AgeSquared
                      +(1|DeerYear)
                      +(1|Female),
                      family = binomial(link = "logit"),data=milk)
summary(milk_mod_age)

yeld_mod_age<-glmmTMB(Fecundity~Hinds+Age+AgeSquared
                      +ReprodStatus
                      +(1|DeerYear)
                      +(1|Female),
                      family = binomial(link = "logit"),data=yeld)
summary(yeld_mod_age)

naive_mod_age<-glmmTMB(Fecundity~Hinds+Age+AgeSquared
                       +(1|DeerYear)
                       +(1|Female),
                       family = binomial(link = "logit"),data=naive)
summary(naive_mod_age)

#models without age and year and interaction----
Hinds_fecundity<-glmmTMB(Fecundity~Hinds+ReprodStatus+(1|DeerYear)+(1|Female),family = binomial(link = "logit"),data=rut)

Adults_fecundity<-glmmTMB(Fecundity~Adults+ReprodStatus+(1|DeerYear)+(1|Female),family = binomial(link = "logit"),data=rut)

Total_fecundity<-glmmTMB(Fecundity~Total+ReprodStatus+(1|DeerYear)+(1|Female),family = binomial(link = "logit"),data=rut)

LU_Total_fecundity<-glmmTMB(Fecundity~LU_Total+ReprodStatus+(1|DeerYear)+(1|Female),family = binomial(link = "logit"),data=rut)

summary(Hinds_fecundity)
summary(Adults_fecundity)
summary(Total_fecundity)
summary(LU_Total_fecundity)

#models with interaction without age and year----
int_Hinds_fecundity<-glmmTMB(Fecundity~Hinds*ReprodStatus+(1|DeerYear)+(1|Female),family = binomial(link = "logit"),data=rut)

int_Adults_fecundity<-glmmTMB(Fecundity~Adults*ReprodStatus+(1|DeerYear)+(1|Female),family = binomial(link = "logit"),data=rut)

int_Total_fecundity<-glmmTMB(Fecundity~Total*ReprodStatus+(1|DeerYear)+(1|Female),family = binomial(link = "logit"),data=rut)

int_LU_Total_fecundity<-glmmTMB(Fecundity~LU_Total*ReprodStatus+(1|DeerYear)+(1|Female),family = binomial(link = "logit"),data=rut)

summary(int_Hinds_fecundity)
summary(int_Adults_fecundity)
summary(int_Total_fecundity)
summary(int_LU_Total_fecundity)

#models with  year
yr_Hinds_fecundity<-glmmTMB(Fecundity~Hinds+ReprodStatus+DeerYear+(1|DeerYear)+(1|Female),family = binomial(link = "logit"),data=rut)

yr_Adults_fecundity<-glmmTMB(Fecundity~Adults+ReprodStatus+DeerYear+(1|DeerYear)+(1|Female),family = binomial(link = "logit"),data=rut)

yr_Total_fecundity<-glmmTMB(Fecundity~Total+ReprodStatus+DeerYear+(1|DeerYear)+(1|Female),family = binomial(link = "logit"),data=rut)

yr_LU_Total_fecundity<-glmmTMB(Fecundity~LU_Total+ReprodStatus+DeerYear+(1|DeerYear)+(1|Female),family = binomial(link = "logit"),data=rut)

summary(yr_Hinds_fecundity)
summary(yr_Adults_fecundity)
summary(yr_Total_fecundity)
summary(yr_LU_Total_fecundity)

#models with year and interaction
int_yr_Hinds_fecundity<-glmmTMB(Fecundity~Hinds*ReprodStatus+DeerYear+(1|DeerYear)+(1|Female),family = binomial(link = "logit"),data=rut)

int_yr_Adults_fecundity<-glmmTMB(Fecundity~Adults*ReprodStatus+DeerYear+(1|DeerYear)+(1|Female),family = binomial(link = "logit"),data=rut)

int_yr_Total_fecundity<-glmmTMB(Fecundity~Total*ReprodStatus+DeerYear+(1|DeerYear)+(1|Female),family = binomial(link = "logit"),data=rut)

int_yr_LU_Total_fecundity<-glmmTMB(Fecundity~LU_Total*ReprodStatus+DeerYear+(1|DeerYear)+(1|Female),family = binomial(link = "logit"),data=rut)

summary(int_yr_Hinds_fecundity)
summary(int_yr_Adults_fecundity)
summary(int_yr_Total_fecundity)
summary(int_yr_LU_Total_fecundity)

#with age no year

str(rut)

Hinds_fecundity_age<-glmmTMB(Fecundity~Hinds+Age+AgeSquared+ReprodStatus+(1|DeerYear)+(1|Female),family = binomial(link = "logit"),data=rut)

Adults_fecundity_age<-glmmTMB(Fecundity~Adults+Age+AgeSquared+ReprodStatus+(1|DeerYear)+(1|Female),family = binomial(link = "logit"),data=rut)

Total_fecundity_age<-glmmTMB(Fecundity~Total+Age+AgeSquared+ReprodStatus+(1|DeerYear)+(1|Female),family = binomial(link = "logit"),data=rut)

LU_Total_fecundity_age<-glmmTMB(Fecundity~LU_Total+Age+AgeSquared+ReprodStatus+(1|DeerYear)+(1|Female),family = binomial(link = "logit"),data=rut)

summary(Hinds_fecundity_age)
summary(Adults_fecundity_age)
summary(Total_fecundity_age)
summary(LU_Total_fecundity_age)



# Filter out non-finite values from the Hinds column
rut_filtered <- rut %>%
  filter(is.finite(Hinds))

# Generate new data for prediction
new_data <- expand.grid(
  Hinds = seq(min(rut_filtered$Hinds, na.rm = TRUE), max(rut_filtered$Hinds, na.rm = TRUE), length.out = 100),
  Age = mean(rut_filtered$Age, na.rm = TRUE),
  AgeSquared = mean(rut_filtered$AgeSquared, na.rm = TRUE),
  ReprodStatus = unique(rut_filtered$ReprodStatus),
  DeerYear = unique(rut_filtered$DeerYear),
  Female = unique(rut_filtered$Female)
)

# Generate predictions with standard errors
predictions <- predict(Hinds_fecundity_age, newdata = new_data, se.fit = TRUE, re.form = NA, type = "response")
new_data$Fecundity_pred <- predictions$fit
new_data$SE <- predictions$se.fit
new_data <- new_data %>%
  mutate(
    lower = Fecundity_pred - 1.96 * SE,
    upper = Fecundity_pred + 1.96 * SE
  )

# Plot the actual data and the model's predictions with standard error bands
ggplot(data = rut_filtered, aes(x = Hinds, y = Fecundity)) +
  geom_point(alpha = 0.5, position = position_jitter(width = 0.2, height = 0.02)) +
  geom_line(data = new_data, aes(x = Hinds, y = Fecundity_pred), color = "blue") +
  geom_ribbon(data = new_data, aes(x = Hinds, ymin = lower, ymax = upper), alpha = 0.2, fill = "blue") +
  labs(title = "Model Fit of Fecundity vs Hinds with Standard Error Bands", 
       x = "Number of Hinds", y = "Fecundity (Probability)") +
  theme_minimal()



#with interaction and age no year 
int_Hinds_fecundity_age<-glmmTMB(Fecundity~Hinds*ReprodStatus+Age+AgeSquared+(1|DeerYear)+(1|Female),family = binomial(link = "logit"),data=rut)

int_Adults_fecundity_age<-glmmTMB(Fecundity~Adults*ReprodStatus+Age+AgeSquared+(1|DeerYear)+(1|Female),family = binomial(link = "logit"),data=rut)

int_Total_fecundity_age<-glmmTMB(Fecundity~Total*ReprodStatus+Age+AgeSquared+(1|DeerYear)+(1|Female),family = binomial(link = "logit"),data=rut)

int_LU_Total_fecundity_age<-glmmTMB(Fecundity~LU_Total*ReprodStatus+Age+AgeSquared+(1|DeerYear)+(1|Female),family = binomial(link = "logit"),data=rut)

summary(int_Hinds_fecundity_age)
summary(int_Adults_fecundity_age)
summary(int_Total_fecundity_age)
summary(int_LU_Total_fecundity_age)


#models with age and additional year
yr_Hinds_fecundity_age<-glmmTMB(Fecundity~Hinds+Age+AgeSquared+ReprodStatus+DeerYear+(1|DeerYear)+(1|Female),family = binomial(link = "logit"),data=rut)

yr_Adults_fecundity_age<-glmmTMB(Fecundity~Adults+Age+AgeSquared+ReprodStatus+DeerYear+(1|DeerYear)+(1|Female),family = binomial(link = "logit"),data=rut)

yr_Total_fecundity_age<-glmmTMB(Fecundity~Total+Age+AgeSquared+ReprodStatus+DeerYear+(1|DeerYear)+(1|Female),family = binomial(link = "logit"),data=rut)

yr_LU_Total_fecundity_age<-glmmTMB(Fecundity~LU_Total+Age+AgeSquared+ReprodStatus+DeerYear+(1|DeerYear)+(1|Female),family = binomial(link = "logit"),data=rut)

summary(yr_Hinds_fecundity_age)
summary(yr_Adults_fecundity_age)
summary(yr_Total_fecundity_age)
summary(yr_LU_Total_fecundity_age)

yr_noden_fecundity_age<-glmmTMB(Fecundity~Age+AgeSquared+ReprodStatus+DeerYear+(1|DeerYear)+(1|Female),family = binomial(link = "logit"),data=rut)
summary(yr_noden_fecundity_age)

noyr_noden_fecundity_age<-glmmTMB(Fecundity~Age+AgeSquared+ReprodStatus+DeerYear+(1|DeerYear)+(1|Female),family = binomial(link = "logit"),data=rut)
summary(noyr_noden_fecundity_age)


#models with interaction age and additional year
int_yr_Hinds_fecundity_age<-glmmTMB(Fecundity~Hinds*ReprodStatus+Age+AgeSquared+DeerYear+(1|DeerYear)+(1|Female),family = binomial(link = "logit"),data=rut)

int_yr_Adults_fecundity_age<-glmmTMB(Fecundity~Adults*ReprodStatus+Age+AgeSquared+DeerYear+(1|DeerYear)+(1|Female),family = binomial(link = "logit"),data=rut)

int_yr_Total_fecundity_age<-glmmTMB(Fecundity~Total*ReprodStatus+Age+AgeSquared+DeerYear+(1|DeerYear)+(1|Female),family = binomial(link = "logit"),data=rut)

int_yr_LU_Total_fecundity_age<-glmmTMB(Fecundity~LU_Total*ReprodStatus+Age+AgeSquared+DeerYear+(1|DeerYear)+(1|Female),family = binomial(link = "logit"),data=rut)

summary(int_yr_Hinds_fecundity_age)
summary(int_yr_Adults_fecundity_age)
summary(int_yr_Total_fecundity_age)
summary(int_yr_LU_Total_fecundity_age)

# anova(int_yr_Hinds_fecundity_age,int_yr_Adults_fecundity_age test = "Chisq")

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


#Plots
#Hinds----

portion_filtered <- portion %>% select(DeerYear,portion)

fecundity_pred <- rut %>% left_join(portion_filtered, by = "DeerYear")

predictions_Hinds_fecundity_yr <- ggpredict(yr_Hinds_fecundity_age, terms = "Hinds [all]")

predictions_Hinds_fecundity_yr$Hinds <- predictions_Hinds_fecundity_yr$x

predictions_Hinds_fecundity_noyr <- ggpredict(Hinds_fecundity_age, terms = "Hinds [all]")
predictions_Hinds_fecundity_noyr$Hinds <- predictions_Hinds_fecundity_noyr$x


fecundity_pred_Hinds_yr <- fecundity_pred %>% left_join(predictions_Hinds_fecundity_yr, by = "Hinds")

fecundity_pred_Hinds_noyr <- fecundity_pred %>% left_join(predictions_Hinds_fecundity_noyr, by = "Hinds")

# Add a Type column to differentiate the data sets
fecundity_pred_Hinds_yr$Type <- "With Year as Fixed effect"
fecundity_pred_Hinds_noyr$Type <- "Without Year as Fixed effect"

# Combine the data frames
combined_fecundity_Hinds <- rbind(
  fecundity_pred_Hinds_yr,
  fecundity_pred_Hinds_noyr
)

# Keep only unique points for plotting
unique_fecundity_Hinds <- combined_fecundity_Hinds %>%
  distinct(Hinds, portion,.keep_all = TRUE)


# Plot the combined data
ggplot(combined_fecundity_Hinds, aes(x = Hinds, y = Fecundity)) +
  geom_jitter(width = 1, height = 0.01, alpha = 0.008, size = 2) +
  geom_point(data = unique_fecundity_Hinds, aes(y = portion)) +
  geom_text(data = unique_fecundity_Hinds, aes(y = portion + 0.03, label = paste0("(", DeerYear, ")")), size = 2.5, color = "black") +
  geom_line(aes(y = predicted, color = Type)) +  # Predicted values with different colors
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = Type), alpha = 0.2) +  # Confidence intervals with different fills
  labs(
    x = "Hinds Density",
    y = "Predicted Probability of female reproduction",
    title = "Predicted Probability of female reproduction over Density\nwith 95% Confidence Interval"
  ) +
  scale_color_manual(values = c("With Year as Fixed effect" = "blue", "Without Year as Fixed effect" = "red")) +  # Customize line colors
  scale_fill_manual(values = c("With Year as Fixed effect" = "lightblue", "Without Year as Fixed effect" = "lightpink")) +  # Customize ribbon fills
  theme_minimal() +
  theme(
    legend.position = "bottom",  # Position the legend at the bottom
    legend.title = element_blank() ) # Optionally remove the legend title
  
#Adults----

predictions_Adults_fecundity_yr <- ggpredict(yr_Adults_fecundity_age, terms = "Adults [all]")

predictions_Adults_fecundity_yr$Adults <- predictions_Adults_fecundity_yr$x

predictions_Adults_fecundity_noyr <- ggpredict(Adults_fecundity_age, terms = "Adults [all]")
predictions_Adults_fecundity_noyr$Adults <- predictions_Adults_fecundity_noyr$x


fecundity_pred_Adults_yr <- fecundity_pred %>% left_join(predictions_Adults_fecundity_yr, by = "Adults")

fecundity_pred_Adults_noyr <- fecundity_pred %>% left_join(predictions_Adults_fecundity_noyr, by = "Adults")

# Add a Type column to differentiate the data sets
fecundity_pred_Adults_yr$Type <- "With Year as Fixed effect"
fecundity_pred_Adults_noyr$Type <- "Without Year as Fixed effect"

# Combine the data frames
combined_fecundity_Adults <- rbind(
  fecundity_pred_Adults_yr,
  fecundity_pred_Adults_noyr
)

# Keep only unique points for plotting
unique_fecundity_Adults <- combined_fecundity_Adults %>%
  distinct(Adults, portion,.keep_all = TRUE)


# Plot the combined data
ggplot(combined_fecundity_Adults, aes(x = Adults, y = Fecundity)) +
  geom_point(shape=1) +
  geom_point(data = unique_fecundity_Adults, aes(y = portion)) +
  geom_text(data = unique_fecundity_Adults, aes(y = portion + 0.03, label = paste0("(", DeerYear, ")")), size = 2.5, color = "black") +
  geom_line(aes(y = predicted, color = Type)) +  # Predicted values with different colors
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = Type), alpha = 0.2) +  # Confidence intervals with different fills
  labs(
    x = "Adults Density",
    y = "Predicted Probability of female reproduction",
    title = "Predicted Probability of female reproduction over Density\nwith 95% Confidence Interval"
  ) +
  scale_color_manual(values = c("With Year as Fixed effect" = "blue", "Without Year as Fixed effect" = "red")) +  # Customize line colors
  scale_fill_manual(values = c("With Year as Fixed effect" = "lightblue", "Without Year as Fixed effect" = "lightpink")) +  # Customize ribbon fills
  theme_minimal() +
  theme(
    legend.position = "bottom",  # Position the legend at the bottom
    legend.title = element_blank())  # Optionally remove the legend title
  
#Total----

predictions_Total_fecundity_yr <- ggpredict(yr_Total_fecundity_age, terms = "Total [all]")

predictions_Total_fecundity_yr$Total <- predictions_Total_fecundity_yr$x

predictions_Total_fecundity_noyr <- ggpredict(Total_fecundity_age, terms = "Total [all]")
predictions_Total_fecundity_noyr$Total <- predictions_Total_fecundity_noyr$x


fecundity_pred_Total_yr <- fecundity_pred %>% left_join(predictions_Total_fecundity_yr, by = "Total")

fecundity_pred_Total_noyr <- fecundity_pred %>% left_join(predictions_Total_fecundity_noyr, by = "Total")

# Add a Type column to differentiate the data sets
fecundity_pred_Total_yr$Type <- "With Year as Fixed effect"
fecundity_pred_Total_noyr$Type <- "Without Year as Fixed effect"

# Combine the data frames
combined_fecundity_Total <- rbind(
  fecundity_pred_Total_yr,
  fecundity_pred_Total_noyr
)

# Keep only unique points for plotting
unique_fecundity_Total <- combined_fecundity_Total %>%
  distinct(Total, portion,.keep_all = TRUE)


# Plot the combined data
ggplot(combined_fecundity_Total, aes(x = Total, y = Fecundity)) +
  geom_point(shape=1) +
  geom_point(data = unique_fecundity_Total, aes(y = portion)) +
  geom_text(data = unique_fecundity_Total, aes(y = portion + 0.03, label = paste0("(", DeerYear, ")")), size = 2.5, color = "black") +
  geom_line(aes(y = predicted, color = Type)) +  # Predicted values with different colors
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = Type), alpha = 0.2) +  # Confidence intervals with different fills
  labs(
    x = "Total Density",
    y = "Predicted Probability of female reproduction",
    title = "Predicted Probability of female reproduction over Density\nwith 95% Confidence Interval"
  ) +
  scale_color_manual(values = c("With Year as Fixed effect" = "blue", "Without Year as Fixed effect" = "red")) +  # Customize line colors
  scale_fill_manual(values = c("With Year as Fixed effect" = "lightblue", "Without Year as Fixed effect" = "lightpink")) +  # Customize ribbon fills
  theme_minimal() +
  theme(
    legend.position = "bottom",  # Position the legend at the bottom
    legend.title = element_blank())  # Optionally remove the legend title
  
#LU_Total----

predictions_LU_Total_fecundity_yr <- ggpredict(yr_LU_Total_fecundity_age, terms = "LU_Total [all]")
predictions_LU_Total_fecundity_yr$LU_Total <- predictions_LU_Total_fecundity_yr$x


predictions_LU_Total_fecundity_noyr <- ggpredict(LU_Total_fecundity_age, terms = "LU_Total [all]")
predictions_LU_Total_fecundity_noyr$LU_Total <- predictions_LU_Total_fecundity_noyr$x


fecundity_pred_LU_Total_yr <- fecundity_pred %>% left_join(predictions_LU_Total_fecundity_yr, by = "LU_Total")

fecundity_pred_LU_Total_noyr <- fecundity_pred %>% left_join(predictions_LU_Total_fecundity_noyr, by = "LU_Total")

# Add a Type column to differentiate the data sets
fecundity_pred_LU_Total_yr$Type <- "With Year as Fixed effect"
fecundity_pred_LU_Total_noyr$Type <- "Without Year as Fixed effect"

# Combine the data frames
combined_fecundity_LU_Total <- rbind(
  fecundity_pred_LU_Total_yr,
  fecundity_pred_LU_Total_noyr
)

# Keep only unique points for plotting
unique_fecundity_LU_Total <- combined_fecundity_LU_Total %>%
  distinct(LU_Total, portion,.keep_all = TRUE)


# Plot the combined data
ggplot(combined_fecundity_LU_Total, aes(x = LU_Total, y = Fecundity)) +
  geom_point(shape=1) +
  geom_point(data = unique_fecundity_LU_Total, aes(y = portion)) +
  geom_text(data = unique_fecundity_LU_Total, aes(y = portion + 0.03, label = paste0("(", DeerYear, ")")), size = 2.5, color = "black") +
  geom_line(aes(y = predicted, color = Type)) +  # Predicted values with different colors
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = Type), alpha = 0.2) +  # Confidence intervals with different fills
  labs(
    x = "LU_Total Density",
    y = "Predicted Probability of female reproduction",
    title = "Predicted Probability of female reproduction over Density\nwith 95% Confidence Interval"
  ) +
  scale_color_manual(values = c("With Year as Fixed effect" = "blue", "Without Year as Fixed effect" = "red")) +  # Customize line colors
  scale_fill_manual(values = c("With Year as Fixed effect" = "lightblue", "Without Year as Fixed effect" = "lightpink")) +  # Customize ribbon fills
  theme_minimal() +
  theme(
    legend.position = "bottom",  # Position the legend at the bottom
    legend.title = element_blank() ) # Optionally remove the legend title
  



print(predictions_Hinds_fecundity_yr)
print(predictions_Hinds_fecundity_noyr)
print(predictions_Adults_fecundity_yr)
print(predictions_Adults_fecundity_noyr)
print(predictions_Total_fecundity_yr)
print(predictions_Total_fecundity_noyr)
print(predictions_LU_Total_fecundity_yr)
print(predictions_LU_Total_fecundity_noyr)
