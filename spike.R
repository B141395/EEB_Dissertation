library(tidyverse)
library(lme4)
library(ggplot2)
library(lmerTest)
library(glmmTMB)

Spike<-read.csv("spike.csv")

Density<-read.csv("Population_estimate_calculations.csv")

Spike<-Spike%>% filter(!DeerYear %in% c(1969))

PopD<-Density %>% select (DeerYear,Hinds,Stags,Calves,Calves_M,Calves_F,Adults,Total,LU_Total)

PopD7172<-data.frame(
  DeerYear = c(1971, 1972),
  Hinds = c(57, 66) )

PopD_new<-bind_rows(PopD7172,PopD)



ggplot(PopD_new, aes(x = DeerYear)) +
  geom_line(aes(y = Hinds, color = "Hinds"), size = 2) +
  geom_line(aes(y = Stags, color = "Stags"), size = 2) +
  geom_line(aes(y = Adults, color = "Adults"), size = 2) + 
  geom_line(aes(y = Total, color = "Total"), size = 2) +
  geom_line(aes(y = LU_Total, color = "LU_Total"), size = 2) +
  labs(title = "Different Deer Density Measures per Year",
       x = "Year",
       y = "Deer Density") +
  scale_color_manual(values = c("Hinds" = "red", "Stags" = "blue", "Adults" = "black", 
                                "Total" = "green", "LU_Total" = "orange"),
                     breaks = c("Hinds", "Stags", "Adults", "Total", "LU_Total"),
                     labels = c("Hinds", "Stags", "Adults", "Total", "Livestock Units"))


ggplot(PopD_new, aes(x = DeerYear)) +
  geom_line(aes(y = Hinds, color = "Hinds"), size = 2) +
  geom_line(aes(y = Stags, color = "Stags"), size = 2) +
  labs(title = "Different Deer Density Measures per Year",
       x = "Year",
       y = "Deer Density") +
  scale_color_manual(values = c("red", "blue"),
                     labels = c("Hinds", "Stags"))



Spike <- Spike %>%
  left_join(PopD, by = "DeerYear")

Spike96<-Spike%>% filter(DeerYear <= 1996)

AvgSpike <- Spike %>%
  group_by(DeerYear) %>%
  summarise(
    mean_spike = mean(AvgSpike),
    sample_size = n(),
    se_spike = sd(AvgSpike) / sqrt(n())
  )

AvgSpike <- AvgSpike %>%
  left_join(PopD, by = "DeerYear")





AvgSpike96 <- AvgSpike%>% filter(DeerYear <= 1996)

ggplot(data = AvgSpike, aes(x = DeerYear, y = mean_spike)) +
  geom_line(color = "black", size = 0.75) +
  geom_point(color = "black", size = 2,shape = 15) +
  geom_errorbar(aes(ymin = mean_spike - se_spike, ymax = mean_spike + se_spike), width = 0.2, color = "black") +
  labs(title = "Mean cohort yearling anter length for cohorts born 1970-2021", x = "Year of Birth", y = "Yearling Mean Antler Length (mm)") +
  theme_minimal() +
  geom_text(aes(y = mean_spike + se_spike + 0.5, label = paste0("(", sample_size, ")")), size = 3, color = "black") 


ggplot(data = AvgSpike96, aes(x = DeerYear, y = mean_spike)) +
  geom_line(color = "black", size = 0.75) +
  geom_point(color = "black", size = 2,shape = 15) +
  geom_errorbar(aes(ymin = mean_spike - se_spike, ymax = mean_spike + se_spike), width = 0.2, color = "black") +
  labs(title = "Mean cohort yearling anter length for cohorts born 1970-1996", x = "Year of Birth", y = "Yearling Mean Antler Length (mm)") +
  theme_minimal() +
  geom_text(aes(y = mean_spike + se_spike + 0.5, label = paste0("(", sample_size, ")")), size = 3, color = "black") 

Hinds_spike<-glmmTMB(AvgSpike~Hinds
                            +BirthWt
                            +(1|DeerYear)
                            +(1|MumCode),data=Spike)
summary(Hinds_spike)

Hinds_spike96<-glmmTMB(AvgSpike~Hinds
                     +BirthWt
                     +(1|DeerYear)
                     +(1|MumCode),data=Spike96)
summary(Hinds_spike96)

SpikeByYear<-glm(mean_spike~Hinds,data=AvgSpike)
summary(SpikeByYear)

SpikeByYear96<-glm(mean_spike~Hinds,data=AvgSpike96)
summary(SpikeByYear96)


Hinds_spike_simple<-glmmTMB(AvgSpike~Hinds
               +BirthWt
               +(1|DeerYear)
               +(1|MumCode),data=Spike)

Adults_spike_simple<-glmmTMB(AvgSpike~Adults
                     +BirthWt
                     +(1|DeerYear)
                     +(1|MumCode),data=Spike)

Total_spike_simple<-glmmTMB(AvgSpike~Total
                     +BirthWt
                     +(1|DeerYear)
                     +(1|MumCode),data=Spike)

LU_Total_spike_simple<-glmmTMB(AvgSpike~LU_Total
                     +BirthWt
                     +(1|DeerYear)
                     +(1|MumCode),data=Spike)

summary(Hinds_spike_simple)
summary(Adults_spike_simple)
summary(Total_spike_simple)
summary(LU_Total_spike_simple)

Hinds_spike<-glmmTMB(AvgSpike~Hinds
                     +DeerYear
                     +BirthWt
                     +(1|DeerYear)
                     +(1|MumCode),data=Spike)

Adults_spike<-glmmTMB(AvgSpike~Adults
                      +DeerYear
                      +BirthWt
                      +(1|DeerYear)
                      +(1|MumCode),data=Spike)

Total_spike<-glmmTMB(AvgSpike~Total
                     +DeerYear
                     +BirthWt
                     +(1|DeerYear)
                     +(1|MumCode),data=Spike)

LU_Total_spike<-glmmTMB(AvgSpike~LU_Total
                        +DeerYear
                        +BirthWt
                        +(1|DeerYear)
                        +(1|MumCode),data=Spike)

summary(Hinds_spike)
summary(Adults_spike)
summary(Total_spike)
summary(LU_Total_spike)

