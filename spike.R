library(tidyverse)
library(lme4)
library(ggplot2)
library(lmerTest)
library(glmmTMB)

Spike<-read.csv("spike.csv")
nowt<-read.csv("Spike_nowt.csv")
birth<-read.csv("Spike_nowt.csv")
birth_w_wt<-read.csv("Spike.csv")


birth$DeerYear<-birth$DeerYear-1
birth_w_wt$DeerYear<-birth_w_wt$DeerYear-1


Density<-read.csv("Population_estimate_calculations.csv")


Spike<-Spike%>% filter(!DeerYear %in% c(1969))

nowt<-nowt%>% filter(!DeerYear %in% c(1969))


PopD<-Density %>% select (DeerYear,Hinds,Stags,Calves,Calves_M,Calves_F,Adults,Total,LU_Total)

PopD7172<-data.frame(
  DeerYear = c(1971, 1972),
  Hinds = c(57, 66) )

PopD_new<-bind_rows(PopD7172,PopD)

nowt7172<-read.csv("Spike_nowt.csv")


ggplot(PopD_new, aes(x = DeerYear)) +
  geom_line(aes(y = Hinds, color = "Hinds"), size = 1.5) +
  geom_line(aes(y = Stags, color = "Stags"), size = 1.5) +
  geom_line(aes(y = Adults, color = "Adults"), size = 1.5) + 
  geom_line(aes(y = Total, color = "Total"), size = 1.5) +
  geom_line(aes(y = LU_Total, color = "LU_Total"), size = 1.5) +
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




FWS_spike<-read.csv("FWS_data.csv")
FWS_spike<-FWS_spike%>% filter(!DeerYear %in% c(2022, 2023))
FWS_spike<-FWS_spike%>% filter(!Sex %in% c(1,3))
rate_spike<- FWS_spike %>%
  group_by(DeerYear) %>%
  summarize(
    count = n(),                         
    survived = sum(FWSurvival, na.rm = TRUE),
    rate = survived/count,
    Mrate = 1-rate
  )
rate_spike<-rate_spike %>% select(DeerYear,rate)

Spike <- Spike %>%
  left_join(PopD, by = "DeerYear")

Spike<-na.omit(Spike)

Spike <- Spike %>%
  left_join(rate_spike, by = "DeerYear")

nowt<- nowt %>%
  left_join(PopD, by = "DeerYear")

nowt <- nowt %>%
  left_join(rate_spike, by = "DeerYear")

birth<- birth %>%
  left_join(PopD, by = "DeerYear")

birth <- birth %>%
  left_join(rate_spike, by = "DeerYear")

birth_w_wt<- birth_w_wt %>%
  left_join(PopD, by = "DeerYear")

birth_w_wt <- birth_w_wt %>%
  left_join(rate_spike, by = "DeerYear")




nowt7172<- nowt7172 %>%
  left_join(PopD_new, by = "DeerYear")

Spike96<-Spike%>% filter(DeerYear <= 1996)

nowt96<-nowt%>% filter(DeerYear <= 1996)

birth96<-birth%>% filter(DeerYear <= 1996)

birth_w_wt96<-birth_w_wt%>% filter(DeerYear <= 1996)

nowt717296<-nowt7172%>% filter(DeerYear <= 1996)

nowt7172Hinds_spike<-glmmTMB(AvgSpike~Hinds+(1|DeerYear),data=nowt7172)

nowt717296Hinds_spike<-glmmTMB(AvgSpike~Hinds+(1|DeerYear),data=nowt717296)

summary(nowt7172Hinds_spike)
summary(nowt717296Hinds_spike)

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
  labs(title = "Mean cohort yearling anter length for cohorts born 1970-2021", x = "Year of Birth", y = "Yearling Mean Antler Length (inches)") +
  theme_minimal() +
  geom_text(aes(y = mean_spike + se_spike + 0.5, label = paste0("(", sample_size, ")")), size = 3, color = "black") 


ggplot(data = AvgSpike96, aes(x = DeerYear, y = mean_spike)) +
  geom_line(color = "black", size = 0.75) +
  geom_point(color = "black", size = 2,shape = 15) +
  geom_errorbar(aes(ymin = mean_spike - se_spike, ymax = mean_spike + se_spike), width = 0.2, color = "black") +
  labs(title = "Mean cohort yearling anter length for cohorts born 1970-1996", x = "Year of Birth", y = "Yearling Mean Antler Length (inches)") +
  theme_minimal() +
  geom_text(aes(y = mean_spike + se_spike + 0.5, label = paste0("(", sample_size, ")")), size = 3, color = "black") 


Avg_nowt <- nowt %>%
  group_by(DeerYear) %>%
  summarise(
    mean_spike = mean(AvgSpike),
    sample_size = n(),
    se_spike = sd(AvgSpike) / sqrt(n())
  )

Avg_nowt <- Avg_nowt %>%
  left_join(PopD, by = "DeerYear")

Avg_nowt96 <- Avg_nowt%>% filter(DeerYear <= 1996)

ggplot(data = Avg_nowt, aes(x = DeerYear, y = mean_spike)) +
  geom_line(color = "black", size = 0.75) +
  geom_point(color = "black", size = 2,shape = 15) +
  geom_errorbar(aes(ymin = mean_spike - se_spike, ymax = mean_spike + se_spike), width = 0.2, color = "black") +
  labs(title = "Mean cohort yearling anter length for cohorts born 1970-2021", x = "Year of Birth", y = "Yearling Mean Antler Length (inches)") +
  theme_minimal() +
  geom_text(aes(y = mean_spike + se_spike + 0.5, label = paste0("(", sample_size, ")")), size = 3, color = "black") 


ggplot(data = Avg_nowt96, aes(x = DeerYear, y = mean_spike)) +
  geom_line(color = "black", size = 0.75) +
  geom_point(color = "black", size = 2,shape = 15) +
  geom_errorbar(aes(ymin = mean_spike - se_spike, ymax = mean_spike + se_spike), width = 0.2, color = "black") +
  labs(title = "Mean cohort yearling anter length for cohorts born 1970-1996", x = "Year of Birth", y = "Yearling Mean Antler Length (inches)") +
  theme_minimal() +
  geom_text(aes(y = mean_spike + se_spike + 0.5, label = paste0("(", sample_size, ")")), size = 3, color = "black") 


ggplot(data = Avg_nowt, aes(x = Hinds, y = mean_spike)) +
  geom_smooth(method = "lm",color = "black", size = 0.75) +
  geom_point(color = "black", size = 2,shape = 15) +
  labs(title = "Mean cohort yearling anter length over Hind Density", x = "Hind Density", y = "Yearling Mean Antler Length (inches)") +
  theme_minimal() +
  geom_text(aes(y = mean_spike+ 0.1, label = paste0("(", DeerYear, ")")), size = 2, color = "black") 

ggplot(data = Avg_nowt, aes(x = Adults, y = mean_spike)) +
  geom_smooth(method = "lm",color = "black", size = 0.75) +
  geom_point(color = "black", size = 2,shape = 15) +
  labs(title = "Mean cohort yearling anter length over Adult Density", x = "Adult Density", y = "Yearling Mean Antler Length (inches)") +
  theme_minimal() +
  geom_text(aes(y = mean_spike+ 0.1, label = paste0("(", DeerYear, ")")), size = 2, color = "black") 

ggplot(data = Avg_nowt, aes(x = Total, y = mean_spike)) +
  geom_smooth(method = "lm",color = "black", size = 0.75) +
  geom_point(color = "black", size = 2,shape = 15) +
  labs(title = "Mean cohort yearling anter length over Total Density", x = "Total Density", y = "Yearling Mean Antler Length (inches)") +
  theme_minimal() +
  geom_text(aes(y = mean_spike+ 0.1, label = paste0("(", DeerYear, ")")), size = 2, color = "black") 

ggplot(data = Avg_nowt, aes(x = LU_Total, y = mean_spike)) +
  geom_smooth(method = "lm",color = "black", size = 0.75) +
  geom_point(color = "black", size = 2,shape = 15) +
  labs(title = "Mean cohort yearling anter length over Livestock units", x = "Livestock units", y = "Yearling Mean Antler Length (inches)") +
  theme_minimal() +
  geom_text(aes(y = mean_spike+ 0.1, label = paste0("(", DeerYear, ")")), size = 2, color = "black") 





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



Hinds_spike_simple96<-glmmTMB(AvgSpike~Hinds
                            +BirthWt
                            +(1|DeerYear)
                            +(1|MumCode),data=Spike96)

Adults_spike_simple96<-glmmTMB(AvgSpike~Adults
                             +BirthWt
                             +(1|DeerYear)
                             +(1|MumCode),data=Spike96)

Total_spike_simple96<-glmmTMB(AvgSpike~Total
                            +BirthWt
                            +(1|DeerYear)
                            +(1|MumCode),data=Spike96)

LU_Total_spike_simple96<-glmmTMB(AvgSpike~LU_Total
                               +BirthWt
                               +(1|DeerYear)
                               +(1|MumCode),data=Spike96)

summary(Hinds_spike_simple96)
summary(Adults_spike_simple96)
summary(Total_spike_simple96)
summary(LU_Total_spike_simple96)

#Initial Models With Year and Weight----

str(Spike)

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


library(ggeffects)

AvgSpike_filtered <- AvgSpike %>% select(DeerYear, mean_spike,sample_size,se_spike)

Spike_pred <- Spike %>% left_join(AvgSpike_filtered, by = "DeerYear")

predictions_1 <- ggpredict(Hinds_spike, terms = "Hinds [all]")

predictions_1$Hinds <- predictions_1$x

predictions_2 <- ggpredict(Hinds_spike_simple, terms = "Hinds [all]")
predictions_2$Hinds <- predictions_2$x


Spike_pred_yr <- Spike_pred %>% left_join(predictions_1, by = "Hinds")

Spike_pred_noyr <- Spike_pred %>% left_join(predictions_2, by = "Hinds")

ggplot(Spike_pred_yr, aes(x = Hinds, y = mean_spike)) +
  geom_point()+
  geom_line(aes(y= predicted),color = "blue") +  # Predicted values
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "lightblue") +  # Confidence interval
  labs(
    x = "Hinds",
    y = "Predicted Average Spike",
    title = "Predicted Average Spike with 95% Confidence Interval"
  ) +
  theme_minimal()

ggplot(Spike_pred_noyr, aes(x = Hinds, y = mean_spike)) +
  geom_point()+
  geom_line(aes(y= predicted),color = "red") +  # Predicted values
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "lightpink") +  # Confidence interval
  labs(
    x = "Hinds",
    y = "Predicted Average Spike",
    title = "Predicted Average Spike with 95% Confidence Interval"
  ) +
  theme_minimal()


#With & Without Year models for Hinds

# Assume Spike_pred_yr and Spike_pred_noyr are already available and structured similarly

# Add a Type column to differentiate the data sets
Spike_pred_yr$Type <- "With Year as Fixed effect"
Spike_pred_noyr$Type <- "Without Year as Fixed effect"

# Combine the data frames
combined_data <- rbind(
  Spike_pred_yr,
  Spike_pred_noyr
)

# Keep only unique points for plotting
unique_points <- combined_data %>%
  distinct(Hinds, mean_spike, .keep_all = TRUE)


# Plot the combined data
ggplot(combined_data, aes(x = Hinds, y = mean_spike)) +
  geom_point(data = unique_points) +
  geom_text(data = unique_points, aes(y = mean_spike + 0.1, label = paste0("(", DeerYear, ")")), size = 2.5, color = "black") +
  geom_line(aes(y = predicted, color = Type)) +  # Predicted values with different colors
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = Type), alpha = 0.2) +  # Confidence intervals with different fills
  labs(
    x = "Hind Density",
    y = "Mean Yearling Antler Length (inches)",
    title = "Predicted Mean Yearling Antler Length over Density\nwith 95% Confidence Interval"
  ) +
  scale_color_manual(values = c("With Year as Fixed effect" = "blue", "Without Year as Fixed effect" = "red")) +  # Customize line colors
  scale_fill_manual(values = c("With Year as Fixed effect" = "lightblue", "Without Year as Fixed effect" = "lightpink")) +  # Customize ribbon fills
  theme_minimal() +
  theme(
    legend.position = "bottom",  # Position the legend at the bottom
    legend.title = element_blank()  # Optionally remove the legend title
  )





Hinds_spike96<-glmmTMB(AvgSpike~Hinds
                     +DeerYear
                     +BirthWt
                     +(1|DeerYear)
                     +(1|MumCode),data=Spike96)

Adults_spike96<-glmmTMB(AvgSpike~Adults
                      +DeerYear
                      +BirthWt
                      +(1|DeerYear)
                      +(1|MumCode),data=Spike96)

Total_spike96<-glmmTMB(AvgSpike~Total
                     +DeerYear
                     +BirthWt
                     +(1|DeerYear)
                     +(1|MumCode),data=Spike96)

LU_Total_spike96<-glmmTMB(AvgSpike~LU_Total
                        +DeerYear
                        +BirthWt
                        +(1|DeerYear)
                        +(1|MumCode),data=Spike96)

summary(Hinds_spike96)
summary(Adults_spike96)
summary(Total_spike96)
summary(LU_Total_spike96)


#Birth Year with weight and without year----
birthHinds_spike<-glmmTMB(AvgSpike~Hinds
                     +BirthWt
                     +(1|DeerYear)
                     +(1|MumCode),data=birth_w_wt)

birthAdults_spike<-glmmTMB(AvgSpike~Adults
                      +BirthWt
                      +(1|DeerYear)
                      +(1|MumCode),data=birth_w_wt)

birthTotal_spike<-glmmTMB(AvgSpike~Total
                     +BirthWt
                     +(1|DeerYear)
                     +(1|MumCode),data=birth_w_wt)

birthLU_Total_spike<-glmmTMB(AvgSpike~LU_Total
                        +BirthWt
                        +(1|DeerYear)
                        +(1|MumCode),data=birth_w_wt)

summary(birthHinds_spike)
summary(birthAdults_spike)
summary(birthTotal_spike)
summary(birthLU_Total_spike)

birthHinds_spike96<-glmmTMB(AvgSpike~Hinds
                       +BirthWt
                       +(1|DeerYear)
                       +(1|MumCode),data=birth_w_wt96)

birthAdults_spike96<-glmmTMB(AvgSpike~Adults
                        +BirthWt
                        +(1|DeerYear)
                        +(1|MumCode),data=birth_w_wt96)

birthTotal_spike96<-glmmTMB(AvgSpike~Total
                       +BirthWt
                       +(1|DeerYear)
                       +(1|MumCode),data=birth_w_wt96)

birthLU_Total_spike96<-glmmTMB(AvgSpike~LU_Total
                          +BirthWt
                          +(1|DeerYear)
                          +(1|MumCode),data=birth_w_wt96)

summary(birthHinds_spike96)
summary(birthAdults_spike96)
summary(birthTotal_spike96)
summary(birthLU_Total_spike96)

#birth with year and weight
yrbirthHinds_spike<-glmmTMB(AvgSpike~Hinds
                          +DeerYear
                          +BirthWt
                          +(1|DeerYear)
                          +(1|MumCode),data=birth_w_wt)

yrbirthAdults_spike<-glmmTMB(AvgSpike~Adults
                           +DeerYear
                           +BirthWt
                           +(1|DeerYear)
                           +(1|MumCode),data=birth_w_wt)

yrbirthTotal_spike<-glmmTMB(AvgSpike~Total
                          +DeerYear
                          +BirthWt
                          +(1|DeerYear)
                          +(1|MumCode),data=birth_w_wt)

yrbirthLU_Total_spike<-glmmTMB(AvgSpike~LU_Total
                             +DeerYear
                             +BirthWt
                             +(1|DeerYear)
                             +(1|MumCode),data=birth_w_wt)

summary(yrbirthHinds_spike)
summary(yrbirthAdults_spike)
summary(yrbirthTotal_spike)
summary(yrbirthLU_Total_spike)

str(birth_w_wt)

yrbirthHinds_spike96<-glmmTMB(AvgSpike~Hinds
                            +DeerYear
                            +BirthWt
                            +(1|DeerYear)
                            +(1|MumCode),data=birth_w_wt96)

yrbirthAdults_spike96<-glmmTMB(AvgSpike~Adults
                             +DeerYear
                             +BirthWt
                             +(1|DeerYear)
                             +(1|MumCode),data=birth_w_wt96)

yrbirthTotal_spike96<-glmmTMB(AvgSpike~Total
                            +DeerYear
                            +BirthWt
                            +(1|DeerYear)
                            +(1|MumCode),data=birth_w_wt96)

yrbirthLU_Total_spike96<-glmmTMB(AvgSpike~LU_Total
                               +DeerYear
                               +BirthWt
                               +(1|DeerYear)
                               +(1|MumCode),data=birth_w_wt96)

summary(yrbirthHinds_spike96)
summary(yrbirthAdults_spike96)
summary(yrbirthTotal_spike96)
summary(yrbirthLU_Total_spike96)

#Without Weight----
nowtHinds_spike<-glmmTMB(AvgSpike~Hinds+DeerYear+(1|DeerYear),data=nowt)

nowtAdults_spike<-glmmTMB(AvgSpike~Adults+DeerYear+(1|DeerYear),data=nowt)

nowtTotal_spike<-glmmTMB(AvgSpike~Total+DeerYear+(1|DeerYear),data=nowt)

nowtLU_Total_spike<-glmmTMB(AvgSpike~LU_Total+DeerYear+(1|DeerYear),data=nowt)

summary(nowtHinds_spike)
summary(nowtAdults_spike)
summary(nowtTotal_spike)
summary(nowtLU_Total_spike)

#without weight to 1996----
nowt96Hinds_spike<-glmmTMB(AvgSpike~Hinds+DeerYear+(1|DeerYear),data=nowt96)

nowt96Adults_spike<-glmmTMB(AvgSpike~Adults+DeerYear+(1|DeerYear),data=nowt96)

nowt96Total_spike<-glmmTMB(AvgSpike~Total+DeerYear+(1|DeerYear),data=nowt96)

nowt96LU_Total_spike<-glmmTMB(AvgSpike~LU_Total+DeerYear+(1|DeerYear),data=nowt96)

summary(nowt96Hinds_spike)
summary(nowt96Adults_spike)
summary(nowt96Total_spike)
summary(nowt96LU_Total_spike)

#Using Birth Year w/o weight----
nowtbirthHinds_spike<-glmmTMB(AvgSpike~Hinds+DeerYear +(1|DeerYear),data=birth)

nowtbirthAdults_spike<-glmmTMB(AvgSpike~Adults+DeerYear+(1|DeerYear),data=birth)

nowtbirthTotal_spike<-glmmTMB(AvgSpike~Total+DeerYear+(1|DeerYear),data=birth)

nowtbirthLU_Total_spike<-glmmTMB(AvgSpike~LU_Total+DeerYear+(1|DeerYear),data=birth)

summary(nowtbirthHinds_spike)
summary(nowtbirthAdults_spike)
summary(nowtbirthTotal_spike)
summary(nowtbirthLU_Total_spike)

#Birth Year w/o weight to 1996----
nowtbirth96Hinds_spike<-glmmTMB(AvgSpike~Hinds+DeerYear+(1|DeerYear),data=birth96)

nowtbirth96Adults_spike<-glmmTMB(AvgSpike~Adults+DeerYear+(1|DeerYear),data=birth96)

nowtbirth96Total_spike<-glmmTMB(AvgSpike~Total+DeerYear+(1|DeerYear),data=birth96)

nowtbirth96LU_Total_spike<-glmmTMB(AvgSpike~LU_Total+DeerYear+(1|DeerYear),data=birth96)

summary(nowtbirth96Hinds_spike)
summary(nowtbirth96Adults_spike)
summary(nowtbirth96Total_spike)
summary(nowtbirth96LU_Total_spike)

#w/o additional year and weight----
noyr_nowtHinds_spike<-glmmTMB(AvgSpike~Hinds+(1|DeerYear),data=nowt)

noyr_nowtAdults_spike<-glmmTMB(AvgSpike~Adults+(1|DeerYear),data=nowt)

noyr_nowtTotal_spike<-glmmTMB(AvgSpike~Total+(1|DeerYear),data=nowt)

noyr_nowtLU_Total_spike<-glmmTMB(AvgSpike~LU_Total+(1|DeerYear),data=nowt)

summary(noyr_nowtHinds_spike)
summary(noyr_nowtAdults_spike)
summary(noyr_nowtTotal_spike)
summary(noyr_nowtLU_Total_spike)

#w/o additional year and weight to 1996----
noyr_nowt96Hinds_spike<-glmmTMB(AvgSpike~Hinds+(1|DeerYear),data=nowt96)

noyr_nowt96Adults_spike<-glmmTMB(AvgSpike~Adults +(1|DeerYear),data=nowt96)

noyr_nowt96Total_spike<-glmmTMB(AvgSpike~Total+(1|DeerYear),data=nowt96)

noyr_nowt96LU_Total_spike<-glmmTMB(AvgSpike~LU_Total+(1|DeerYear),data=nowt96)

summary(noyr_nowt96Hinds_spike)
summary(noyr_nowt96Adults_spike)
summary(noyr_nowt96Total_spike)
summary(noyr_nowt96LU_Total_spike)

#Birth year w/o additional year and weight----
noyr_birthHinds_spike<-glmmTMB(AvgSpike~Hinds+(1|DeerYear),data=birth)

noyr_birthAdults_spike<-glmmTMB(AvgSpike~Adults+(1|DeerYear),data=birth)

noyr_birthTotal_spike<-glmmTMB(AvgSpike~Total+(1|DeerYear),data=birth)

noyr_birthLU_Total_spike<-glmmTMB(AvgSpike~LU_Total+(1|DeerYear),data=birth)

summary(noyr_birthHinds_spike)
summary(noyr_birthAdults_spike)
summary(noyr_birthTotal_spike)
summary(noyr_birthLU_Total_spike)

#Birth year w/o additional year and weight to 1996----
noyr_birth96Hinds_spike<-glmmTMB(AvgSpike~Hinds
                            +(1|DeerYear),data=birth96)

noyr_birth96Adults_spike<-glmmTMB(AvgSpike~Adults
                             +(1|DeerYear),data=birth96)

noyr_birth96Total_spike<-glmmTMB(AvgSpike~Total
                            +(1|DeerYear),data=birth96)

noyr_birth96LU_Total_spike<-glmmTMB(AvgSpike~LU_Total
                               +(1|DeerYear),data=birth96)

summary(noyr_birth96Hinds_spike)
summary(noyr_birth96Adults_spike)
summary(noyr_birth96Total_spike)
summary(noyr_birth96LU_Total_spike)


#FWS----
#without weight----
FWS_nowtHinds_spike<-glmmTMB(AvgSpike~Hinds+DeerYear+rate+(1|DeerYear),data=nowt)

FWS_nowtAdults_spike<-glmmTMB(AvgSpike~Adults+DeerYear+rate+(1|DeerYear),data=nowt)

FWS_nowtTotal_spike<-glmmTMB(AvgSpike~Total+DeerYear+rate+(1|DeerYear),data=nowt)

FWS_nowtLU_Total_spike<-glmmTMB(AvgSpike~LU_Total+DeerYear+rate+(1|DeerYear),data=nowt)

summary(FWS_nowtHinds_spike)
summary(FWS_nowtAdults_spike)
summary(FWS_nowtTotal_spike)
summary(FWS_nowtLU_Total_spike)

#without weight to 1996----
FWS_nowt96Hinds_spike<-glmmTMB(AvgSpike~Hinds+DeerYear+rate+(1|DeerYear),data=nowt96)

FWS_nowt96Adults_spike<-glmmTMB(AvgSpike~Adults+DeerYear+rate+(1|DeerYear),data=nowt96)

FWS_nowt96Total_spike<-glmmTMB(AvgSpike~Total+DeerYear+rate+(1|DeerYear),data=nowt96)

FWS_nowt96LU_Total_spike<-glmmTMB(AvgSpike~LU_Total+DeerYear+rate+(1|DeerYear),data=nowt96)

summary(FWS_nowt96Hinds_spike)
summary(FWS_nowt96Adults_spike)
summary(FWS_nowt96Total_spike)
summary(FWS_nowt96LU_Total_spike)

FWS_birthHinds_spike<-glmmTMB(AvgSpike~Hinds
                          +DeerYear
                          +rate
                          +(1|DeerYear),data=birth)

FWS_birthAdults_spike<-glmmTMB(AvgSpike~Adults
                           +DeerYear
                           +rate
                           +(1|DeerYear),data=birth)

FWS_birthTotal_spike<-glmmTMB(AvgSpike~Total
                          +DeerYear
                          +rate
                          +(1|DeerYear),data=birth)

FWS_birthLU_Total_spike<-glmmTMB(AvgSpike~LU_Total
                             +DeerYear
                             +rate
                             +(1|DeerYear),data=birth)

summary(FWS_birthHinds_spike)
summary(FWS_birthAdults_spike)
summary(FWS_birthTotal_spike)
summary(FWS_birthLU_Total_spike)

FWS_birth96Hinds_spike<-glmmTMB(AvgSpike~Hinds
                            +DeerYear
                            +rate
                            +(1|DeerYear),data=birth96)

FWS_birth96Adults_spike<-glmmTMB(AvgSpike~Adults
                             +DeerYear
                             +rate
                             +(1|DeerYear),data=birth96)

FWS_birth96Total_spike<-glmmTMB(AvgSpike~Total
                            +DeerYear
                            +rate
                            +(1|DeerYear),data=birth96)

FWS_birth96LU_Total_spike<-glmmTMB(AvgSpike~LU_Total
                               +DeerYear
                               +rate
                               +(1|DeerYear),data=birth96)

summary(FWS_birth96Hinds_spike)
summary(FWS_birth96Adults_spike)
summary(FWS_birth96Total_spike)
summary(FWS_birth96LU_Total_spike)

FWS_noyr_nowtHinds_spike<-glmmTMB(AvgSpike~Hinds
                                  +rate
                              +(1|DeerYear),data=nowt)

FWS_noyr_nowtAdults_spike<-glmmTMB(AvgSpike~Adults
                                   +rate
                               +(1|DeerYear),data=nowt)

FWS_noyr_nowtTotal_spike<-glmmTMB(AvgSpike~Total
                                  +rate
                              +(1|DeerYear),data=nowt)

FWS_noyr_nowtLU_Total_spike<-glmmTMB(AvgSpike~LU_Total
                                     +rate
                                 +(1|DeerYear),data=nowt)

summary(FWS_noyr_nowtHinds_spike)
summary(FWS_noyr_nowtAdults_spike)
summary(FWS_noyr_nowtTotal_spike)
summary(FWS_noyr_nowtLU_Total_spike)

FWS_noyr_nowt96Hinds_spike<-glmmTMB(AvgSpike~Hinds
                                    +rate
                                +(1|DeerYear),data=nowt96)

FWS_noyr_nowt96Adults_spike<-glmmTMB(AvgSpike~Adults
                                     +rate
                                 +(1|DeerYear),data=nowt96)

FWS_noyr_nowt96Total_spike<-glmmTMB(AvgSpike~Total
                                    +rate
                                +(1|DeerYear),data=nowt96)

FWS_noyr_nowt96LU_Total_spike<-glmmTMB(AvgSpike~LU_Total
                                       +rate
                                   +(1|DeerYear),data=nowt96)

summary(FWS_noyr_nowt96Hinds_spike)
summary(FWS_noyr_nowt96Adults_spike)
summary(FWS_noyr_nowt96Total_spike)
summary(FWS_noyr_nowt96LU_Total_spike)

FWS_noyr_birthHinds_spike<-glmmTMB(AvgSpike~Hinds
                                   +rate
                               +(1|DeerYear),data=birth)

FWS_noyr_birthAdults_spike<-glmmTMB(AvgSpike~Adults
                                    +rate
                                +(1|DeerYear),data=birth)

FWS_noyr_birthTotal_spike<-glmmTMB(AvgSpike~Total
                                   +rate
                               +(1|DeerYear),data=birth)

FWS_noyr_birthLU_Total_spike<-glmmTMB(AvgSpike~LU_Total
                                      +rate
                                  +(1|DeerYear),data=birth)

summary(FWS_noyr_birthHinds_spike)
summary(FWS_noyr_birthAdults_spike)
summary(FWS_noyr_birthTotal_spike)
summary(FWS_noyr_birthLU_Total_spike)

FWS_noyr_birth96Hinds_spike<-glmmTMB(AvgSpike~Hinds
                                     +rate
                                 +(1|DeerYear),data=birth96)

FWS_noyr_birth96Adults_spike<-glmmTMB(AvgSpike~Adults
                                      +rate
                                  +(1|DeerYear),data=birth96)

FWS_noyr_birth96Total_spike<-glmmTMB(AvgSpike~Total
                                     +rate
                                 +(1|DeerYear),data=birth96)

FWS_noyr_birth96LU_Total_spike<-glmmTMB(AvgSpike~LU_Total
                                        +rate
                                    +(1|DeerYear),data=birth96)

summary(FWS_noyr_birth96Hinds_spike)
summary(FWS_noyr_birth96Adults_spike)
summary(FWS_noyr_birth96Total_spike)
summary(FWS_noyr_birth96LU_Total_spike)

