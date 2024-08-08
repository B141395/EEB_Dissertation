library(tidyverse)
library(lme4)
library(ggplot2)
library(lmerTest)
library(glmmTMB)
library(ggeffects)


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


ggplot(PopD, aes(x = DeerYear)) +
  geom_line(aes(y = Hinds, color = "Hinds"), size = 1) +
  geom_line(aes(y = Adults, color = "Adults"), size = 1) + 
  geom_line(aes(y = Total, color = "Total"), size = 1) +
  geom_line(aes(y = LU_Total, color = "LU_Total"), size = 1) +
  geom_hline(yintercept = c(100, 200, 300, 400), color = 'darkgrey', linetype = "dashed") + 
  labs(x = "Year",
       y = "Deer Density") +
  scale_color_manual(name = "Density Metrics", values = c("Hinds" = "red", "Adults" = "black",
                                "Total" = "#ff7f0e", 
                                "LU_Total" = "#2ca02c"),
                     breaks = c("Hinds", "Stags", "Adults", "Calves","Total", "LU_Total"),
                     labels = c("Hinds", "Stags", "Adults", "Calves", "Total", "Livestock Units"))+
  
  theme(
    axis.line = element_line(color = "black", size = 0.5),
    panel.background = element_blank(),  # Remove background gridlines
    legend.position = "bottom"
  )

ggplot(PopD, aes(x = DeerYear)) +
  geom_line(aes(y = Hinds, color = "Hinds"), size = 1) +
  geom_line(aes(y = Stags, color = "Stags"), size = 1) +
  geom_line(aes(y = Calves, color = "Calves"), size = 1) +
  geom_hline(yintercept = c(50,100,150, 200), color = 'darkgrey', linetype = "dashed") + 
  labs(x = "Year",
       y = "Deer Density") +
  scale_color_manual(name = "Density Metrics",
                     values = c("Hinds" = "red","Stags" = "blue","Calves"="brown"),
                     breaks = c("Hinds", "Stags","Calves"),
                     labels = c("Hinds", "Stags","Calves"))+
  
  theme(
    axis.line = element_line(color = "black", size = 0.5),
    panel.background = element_blank(),  # Remove background gridlines
    legend.position = "bottom"
  )





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
  scale_x_continuous(breaks = seq(1973, 2022, by = 1))+
  labs( x = "Year", y = "Yearling Mean Antler Length (inches)") +
  theme_minimal() +
  theme( axis.title.x = element_text(size = 15),  
         axis.title.y = element_text(size = 15),
         axis.text.x = element_text(size = 12,angle = 90, hjust = 0),
         axis.text.y = element_text(size = 12),
         axis.line = element_line(color = "black", size = 0.5),
         panel.background = element_blank(),  # Remove background gridlines
         legend.position = "bottom") +
  geom_text(aes(y = mean_spike + se_spike + 0.5, label = paste0("(", sample_size, ")")), size = 3, color = "black") 


ggplot(data = AvgSpike96, aes(x = DeerYear, y = mean_spike)) +
  geom_line(color = "black", size = 0.75) +
  geom_point(color = "black", size = 2,shape = 15) +
  geom_errorbar(aes(ymin = mean_spike - se_spike, ymax = mean_spike + se_spike), width = 0.2, color = "black") +
  labs(title = "Mean cohort yearling anter length for cohorts born 1973-1996", x = "Year of Birth", y = "Yearling Mean Antler Length (inches)") +
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



Spike_noden<-glmmTMB(AvgSpike~BirthWt
                            +DeerYear
                            +(1|DeerYear)
                            +(1|MumCode),data=Spike)

summary(Spike_noden)

tab_model(Spike_noden,digits = 4, show.ci = FALSE, show.se = TRUE)
plot_model(Spike_noden)


Spike_noyr_noden<-glmmTMB(AvgSpike~BirthWt
                     +(1|DeerYear)
                     +(1|MumCode),data=Spike)

summary(Spike_noyr_noden)

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

predictions_Hinds_yr <- ggpredict(Hinds_spike, terms = "Hinds [all]")

predictions_Hinds_yr$Hinds <- predictions_Hinds_yr$x

predictions_Hinds_noyr <- ggpredict(Hinds_spike_simple, terms = "Hinds [all]")
predictions_Hinds_noyr$Hinds <- predictions_Hinds_noyr$x


Spike_pred_Hinds_yr <- Spike_pred %>% left_join(predictions_Hinds_yr, by = "Hinds")

Spike_pred_Hinds_noyr <- Spike_pred %>% left_join(predictions_Hinds_noyr, by = "Hinds")

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
Spike_pred_Hinds_yr$Type <- "With Year as Fixed effect"
Spike_pred_Hinds_noyr$Type <- "Without Year as Fixed effect"

# Combine the data frames
combined_spike_Hinds <- rbind(
  Spike_pred_Hinds_yr,
  Spike_pred_Hinds_noyr
)

# Keep only unique points for plotting
unique_spike_Hinds <- combined_spike_Hinds %>%
  distinct(Hinds, mean_spike, .keep_all = TRUE)


# Plot the combined data
ggplot(combined_spike_Hinds, aes(x = Hinds, y = mean_spike)) +
  geom_point(data = unique_spike_Hinds) +
  geom_text(data = unique_spike_Hinds, aes(y = mean_spike + 0.1, label = paste0("(", DeerYear, ")")), size = 2.5, color = "black") +
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

#adults----
predictions_Adults_yr <- ggpredict(Adults_spike, terms = "Adults [all]")

predictions_Adults_yr$Adults <- predictions_Adults_yr$x

predictions_Adults_noyr <- ggpredict(Adults_spike_simple, terms = "Adults [all]")
predictions_Adults_noyr$Adults <- predictions_Adults_noyr$x


Spike_pred_Adults_yr <- Spike_pred %>% left_join(predictions_Adults_yr, by = "Adults")

Spike_pred_Adults_noyr <- Spike_pred %>% left_join(predictions_Adults_noyr, by = "Adults")

# Add a Type column to differentiate the data sets
Spike_pred_Adults_yr$Type <- "With Year as Fixed effect"
Spike_pred_Adults_noyr$Type <- "Without Year as Fixed effect"

# Combine the data frames
combined_spike_Adults <- rbind(
  Spike_pred_Adults_yr,
  Spike_pred_Adults_noyr
)

# Keep only unique points for plotting
unique_spike_Adults <- combined_spike_Adults %>%
  distinct(Adults, mean_spike, .keep_all = TRUE)


Adults_Spike_terms <- paste(
  "Adjusted for:",
  "Birth Weight = 6.99 kg",
  "Deer Year = 2001",
  sep = "\n"
)

# Plot the combined data
ggplot(combined_spike_Adults, aes(x = Adults, y = mean_spike)) +
  geom_point(data = unique_spike_Adults) +
  geom_text(data = unique_spike_Adults, aes(y = mean_spike + 0.1, label = paste0("(", DeerYear, ")")), size = 2.5, color = "black") +
  geom_line(aes(y = predicted, color = Type)) +  # Predicted values with different colors
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = Type), alpha = 0.2) +  # Confidence intervals with different fills
  labs(
    x = "Adults Population Size",
    y = "Mean Yearling Spike Length (inches)"  ) +
  scale_color_manual(values = c("With Year as Fixed effect" = "blue", "Without Year as Fixed effect" = "red")) +  # Customize line colors
  scale_fill_manual(values = c("With Year as Fixed effect" = "lightblue", "Without Year as Fixed effect" = "lightpink")) +  # Customize ribbon fills
  theme_minimal() +
  theme( axis.title.x = element_text(size = 15),  
         axis.title.y = element_text(size = 15),
         axis.text.x = element_text(size = 12),
         axis.text.y = element_text(size = 12),
         legend.text = element_text(size = 10),
         axis.line = element_line(color = "black", size = 0.5),
         panel.background = element_blank(),  # Remove background gridlines
         legend.position = "bottom",  # Position the legend at the bottom
         legend.title = element_blank())+ # Optionally remove the legend title
  annotate("text", x = 185, y = 1.1, label = Adults_Spike_terms, size = 4, color = "#8A7D23", hjust = 0, vjust = 1) 




#total----
predictions_Total_yr <- ggpredict(Total_spike, terms = "Total [all]")

predictions_Total_yr$Total <- predictions_Total_yr$x

predictions_Total_noyr <- ggpredict(Total_spike_simple, terms = "Total [all]")
predictions_Total_noyr$Total <- predictions_Total_noyr$x


Spike_pred_Total_yr <- Spike_pred %>% left_join(predictions_Total_yr, by = "Total")

Spike_pred_Total_noyr <- Spike_pred %>% left_join(predictions_Total_noyr, by = "Total")

# Add a Type column to differentiate the data sets
Spike_pred_Total_yr$Type <- "With Year as Fixed effect"
Spike_pred_Total_noyr$Type <- "Without Year as Fixed effect"

# Combine the data frames
combined_spike_Total <- rbind(
  Spike_pred_Total_yr,
  Spike_pred_Total_noyr
)

# Keep only unique points for plotting
unique_spike_Total <- combined_spike_Total %>%
  distinct(Total, mean_spike, .keep_all = TRUE)


# Plot the combined data
ggplot(combined_spike_Total, aes(x = Total, y = mean_spike)) +
  geom_point(data = unique_spike_Total) +
  geom_text(data = unique_spike_Total, aes(y = mean_spike + 0.1, label = paste0("(", DeerYear, ")")), size = 2.5, color = "black") +
  geom_line(aes(y = predicted, color = Type)) +  # Predicted values with different colors
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = Type), alpha = 0.2) +  # Confidence intervals with different fills
  labs(
    x = "Total Density",
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


#LU_total----
predictions_LU_Total_yr <- ggpredict(LU_Total_spike, terms = "LU_Total [all]")

predictions_LU_Total_yr$LU_Total <- predictions_LU_Total_yr$x

predictions_LU_Total_noyr <- ggpredict(LU_Total_spike_simple, terms = "LU_Total [all]")
predictions_LU_Total_noyr$LU_Total <- predictions_LU_Total_noyr$x


Spike_pred_LU_Total_yr <- Spike_pred %>% left_join(predictions_LU_Total_yr, by = "LU_Total")

Spike_pred_LU_Total_noyr <- Spike_pred %>% left_join(predictions_LU_Total_noyr, by = "LU_Total")

# Add a Type column to differentiate the data sets
Spike_pred_LU_Total_yr$Type <- "With Year as Fixed effect"
Spike_pred_LU_Total_noyr$Type <- "Without Year as Fixed effect"

# Combine the data frames
combined_spike_LU_Total <- rbind(
  Spike_pred_LU_Total_yr,
  Spike_pred_LU_Total_noyr
)

# Keep only unique points for plotting
unique_spike_LU_Total <- combined_spike_LU_Total %>%
  distinct(LU_Total, mean_spike, .keep_all = TRUE)


# Plot the combined data
ggplot(combined_spike_LU_Total, aes(x = LU_Total, y = mean_spike)) +
  geom_point(data = unique_spike_LU_Total) +
  geom_text(data = unique_spike_LU_Total, aes(y = mean_spike + 0.1, label = paste0("(", DeerYear, ")")), size = 2.5, color = "black") +
  geom_line(aes(y = predicted, color = Type)) +  # Predicted values with different colors
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = Type), alpha = 0.2) +  # Confidence intervals with different fills
  labs(
    x = "LU_Total Density",
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


print(predictions_Hinds_noyr)
print(predictions_Hinds_yr)
print(predictions_Adults_noyr)
print(predictions_Adults_yr)
print(predictions_Total_noyr)
print(predictions_Total_yr)
print(predictions_LU_Total_noyr)
print(predictions_LU_Total_yr)


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

Spike_pred_Adults_yr$Type <- "With Year as Fixed effect"
Spike_pred_Adults_noyr$Type <- "Without Year as Fixed effect"

# Combine the data frames
combined_spike_Adults <- rbind(
  Spike_pred_Adults_yr,
  Spike_pred_Adults_noyr
)

# Keep only unique points for plotting
unique_spike_Adults <- combined_spike_Adults %>%
  distinct(Adults, mean_spike, .keep_all = TRUE)


Adults_Spike_terms <- paste(
  "Adjusted for:",
  "Birth Weight = 6.99 kg",
  "Deer Year = 2001",
  sep = "\n"
)

# Plot the combined data
ggplot(combined_spike_Adults, aes(x = Adults, y = mean_spike)) +
  geom_point(data = unique_spike_Adults) +
  geom_text(data = unique_spike_Adults, aes(y = mean_spike + 0.1, label = paste0("(", DeerYear, ")")), size = 2.5, color = "black") +
  geom_line(aes(y = predicted, color = Type)) +  # Predicted values with different colors
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = Type), alpha = 0.2) +  # Confidence intervals with different fills
  labs(
    x = "Adults Population Size",
    y = "Mean Yearling Spike Length (inches)"  ) +
  scale_color_manual(values = c("With Year as Fixed effect" = "blue", "Without Year as Fixed effect" = "red")) +  # Customize line colors
  scale_fill_manual(values = c("With Year as Fixed effect" = "lightblue", "Without Year as Fixed effect" = "lightpink")) +  # Customize ribbon fills
  theme_minimal() +
  theme( axis.title.x = element_text(size = 15),  
         axis.title.y = element_text(size = 15),
         axis.text.x = element_text(size = 12),
         axis.text.y = element_text(size = 12),
         legend.text = element_text(size = 10),
         axis.line = element_line(color = "black", size = 0.5),
         panel.background = element_blank(),  # Remove background gridlines
         legend.position = "bottom",  # Position the legend at the bottom
         legend.title = element_blank())+ # Optionally remove the legend title
  annotate("text", x = 185, y = 1.1, label = Adults_Spike_terms, size = 4, color = "#8A7D23", hjust = 0, vjust = 1) 


#DeerYear as fixed effect
#with and without year data

predictions_spike_noden <- ggpredict(Spike_noden, terms = "DeerYear [all]")
predictions_spike_noden$DeerYear <- predictions_spike_noden$x

predictions_spike_noyr_noden <- ggpredict(Spike_noyr_noden, terms = "DeerYear [all]")
predictions_spike_noyr_noden$DeerYear <- predictions_spike_noyr_noden$x

# Add a Type column to differentiate the data sets
predictions_spike_noden$Type <- "With Year as Fixed effect"
predictions_spike_noyr_noden$Type <- "Without Year as Fixed effect"


predictions_spike_noden <- Spike_pred %>% left_join(predictions_spike_noden, by = "DeerYear")

predictions_spike_noyr_noden <- Spike_pred %>% left_join(predictions_spike_noyr_noden, by = "DeerYear")



# Combine the data frames
combined_spike_noden <- rbind(
  predictions_spike_noden,
  predictions_spike_noyr_noden
)

# Keep only unique points for plotting
unique_spike_noden <- combined_spike_noden %>%
  distinct(DeerYear, mean_spike, .keep_all = TRUE)

print(predictions_spike_noden)

noden_Spike_terms <- paste(
  "Adjusted for:",
  "Birth Weight = 6.99 kg",
  sep = "\n"
)



plot_model(Spike_noden, type = "pred")

# Plot the combined data
ggplot(combined_spike_noden, aes(x = DeerYear, y = mean_spike)) +
  geom_point(data = unique_spike_noden) +
  geom_line(aes(y = predicted, color = Type)) +  # Predicted values with different colors
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = Type), alpha = 0.2) +  # Confidence intervals with different fills
  labs(
    x = "Deer Year",
    y = "Mean Yearling Spike Length (inches)"  ) +
  scale_color_manual(values = c("With Year as Fixed effect" = "blue", "Without Year as Fixed effect" = "red")) +  # Customize line colors
  scale_fill_manual(values = c("With Year as Fixed effect" = "lightblue", "Without Year as Fixed effect" = "lightpink")) +  # Customize ribbon fills
  theme_minimal() +
  theme( axis.title.x = element_text(size = 15),  
         axis.title.y = element_text(size = 15),
         axis.text.x = element_text(size = 12),
         axis.text.y = element_text(size = 12),
         legend.text = element_text(size = 10),
         axis.line = element_line(color = "black", size = 0.5),
         panel.background = element_blank(),  # Remove background gridlines
         legend.position = "bottom",  # Position the legend at the bottom
         legend.title = element_blank())+ # Optionally remove the legend title
  annotate("text", x = 1975, y = 1.1, label = noden_Spike_terms, size = 4, color = "#8A7D23", hjust = 0, vjust = 1) 

