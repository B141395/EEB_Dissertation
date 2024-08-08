library(tidyverse)
library(lme4)
library(ggplot2)
library(lmerTest)
library(glmmTMB)
library(ggeffects)
library (sjPlot)

ind_bw<-birthwt[,"Code", drop = FALSE]
ind_fws<-FWS[,"Code", drop = FALSE]
ind_spike<-Spike_pred[,"Code", drop = FALSE]
ind_fecundity<-unique_rut[,"Female", drop = FALSE]
names(ind_fecundity) <- "Code"

all_individuals <- rbind(ind_bw, ind_fws, ind_spike, ind_fecundity)


all_individuals <- unique(all_individuals)



birthwt<-read.csv("birth_wt_mum_age.csv")

Density<-read.csv("Population_estimate_calculations.csv")
PopD<-Density %>% select (DeerYear,Hinds,Stags,Calves,Calves_M,Calves_F,Adults,Total,LU_Total)

birthwt$MumAgeSquared<-(birthwt$MumAge)*(birthwt$MumAge)
birthwt$DeerYear<-birthwt$BirthYear
birthwt <- birthwt %>%
  left_join(PopD, by = "DeerYear")

Hinds_bw_simple<-glmmTMB(BirthWt~Hinds
                  +Sex
                  +DeerYear
                  +(1|DeerYear)
                  +(1|MumCode),
                  family = gaussian(),data=birthwt)

summary(Hinds_bw_simple)

Hinds_bw_noyr<-glmmTMB(BirthWt~Hinds
                         +Sex
                         +(1|DeerYear)
                         +(1|MumCode),
                         family = gaussian(),data=birthwt)
summary(Hinds_bw_noyr)


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

Mum_noden_bw<-glmmTMB(BirthWt~Sex
                      +DaysFrom1May
                      +DeerYear
                      +MumAge
                      +MumAgeSquared
                      +MotherStatus
                      +(1|DeerYear)
                      +(1|MumCode),
                      family = gaussian(),data=birthwt)
summary(Mum_noden_bw)

Mum_noyr_noden_bw<-glmmTMB(BirthWt~Sex
                      +DaysFrom1May
                      +MumAge
                      +MumAgeSquared
                      +MotherStatus
                      +(1|DeerYear)
                      +(1|MumCode),
                      family = gaussian(),data=birthwt)
summary(Mum_noyr_noden_bw)


noyr_Mum_Hinds_bw<-glmmTMB(BirthWt~Hinds
                      +Sex
                      +DaysFrom1May
                      +MumAge
                      +MumAgeSquared
                      +MotherStatus
                      +(1|DeerYear)
                      +(1|MumCode),
                      family = gaussian(),data=birthwt)

noyr_Mum_Adults_bw<-glmmTMB(BirthWt~Adults
                       +Sex
                       +DaysFrom1May
                       +MumAge
                       +MumAgeSquared
                       +MotherStatus
                       +(1|DeerYear)
                       +(1|MumCode),
                       family = gaussian(),data=birthwt)

noyr_Mum_Total_bw<-glmmTMB(BirthWt~Total
                      +Sex
                      +DaysFrom1May
                      +MumAge
                      +MumAgeSquared
                      +MotherStatus
                      +(1|DeerYear)
                      +(1|MumCode),
                      family = gaussian(),data=birthwt)

noyr_Mum_LU_Total_bw<-glmmTMB(BirthWt~LU_Total
                         +Sex
                         +DaysFrom1May
                         +MumAge
                         +MumAgeSquared
                         +MotherStatus
                         +(1|DeerYear)
                         +(1|MumCode),
                         family = gaussian(),data=birthwt)

summary(noyr_Mum_Hinds_bw)
summary(noyr_Mum_Adults_bw)
summary(noyr_Mum_Total_bw)
summary(noyr_Mum_LU_Total_bw)


reduced_bw_noyr<-glmmTMB(BirthWt~Sex
                    +DaysFrom1May
                    +MumAge
                    +MumAgeSquared
                    +MotherStatus
                    +(1|MumCode),
                    family = gaussian(),data=birthwt)

reduced_bw_nomumid<-glmmTMB(BirthWt~Sex
                            +DaysFrom1May
                            +MumAge
                            +MumAgeSquared
                            +MotherStatus
                            +(1|DeerYear),
                            family = gaussian(),data=birthwt)

anova(Mum_noyr_noden_bw,reduced_bw_noyr)
anova(Mum_noyr_noden_bw,reduced_bw_nomumid)

anova(Mum_noden_bw,Mum_noyr_noden_bw)
anova(Mum_noyr_noden_bw,Mum_Hinds_bw)
anova(Mum_noyr_noden_bw,Mum_Adults_bw)
anova(Mum_noyr_noden_bw,Mum_Total_bw)
anova(Mum_noyr_noden_bw,Mum_LU_Total_bw)

plot_model(Mum_noyr_noden_bw,type = "pred", terms = c("DeerYear [all]", "MotherStatus"))

plot_model(Mum_noyr_noden_bw,type = "pred", terms = "MotherStatus")

plot_model(Mum_noyr_noden_bw)


tab_model(Mum_noyr_noden_bw,digits = 4, show.ci = FALSE, show.se = TRUE)
plot_model(Mum_noyr_noden_bw)


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


birthwt <-na.omit(birthwt)

AvgBw <- birthwt %>%
  group_by(DeerYear) %>%
  summarise(
    mean_Bw = mean(BirthWt),
    sample_size = n(),
    se_Bw = sd(BirthWt) / sqrt(n())
  )

AvgBw73 <-AvgBw %>% filter(!DeerYear %in% c(1968,1969,1970,1971,1972))



ggplot(data = AvgBw73, aes(x = DeerYear, y = mean_Bw)) +
  geom_line(color = "black", size = 0.75) +
  geom_point(color = "black", size = 2,shape = 15) +  
  geom_errorbar(aes(ymin = mean_Bw - se_Bw, ymax = mean_Bw + se_Bw), width = 0.2, color = "black") +
  scale_x_continuous(breaks = seq(1973, 2022, by = 1))+
  labs( x = "Year of Birth", y = "Mean Birth Weight (kg)") +
  theme_minimal() +
  theme( axis.title.x = element_text(size = 15),  
         axis.title.y = element_text(size = 15),
         axis.text.x = element_text(size = 12,angle = 90, hjust = 0),
         axis.text.y = element_text(size = 12),
         axis.line = element_line(color = "black", size = 0.5),
         panel.background = element_blank(),  # Remove background gridlines
         legend.position = "bottom") +
  geom_text(aes(y = mean_Bw + se_Bw + 0.15, label = paste0("(", sample_size, ")")), size = 3, color = "black") 




#Hinds----
AvgBw_filtered <- AvgBw %>% select(DeerYear, mean_Bw,sample_size,se_Bw)

Bw_pred <- birthwt %>% left_join(AvgBw_filtered, by = "DeerYear")

predictions_Hinds_Bw_yr <- ggpredict(Mum_Hinds_bw, terms = "Hinds [all]")

predictions_Hinds_Bw_yr$Hinds <- predictions_Hinds_Bw_yr$x

predictions_Hinds_Bw_noyr <- ggpredict(noyr_Mum_Hinds_bw, terms = "Hinds [all]")
predictions_Hinds_Bw_noyr$Hinds <- predictions_Hinds_Bw_noyr$x


Bw_pred_Hinds_yr <- Bw_pred %>% left_join(predictions_Hinds_Bw_yr, by = "Hinds")

Bw_pred_Hinds_noyr <- Bw_pred %>% left_join(predictions_Hinds_Bw_noyr, by = "Hinds")

# Add a Type column to differentiate the data sets
Bw_pred_Hinds_yr$Type <- "With Year as Fixed effect"
Bw_pred_Hinds_noyr$Type <- "Without Year as Fixed effect"

# Combine the data frames
combined_Bw_Hinds <- rbind(
  Bw_pred_Hinds_yr,
  Bw_pred_Hinds_noyr
)

# Keep only unique points for plotting
unique_Bw_Hinds <- combined_Bw_Hinds %>%
  distinct(Hinds, mean_Bw, .keep_all = TRUE)


# Plot the combined data
ggplot(combined_Bw_Hinds, aes(x = Hinds, y = mean_Bw)) +
  geom_point(shape=1) +
  geom_point(data = unique_Bw_Hinds) +
  geom_text(data = unique_Bw_Hinds, aes(y = mean_Bw + 0.1, label = paste0("(", DeerYear, ")")), size = 2.5, color = "black") +
  geom_line(aes(y = predicted, color = Type)) +  # Predicted values with different colors
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = Type), alpha = 0.2) +  # Confidence intervals with different fills
  labs(
    x = "Hind Density",
    y = "Mean Birth Weight (kg)",
    title = "Predicted Mean Birth Weight over Density\nwith 95% Confidence Interval"
  ) +
  scale_color_manual(values = c("With Year as Fixed effect" = "blue", "Without Year as Fixed effect" = "red")) +  # Customize line colors
  scale_fill_manual(values = c("With Year as Fixed effect" = "lightblue", "Without Year as Fixed effect" = "lightpink")) +  # Customize ribbon fills
  theme_minimal() +
  theme(
    legend.position = "bottom",  # Position the legend at the bottom
    legend.title = element_blank()  # Optionally remove the legend title
  )


#Adults----

predictions_Adults_Bw_yr <- ggpredict(Mum_Adults_bw, terms = "Adults [all]")

predictions_Adults_Bw_yr$Adults <- predictions_Adults_Bw_yr$x

predictions_Adults_Bw_noyr <- ggpredict(noyr_Mum_Adults_bw, terms = "Adults [all]")
predictions_Adults_Bw_noyr$Adults <- predictions_Adults_Bw_noyr$x


Bw_pred_Adults_yr <- Bw_pred %>% left_join(predictions_Adults_Bw_yr, by = "Adults")

Bw_pred_Adults_noyr <- Bw_pred %>% left_join(predictions_Adults_Bw_noyr, by = "Adults")

# Add a Type column to differentiate the data sets
Bw_pred_Adults_yr$Type <- "With Year as Fixed effect"
Bw_pred_Adults_noyr$Type <- "Without Year as Fixed effect"

# Combine the data frames
combined_Bw_Adults <- rbind(
  Bw_pred_Adults_yr,
  Bw_pred_Adults_noyr
)

# Keep only unique points for plotting
unique_Bw_Adults <- combined_Bw_Adults %>%
  distinct(Adults, mean_Bw, .keep_all = TRUE)

Adults_Bw_terms <- paste(
  "Adjusted for:",
  "Sex = Male",
  "Mother's Reproductive Status = True yeld",
  "Mother's Age = 8",
  "Mother's Age Squared = 64",
  "Days From 1st May = 35",
  "Deer Year = 2001",
  sep = "\n"
)

# Plot the combined data
ggplot(combined_Bw_Adults, aes(x = Adults, y = mean_Bw)) +
  geom_point(shape=1) +
  geom_point(data = unique_Bw_Adults) +
  geom_text(data = unique_Bw_Adults, aes(y = mean_Bw + 0.05, label = paste0("(", DeerYear, ")")), size = 2.5, color = "black") +
  geom_line(aes(y = predicted, color = Type)) +  # Predicted values with different colors
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = Type), alpha = 0.2) +  # Confidence intervals with different fills
  labs(
    x = "Adult Population Size",
    y = "Mean Birth Weight (kg)",
  ) +
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
  annotate("text", x = 185, y = 6.31, label = Adults_Bw_terms, size = 4, color = "#8A7D23", hjust = 0, vjust = 1) 




#Total----

predictions_Total_Bw_yr <- ggpredict(Mum_Total_bw, terms = "Total [all]")

predictions_Total_Bw_yr$Total <- predictions_Total_Bw_yr$x

predictions_Total_Bw_noyr <- ggpredict(noyr_Mum_Total_bw, terms = "Total [all]")
predictions_Total_Bw_noyr$Total <- predictions_Total_Bw_noyr$x


Bw_pred_Total_yr <- Bw_pred %>% left_join(predictions_Total_Bw_yr, by = "Total")

Bw_pred_Total_noyr <- Bw_pred %>% left_join(predictions_Total_Bw_noyr, by = "Total")

# Add a Type column to differentiate the data sets
Bw_pred_Total_yr$Type <- "With Year as Fixed effect"
Bw_pred_Total_noyr$Type <- "Without Year as Fixed effect"

# Combine the data frames
combined_Bw_Total <- rbind(
  Bw_pred_Total_yr,
  Bw_pred_Total_noyr
)

# Keep only unique points for plotting
unique_Bw_Total <- combined_Bw_Total %>%
  distinct(Total, mean_Bw, .keep_all = TRUE)


# Plot the combined data
ggplot(combined_Bw_Total, aes(x = Total, y = mean_Bw)) +
  geom_point(shape=1) +
  geom_point(data = unique_Bw_Total) +
  geom_text(data = unique_Bw_Total, aes(y = mean_Bw + 0.1, label = paste0("(", DeerYear, ")")), size = 2.5, color = "black") +
  geom_line(aes(y = predicted, color = Type)) +  # Predicted values with different colors
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = Type), alpha = 0.2) +  # Confidence intervals with different fills
  labs(
    x = "Total Density",
    y = "Mean Birth Weight (kg)",
    title = "Predicted Mean Birth Weight over Density\nwith 95% Confidence Interval"
  ) +
  scale_color_manual(values = c("With Year as Fixed effect" = "blue", "Without Year as Fixed effect" = "red")) +  # Customize line colors
  scale_fill_manual(values = c("With Year as Fixed effect" = "lightblue", "Without Year as Fixed effect" = "lightpink")) +  # Customize ribbon fills
  theme_minimal() +
  theme(
    legend.position = "bottom",  # Position the legend at the bottom
    legend.title = element_blank()  # Optionally remove the legend title
  )

#LU_Total----
predictions_LU_Total_Bw_yr <- ggpredict(Mum_LU_Total_bw, terms = "LU_Total [all]")

predictions_LU_Total_Bw_yr$LU_Total <- predictions_LU_Total_Bw_yr$x

predictions_LU_Total_Bw_noyr <- ggpredict(noyr_Mum_LU_Total_bw, terms = "LU_Total [all]")
predictions_LU_Total_Bw_noyr$LU_Total <- predictions_LU_Total_Bw_noyr$x


Bw_pred_LU_Total_yr <- Bw_pred %>% left_join(predictions_LU_Total_Bw_yr, by = "LU_Total")

Bw_pred_LU_Total_noyr <- Bw_pred %>% left_join(predictions_LU_Total_Bw_noyr, by = "LU_Total")

# Add a Type column to differentiate the data sets
Bw_pred_LU_Total_yr$Type <- "With Year as Fixed effect"
Bw_pred_LU_Total_noyr$Type <- "Without Year as Fixed effect"

# Combine the data frames
combined_Bw_LU_Total <- rbind(
  Bw_pred_LU_Total_yr,
  Bw_pred_LU_Total_noyr
)

# Keep only unique points for plotting
unique_Bw_LU_Total <- combined_Bw_LU_Total %>%
  distinct(LU_Total, mean_Bw, .keep_all = TRUE)


# Plot the combined data
ggplot(combined_Bw_LU_Total, aes(x = LU_Total, y = mean_Bw)) +
  geom_point(shape=1) +
  geom_point(data = unique_Bw_LU_Total) +
  geom_text(data = unique_Bw_LU_Total, aes(y = mean_Bw + 0.1, label = paste0("(", DeerYear, ")")), size = 2.5, color = "black") +
  geom_line(aes(y = predicted, color = Type)) +  # Predicted values with different colors
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = Type), alpha = 0.2) +  # Confidence intervals with different fills
  labs(
    x = "LU_Total Density",
    y = "Mean Birth Weight (kg)",
    title = "Predicted Mean Birth Weight over Density\nwith 95% Confidence Interval"
  ) +
  scale_color_manual(values = c("With Year as Fixed effect" = "blue", "Without Year as Fixed effect" = "red")) +  # Customize line colors
  scale_fill_manual(values = c("With Year as Fixed effect" = "lightblue", "Without Year as Fixed effect" = "lightpink")) +  # Customize ribbon fills
  theme_minimal() +
  theme(
    legend.position = "bottom",  # Position the legend at the bottom
    legend.title = element_blank()  # Optionally remove the legend title
  )

print(predictions_Hinds_Bw_noyr)
print(predictions_Hinds_Bw_yr)
print(predictions_Adults_Bw_noyr)
print(predictions_Adults_Bw_yr)
print(predictions_Total_Bw_noyr)
print(predictions_Total_Bw_yr)
print(predictions_LU_Total_Bw_noyr)
print(predictions_LU_Total_Bw_yr)
