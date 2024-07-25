#with and without mum data

predictions_LU_Total_nomum_Bw <- ggpredict(LU_Total_bw, terms = "LU_Total [all]")
predictions_LU_Total_nomum_Bw$LU_Total <- predictions_LU_Total_nomum_Bw$x

predictions_LU_Total_age_Bw <- ggpredict(Age_LU_Total_bw, terms = "LU_Total [all]")
predictions_LU_Total_age_Bw$LU_Total <- predictions_LU_Total_age_Bw$x

predictions_LU_Total_status_Bw <- ggpredict(Status_LU_Total_bw, terms = "LU_Total [all]")
predictions_LU_Total_status_Bw$LU_Total <- predictions_LU_Total_status_Bw$x

Bw_pred_LU_Total_nomum <- Bw_pred %>% left_join(predictions_LU_Total_nomum_Bw, by = "LU_Total")

Bw_pred_LU_Total_age <- Bw_pred %>% left_join(predictions_LU_Total_age_Bw, by = "LU_Total")

Bw_pred_LU_Total_status <- Bw_pred %>% left_join(predictions_LU_Total_status_Bw, by = "LU_Total")


# Add a Type column to differentiate the data sets
Bw_pred_LU_Total_nomum$Type <- "Without Mum age & status"
Bw_pred_LU_Total_age$Type <- "With Mum age & age squared"
Bw_pred_LU_Total_status$Type <- "With Mum reproductive status"

# Combine the data frames
combined_Bw_LU_Total <- rbind(
  Bw_pred_LU_Total_nomum,
  Bw_pred_LU_Total_age,
  Bw_pred_LU_Total_status
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
  scale_color_manual(values = c("Without Mum age & status" = "blue", "With Mum age & age squared" = "red","With Mum reproductive status"="green")) +  # Customize line colors
  scale_fill_manual(values = c("Without Mum age & status" = "lightblue", "With Mum age & age squared" = "lightpink","With Mum reproductive status"="lightgreen")) +  # Customize ribbon fills
  theme_minimal() +
  theme(
    legend.position = "bottom",  # Position the legend at the bottom
    legend.title = element_blank()  # Optionally remove the legend title
  )


#with and without year simple

predictions_Hinds_Bw_simple <- ggpredict(Hinds_bw_simple, terms = "Hinds [all]")

predictions_Hinds_Bw_simple$Hinds <- predictions_Hinds_Bw_simple$x

predictions_Hinds_Bw_simple_noyr <- ggpredict(Hinds_bw_noyr, terms = "Hinds [all]")
predictions_Hinds_Bw_simple_noyr$Hinds <- predictions_Hinds_Bw_simple_noyr$x


Bw_pred_Hinds_simple <- Bw_pred %>% left_join(predictions_Hinds_Bw_simple, by = "Hinds")

Bw_pred_Hinds_simple_noyr <- Bw_pred %>% left_join(predictions_Hinds_Bw_simple_noyr, by = "Hinds")


Bw_pred_Hinds_simple$Type <- "With Year as Fixed effect"
Bw_pred_Hinds_simple_noyr$Type <- "Without Year as Fixed effect"

# Combine the data frames
combined_Bw_Hinds <- rbind(
  Bw_pred_Hinds_simple,
  Bw_pred_Hinds_simple_noyr
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
    x = "Hinds Density",
    y = "Mean Birth Weight (kg)",
    title = "Predicted Mean Birth Weight over Density\n(simple models without mother information)\nwith 95% Confidence Interval"
  ) +
  scale_color_manual(values = c("With Year as Fixed effect" = "blue", "Without Year as Fixed effect" = "red")) +  # Customize line colors
  scale_fill_manual(values = c("With Year as Fixed effect" = "lightblue", "Without Year as Fixed effect" = "lightpink")) +  # Customize ribbon fills
  theme_minimal() +
  theme(
    legend.position = "bottom",  # Position the legend at the bottom
    legend.title = element_blank()  # Optionally remove the legend title
  )
