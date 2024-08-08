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

