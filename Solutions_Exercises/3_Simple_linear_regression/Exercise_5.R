library(ggplot2)
library(patchwork)  # For combining plots

# Original Model
mod <- lm(height ~ weight, data = d2)

# Extract 10% and 90% quantiles of weight
x_low_high <- quantile(d2$weight, probs = c(0.1, 0.9))

# Scenario 1: Original Slope
mean1 <- 113.8793936 + 0.9050291 * x_low_high[1]
mean2 <- 113.8793936 + 0.9050291 * x_low_high[2]
sd <- 5.086  # Standard deviation remains the same

# Generate density data
x_seq <- seq(mean1 - 4 * sd, mean2 + 4 * sd, length.out = 1000)
data <- data.frame(
  x = rep(x_seq, 2),
  density = c(dnorm(x_seq, mean = mean1, sd = sd),
              dnorm(x_seq, mean = mean2, sd = sd)),
  group = rep(c(paste0("x_low = ", round(x_low_high[1], 2)),
                paste0("x_high = ", round(x_low_high[2], 2))), each = length(x_seq))
)
dim(data)

# Scenario 2: Increased Slope to 3
set.seed(123)
true_slope <- 0.5  # Increasing the slope for better separation
data_more_slope <- data.frame(
  weight = rnorm(352, mean = 45, sd = 6.45)
)
data_more_slope$height <- 113.87939 + true_slope * data_more_slope$weight + rnorm(352, mean = 0, sd = sd)

# Compute means with the new slope
mean1_slope <- 113.87939 + true_slope * x_low_high[1]
mean2_slope <- 113.87939 + true_slope * x_low_high[2]

# Generate density data for the new scenario
x_seq_slope <- seq(mean1_slope - 4 * sd, mean2_slope + 4 * sd, length.out = 1000)
data_slope <- data.frame(
  x = rep(x_seq_slope, 2),
  density = c(dnorm(x_seq_slope, mean = mean1_slope, sd = sd),
              dnorm(x_seq_slope, mean = mean2_slope, sd = sd)),
  group = rep(c(paste0("x_low = ", round(x_low_high[1], 2)),
                paste0("x_high = ", round(x_low_high[2], 2))), each = length(x_seq_slope))
)

# Plot for Scenario 1 (Original Slope)
plot1 <- ggplot(data, aes(x = x, y = density, color = group)) +
  geom_line(linewidth = 1.2) +
  labs(
    title = "Scenario 1: Original Slope",
    x = "Height",
    y = "Density",
    color = "Weight"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("blue", "red")) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    legend.position = "bottom"
  )

# Plot for Scenario 2 (Increased Slope)
plot2 <- ggplot(data_slope, aes(x = x, y = density, color = group)) +
  geom_line(linewidth = 1.2) +
  labs(
    title = paste("Scenario 2: Increased Slope =", true_slope),
    x = "Height",
    y = "Density",
    color = "Weight"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("blue", "red")) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    legend.position = "bottom"
  )

# Combine the two plots side by side with a shared legend
combined_plot <- plot1 + plot2 +
  plot_layout(guides = "collect") &  # Collect the legend
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, size = 16)
  )

# Add a single title to the combined plot
combined_plot <- combined_plot +
  plot_annotation(
    title = "Effect of Increasing Slope on Distribution Separation",
    theme = theme(plot.title = element_text(size = 18, hjust = 0.5))
  )

# Display the combined plot
combined_plot
