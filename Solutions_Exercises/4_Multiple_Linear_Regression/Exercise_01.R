# ex 1

library(rethinking)
library(tidyverse)
data(Howell1)
d <- Howell1
dim(d)

# Standardize weight
d$weight_s <- (d$weight - mean(d$weight)) / sd(d$weight)
# Square of standardized weight
d$weight_s2 <- d$weight_s^2
d$weight_s3 <- d$weight_s^3

# fit the model---------
m4.2 <- quap(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b1*weight_s + b2*weight_s^2 + b3*weight_s^3,
    a ~ dnorm(178, 20),
    b1 ~ dnorm(0, 10),
    b2 ~ dnorm(0, 10),
    b3 ~ dnorm(0, 10),
    sigma ~ dunif(0, 50)
  ), data = d)
precis(m4.2)

# draw the mean function---------

# Summarize the model parameters
model_summary <- precis(m4.2)
params <- as.data.frame(model_summary)

# Extract parameter values
a <- params["a", "mean"]       # Intercept
b1 <- params["b1", "mean"]     # Coefficient for standardized weight
b2 <- params["b2", "mean"]     # Coefficient for squared standardized weight
b3 <- params["b3", "mean"]     # Coefficient for cubed standardized weight

# Generate a sequence of standardized weights for the fitted curve
weight_seq <- seq(min(d$weight_s), max(d$weight_s), length.out = 200)

# Calculate the fitted values using the quadratic equation
height_fitted <- a + b1 * weight_seq + b2 * weight_seq^2 + b3 * weight_seq^3

# Plot the scatterplot
plot(d$weight_s, d$height, pch = 16, col = "blue",
     xlab = "Standardized Weight", ylab = "Height (cm)",
     main = "Scatterplot with Fitted Curve (Standardized Weight)")

# Add the fitted curve
lines(weight_seq, height_fitted, col = "red", lwd = 2)

# Add a legend
legend("topright", legend = c("Observed data", "Fitted curve"),
       col = c("blue", "red"), pch = c(16, NA), lty = c(NA, 1), lwd = 2)


# add prediction bands---------

# Generate a sequence of standardized weights for predictions
weight_seq <- seq(min(d$weight_s), max(d$weight_s), length.out = 200)

# Create a data frame for predictions
pred_data <- tibble(weight_s = weight_seq, 
                    weight_s2 = weight_seq^2, 
                    weight_s3 = weight_seq^3)

# Simulate posterior predictive heights
sim_heights <- sim(m4.2, data = pred_data, n = 1000)
dim(sim_heights)

# Compute the mean, 89% interval, and 95% interval for predictions
height_mean <- apply(sim_heights, 2, mean)
height_PI_89 <- apply(sim_heights, 2, PI, prob = 0.89)
height_PI_95 <- apply(sim_heights, 2, PI, prob = 0.95)
height_PI_99 <- apply(sim_heights, 2, PI, prob = 0.99)

# Plot
plot(d$weight_s, d$height, pch = 16, col = alpha("blue", 0.5),
     xlab = "Standardized Weight", ylab = "Height (cm)",
     main = "Scatterplot with Fitted Curve & Prediction Intervals")

# Add the 89% prediction interval using shade()
shade(height_PI_89, weight_seq, col = alpha("gray", 0.5))

# Add the 99% prediction interval using shade()
shade(height_PI_99, weight_seq, col = alpha("darkgray", 0.5))



# lets overdo it----------

# add polynomial smmoothing line to ggplot:
ggplot(d, aes(x = weight_s, y = height)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x, 23), se = FALSE) +
  theme_minimal() +
  labs(title = "Body height vs. weight",
       x = "Standardized weight",
       y = "Height") +
  theme(plot.title = element_text(hjust = 0.5))
