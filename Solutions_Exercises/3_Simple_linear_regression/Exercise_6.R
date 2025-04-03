set.seed(123)

# Generate data from a sine function
x <- seq(0, 2 * pi, length.out = 250)       # Predictor variable
true_y <- sin(x)                            # True values based on sine function

plot(x, true_y, type = "l", col = "blue", lwd = 2, 
     xlab = "x", ylab = "y", main = "True Sine Function")

# Add noise to simulate observed data
noise <- rnorm(length(x), mean = 0, sd = 0.2)
observed_y <- true_y + noise                # Observed response variable

points(x, observed_y, col = "red", pch = 19)

# Perform a simple linear regression with intercept
linear_model <- lm(observed_y ~ x)

# Display summary of the model
#summary(linear_model)

# add regression line 
abline(linear_model, col = "green", lwd = 2)

res <- residuals(linear_model)
hist(res)

# Extract residuals
res <- residuals(linear_model) # difference between red points and green line

# Autocorrelation function (ACF) plot
acf(res, main = "Autocorrelation of Residuals", col = "blue", lwd = 2)
# -> residuals are not uncorrelated with their neighbors -> they cannot
# be stochatically independent! 
