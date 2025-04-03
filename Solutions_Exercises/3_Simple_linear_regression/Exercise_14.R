set.seed(42)  # For reproducibility

# Define true parameters
true_intercept <- 5 # beta_0
true_slope <- 2.5 # beta_1
sigma <- 2  # Standard deviation of errors

# Number of simulations
n_sim <- 1000
n <- 200  # Sample size per simulation

# Store results
covered_intercept <- numeric(n_sim) # create an empty vector to store the estimates
covered_slope <- numeric(n_sim)

for (i in 1:n_sim) {
  # Simulate data from the true model
  #x <- runif(n, 0, 10)  # Predictor X from a uniform distribution
  x <- rnorm(n, 0, 3.5)  # Predictor X from a normal distribution
  y <- true_intercept + true_slope * x + rnorm(n, mean = 0, sd = sigma)  # Response
  
  # Fit linear model
  model <- lm(y ~ x)
  
  # Compute confidence intervals
  conf_intervals <- confint(model, level = 0.94)  # 94% CI by default
  
  # Check if the true values fall inside the CIs
  covered_intercept[i] <- (conf_intervals[1, 1] <= true_intercept) & (true_intercept <= conf_intervals[1, 2])
  covered_slope[i] <- (conf_intervals[2, 1] <= true_slope) & (true_slope <= conf_intervals[2, 2])
}

# Compute coverage probabilities
coverage_intercept <- mean(covered_intercept) 
sum(covered_intercept)/1000 # alternatively
coverage_slope <- mean(covered_slope)

cat("Coverage probability for intercept:", coverage_intercept, "\n")
cat("Coverage probability for slope:", coverage_slope, "\n")
# approx correct

# Change distribution of X:------

for (i in 1:n_sim) {
  # Simulate data from the true model
  x <- rgamma(n, 4, 4)  # Predictor X from a uniform distribution
  y <- true_intercept + true_slope * x + rnorm(n, mean = 0, sd = sigma)  # Response
  
  # Fit linear model
  model <- lm(y ~ x)
  
  # Compute confidence intervals
  conf_intervals <- confint(model, level = 0.94)  # 94% CI by default
  
  # Check if the true values fall inside the CIs
  covered_intercept[i] <- (conf_intervals[1, 1] <= true_intercept) & (true_intercept <= conf_intervals[1, 2])
  covered_slope[i] <- (conf_intervals[2, 1] <= true_slope) & (true_slope <= conf_intervals[2, 2])
}

# Compute coverage probabilities
coverage_intercept <- mean(covered_intercept)
coverage_slope <- mean(covered_slope)

cat("Coverage probability for intercept:", coverage_intercept, "\n")
cat("Coverage probability for slope:", coverage_slope, "\n")
# approx correct

# What about fixed X-values?------

for (i in 1:n_sim) {
  # Simulate data from the true model
  # range of 10, 1,2,3,...,10
  x <- rep(1:10, each=n/10)
  #x <- runif(n, 0, 10)  # Predictor X from a uniform distribution
  y <- true_intercept + true_slope * x + rnorm(n, mean = 0, sd = sigma)  # Response
  
  # Fit linear model
  model <- lm(y ~ x)
  
  # Compute confidence intervals
  conf_intervals <- confint(model, level = 0.94)  # 94% CI by default
  
  # Check if the true values fall inside the CIs
  covered_intercept[i] <- (conf_intervals[1, 1] <= true_intercept) & (true_intercept <= conf_intervals[1, 2])
  covered_slope[i] <- (conf_intervals[2, 1] <= true_slope) & (true_slope <= conf_intervals[2, 2])
}

# Compute coverage probabilities
coverage_intercept <- mean(covered_intercept)
coverage_slope <- mean(covered_slope)

cat("Coverage probability for intercept:", coverage_intercept, "\n")
cat("Coverage probability for slope:", coverage_slope, "\n")
# approx correct
