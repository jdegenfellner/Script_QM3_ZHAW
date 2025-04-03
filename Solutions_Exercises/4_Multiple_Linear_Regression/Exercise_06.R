library(rethinking)
set.seed(123)
n <- 100
X1 <- rnorm(n, 0, 5)
X2 <- rnorm(n, 0, 5)
Y <- 10 + 0.5 * X1 + 1 * X2 + rnorm(n, 0, 2) # true model
df <- data.frame(X1 = X1, X2 = X2, Y = Y)

# 1) Model with 2 predictors--------------
# fit model
m4.2 <- quap(
  alist(
    Y ~ dnorm(mu, sigma),
    mu <- a + b1*X1 + b2*X2,
    a ~ dnorm(10, 10),
    b1 ~ dnorm(0, 10),
    b2 ~ dnorm(0, 10),
    sigma ~ dunif(0, 50)
  ), data = df)
precis(m4.2)

# _Posterior predictive checks Y vs Y_hat-----
# see Statstical Rethinking p 138.
# call link without specifying new data
# so it uses the original data
mu <- link(m4.2)

# summarize samples accross cases
mu_mean <- apply(mu, 2, mean)
mu_PI <- apply(mu, 2, PI, prob = 0.89)

# simulate observations
# again, no new data, so uses original data
D_sim <- sim(m4.2, n = 1e4)
D_PI <- apply(D_sim, 2, PI, prob = 0.89)

plot(mu_mean ~ df$Y, col = rangi2, ylim = range(mu_PI), 
     xlab = "Observed Y", ylab = "Model-Predicted Y")
abline(a = 0, b = 1, lty = 2)
for(i in 1:nrow(df)) lines(rep(df$Y[i], 2), mu_PI[,i], col = rangi2)

# 2) Same for mean model--------
# fit model
m4.1 <- quap(
  alist(
    Y ~ dnorm(mu, sigma),
    mu <- a,
    a ~ dnorm(10, 10),
    sigma ~ dunif(0, 50)
  ), data = df)
precis(m4.1)

# _Posterior predictive checks Y vs Y_hat-----
# see Statstical Rethinking p 138.
# call link without specifying new data
# so it uses the original data
mu <- link(m4.1)

# summarize samples accross cases
mu_mean <- apply(mu, 2, mean)
mu_PI <- apply(mu, 2, PI, prob = 0.89)

# simulate observations
# again, no new data, so uses original data
D_sim <- sim(m4.1, n = 1e4)
D_PI <- apply(D_sim, 2, PI, prob = 0.89)

plot(mu_mean ~ df$Y, col = rangi2, ylim = range(mu_PI), 
     xlab = "Observed Y", ylab = "Model-Predicted Y")
abline(a = 0, b = 1, lty = 2)
# completely off

# _Posterior predictive densities-------
# Simulate observations using the posterior predictive distribution
D_sim <- sim(m4.1, n = 1e4)  # Generate 10,000 simulated datasets

# Calculate densities for all samples
densities <- apply(D_sim, 1, density)

# Find the maximum density value for setting the y-axis limits
max_density <- max(sapply(densities, function(d) max(d$y)))

# Create the density plot with predefined ylim
plot(NULL, xlim = range(df$Y), ylim = c(0, max_density),
     xlab = "Y", ylab = "Density",
     main = "Comparison of Observed and Predicted Densities")

# Add 100 posterior predictive density lines
set.seed(42)  # For reproducibility
n_lines <- 100
samples <- sample(1:1e4, n_lines)  # Randomly sample 100 posterior predictive datasets
for (s in samples) {
  lines(density(D_sim[s, ]), col = alpha("lightblue", 0.3), lwd = 1)
}

# Add the density line for the observed Y values
obs_density <- density(df$Y)
lines(obs_density$x, obs_density$y, col = "green", lwd = 2)

# Add legend
legend("topright", legend = c("Posterior Predictive Densities", "Observed Density"),
       col = c("lightblue", "green"), lty = 1, lwd = c(1, 2))

# this does work in this simple case! 
# at least with respect to the distributional form of Y!


# Summarising: 
# - the individual predictions are completely off, which makes sense, since 
#   the simple mean model ignores an increase in height that is associated with
#   height weight.
# - The distributional form of the mean model is also a normal distribution 
#   and looking at this alone would fit well.