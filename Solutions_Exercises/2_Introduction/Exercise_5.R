# Change parameters:-------
library(MASS)    # For mvrnorm
library(plotly)  # For 3D plotting

# Define the parameters of the bivariate normal distribution
mu <- c(1, -1)  # Mean vector X and Y
var_x <- 0.6
var_y <- 1.2
covar_xy <- 0.5
(correlation = covar_xy / sqrt(var_x * var_y)) # Correlation
sigma <- matrix(c(var_x, covar_xy, 
                  covar_xy, var_y), ncol = 2) # Covariance matrix
# See Exercise 7 how to get the correlation matrix!
cov2cor(sigma)  # Convert covariance matrix to correlation matrix

# Change mu: shifts the whole mountain
# Change var_x, var_y: changes how broad the mounatain is in each direction
# also changes correlations...

# Generate a grid of x and y values
x <- seq(-3, 3, length.out = 100)
y <- seq(-3, 3, length.out = 100)
grid <- expand.grid(x = x, y = y)

# Compute the density of the bivariate normal
z <- matrix(dmvnorm(grid, mean = mu, sigma = sigma),
            nrow = length(x), ncol = length(y))

# Define the square for highlighting the probability volume
highlight_x <- c(0.5, 2)
highlight_y <- c(0.5, 2)

# Highlighted volume: Set values outside the square to NA
z_highlight <- z
z_highlight[grid$x < highlight_x[1] | grid$x > highlight_x[2] |
              grid$y < highlight_y[1] | grid$y > highlight_y[2]] <- NA

# Create a 3D plot
fig <- plot_ly() %>%
  add_surface(x = x, y = y, z = z, showscale = FALSE, opacity = 0.6) %>%
  add_surface(x = x, y = y, z = z_highlight,
              colorscale = "Viridis", opacity = 0.9) %>%
  plotly::layout(scene = list(
    xaxis = list(title = "Y"),
    yaxis = list(title = "X"),
    zaxis = list(title = "Density")
  ))

fig

# Load the mvtnorm package
library(mvtnorm)

# Define the parameters of the bivariate normal distribution
mu <- c(0, 0)                       # Mean
sigma <- matrix(c(0.6, 0.5, 0.5, 0.6), ncol = 2) # Covariance matrix
cov2cor(sigma)  # Convert covariance matrix to correlation matrix

# Define the bounds of the square
highlight_x <- c(0.5, 2)
highlight_y <- c(0.5, 2)
# Calculate the probability using pmvnorm
pmvnorm(
  lower = c(highlight_x[1], highlight_y[1]),
  upper = c(highlight_x[2], highlight_y[2]),
  mean = mu, # mean vector, mu = c(0, 0)
  sigma = sigma # covariance matrix
)

# Simulate probability of the square----------

# Load necessary library
library(MASS)

# Define the parameters of the bivariate normal distribution
mu <- c(0, 0)                       # Mean
sigma <- matrix(c(0.6, 0.5, 0.5, 0.6), ncol = 2) # Covariance matrix
cov2cor(sigma)  # Convert covariance matrix to correlation matrix
# rho = 0.83

# Define the bounds of the square
highlight_x <- c(0.5, 2)
highlight_y <- c(0.5, 2)

# Number of simulations
n_sim <- 10^4

set.seed(343434)
# Simulate bivariate normal samples
samples <- mvrnorm(n = n_sim, mu = mu, Sigma = sigma)
head(samples)

# Count how many samples fall within the square
inside_square <- sum(
  samples[, 1] >= highlight_x[1] & samples[, 1] <= highlight_x[2] &
    samples[, 2] >= highlight_y[1] & samples[, 2] <= highlight_y[2]
)

# Estimate the probability
inside_square / n_sim

# Scatterplot
plot(samples, col = ifelse(samples[, 1] >= highlight_x[1] & samples[, 1] <= highlight_x[2] &
                            samples[, 2] >= highlight_y[1] & samples[, 2] <= highlight_y[2], "red", "black"))
cor(samples[, 1], samples[, 2]) # 0.83 (should be approximately 
# identical with the values in the correlation matrix)




