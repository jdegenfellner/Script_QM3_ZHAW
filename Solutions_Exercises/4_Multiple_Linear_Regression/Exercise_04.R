library(plotly)
library(tidyverse)

# 1) Standardise the predictors. How are the βs changing and what 
# is their interpration now?

set.seed(123)
n <- 100
X1 <- rnorm(n, 0, 5)
X2 <- rnorm(n, 0, 5)

# 1) Standardize (in this case, since we know the true model, -------
# we could just use rnorm(n, 0, 1) instead of standardizing)

X1_st <- (X1 - mean(X1)) / sd(X1)
X2_st <- (X2 - mean(X2)) / sd(X2)

sd(X1)
sd(X2)

Y <- 10 + 0.5 * X1 + 1 * X2 + 0.89 * X1 * X2 + rnorm(n, 0, 5)

d <- data.frame(X1 = X1, X2 = X2,
                X1_st, X2_st, Y = Y)

# Fit the model (not standardized)
m4.4 <- lm(Y ~ X1 * X2, data = d)
summary(m4.4)

# Fit the model (standardized)
m4.4_st <- lm(Y ~ X1_st * X2_st, data = d)
summary(m4.4_st)

# Important: Since we have an interaction term we CANNOT interpret 
# the coefficients directly (as we did in the case of no interaction).
# Reason: We cannot change x1 and hold all other predictors incl. the interaction
# constant!

# 2) Change the relative sizes of the true but usually unknown βs.  -----
# What happens to the estimates and the graph?

set.seed(123)
n <- 100
X1 <- rnorm(n, 0, 5)
X2 <- rnorm(n, 0, 5)
Y <- 10 + 0.5 * X1 + 1 * X2 + 0.005 * X1 * X2 + rnorm(n, 0, 5)
d <- data.frame(X1 = X1, X2 = X2,
                X1_st, X2_st, Y = Y)
m4.4 <- lm(Y ~ X1 * X2, data = d)
summary(m4.4)

# Create a grid for the plane
X1_grid <- seq(min(d$X1), max(d$X1), length.out = 20)
X2_grid <- seq(min(d$X2), max(d$X2), length.out = 20)
grid <- expand.grid(X1 = X1_grid, X2 = X2_grid)

# Predict the values for the grid
grid$Y <- predict(m4.4, newdata = grid)

# Convert the grid into a matrix for the plane
plane_matrix <- matrix(grid$Y, nrow = length(X1_grid), ncol = length(X2_grid))

# Predict the values for the grid
grid$Y <- predict(m4.4, newdata = grid)

# Create the interactive 3D plot
plot_ly() %>%
  add_markers(
    x = d$X2, y = d$X1, z = d$Y,
    marker = list(color = "blue", size = 5),
    name = "Data Points"
  ) %>%
  add_surface(
    x = X1_grid, y = X2_grid, z = plane_matrix,
    colorscale = list(c(0, 1), c("red", "pink")),
    showscale = FALSE,
    opacity = 0.7,
    name = "Fitted Plane"
  ) %>%
  plotly::layout(
    scene = list(
      xaxis = list(title = "X1"),
      yaxis = list(title = "X2"),
      zaxis = list(title = "Y")
    ),
    title = "Interactive 3D Scatterplot with Fitted Plane"
  )

# beta_3 is relatively smaller compared to the other predictors,
# the smaller beta_3 becomes, the more we are inclined to describe 
# the point cloud with a plane without an interaction.

# 3) What happens if you change the error term and increase ---------
# or decrease its variance?

set.seed(123)
n <- 100
X1 <- rnorm(n, 0, 5)
X2 <- rnorm(n, 0, 5)

Y <- 10 + 0.5 * X1 + 1 * X2 + 0.89 * X1 * X2 + rnorm(n, 0, 50) # change sigma

d <- data.frame(X1 = X1, X2 = X2,
                X1_st, X2_st, Y = Y)

# Fit the model (not standardized)
m4.4 <- lm(Y ~ X1 * X2, data = d)
summary(m4.4)

# Create a grid for the plane
X1_grid <- seq(min(d$X1), max(d$X1), length.out = 20)
X2_grid <- seq(min(d$X2), max(d$X2), length.out = 20)
grid <- expand.grid(X1 = X1_grid, X2 = X2_grid)

# Predict the values for the grid
grid$Y <- predict(m4.4, newdata = grid)

# Convert the grid into a matrix for the plane
plane_matrix <- matrix(grid$Y, nrow = length(X1_grid), ncol = length(X2_grid))

# Predict the values for the grid
grid$Y <- predict(m4.4, newdata = grid)

# Create the interactive 3D plot
plot_ly() %>%
  add_markers(
    x = d$X2, y = d$X1, z = d$Y,
    marker = list(color = "blue", size = 5),
    name = "Data Points"
  ) %>%
  add_surface(
    x = X1_grid, y = X2_grid, z = plane_matrix,
    colorscale = list(c(0, 1), c("red", "pink")),
    showscale = FALSE,
    opacity = 0.7,
    name = "Fitted Plane"
  ) %>%
  plotly::layout(
    scene = list(
      xaxis = list(title = "X1"),
      yaxis = list(title = "X2"),
      zaxis = list(title = "Y")
    ),
    title = "Interactive 3D Scatterplot with Fitted Plane"
  )


