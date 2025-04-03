set.seed(123)
n <- 100
X1 <- rnorm(n, 0, 5)
X2 <- rnorm(n, 0, 5)

Y <- 10 + 0.5 * X1 + 1 * X2 + 0 * X1 * X2 + rnorm(n, 0, 5) # change sigma

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

# Create categorical variables based on quartiles
d$X2_cat <- cut(d$X2, 
                breaks = quantile(d$X2, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE), 
                include.lowest = TRUE, 
                labels = c("Q1", "Q2", "Q3", "Q4"))

d$X1_cat <- cut(d$X1, 
                breaks = quantile(d$X1, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE), 
                include.lowest = TRUE, 
                labels = c("Q1", "Q2", "Q3", "Q4"))

# Create the interaction plot
interaction.plot(d$X2_cat, d$X1_cat, d$Y)
