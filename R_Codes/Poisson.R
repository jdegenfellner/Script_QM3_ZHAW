library(ggplot2)

# Wertebereich und Lambdas definieren
lambda_values <- c(1, 5, 10, 20)
x_vals <- 0:40

# Daten generieren
poisson_data <- expand.grid(x = x_vals, lambda = lambda_values)
poisson_data$prob <- dpois(poisson_data$x, poisson_data$lambda)
poisson_data$lambda <- factor(poisson_data$lambda, 
                              labels = paste0("λ = ", lambda_values))

# Plot
ggplot(poisson_data, aes(x = x, y = prob, color = lambda)) +
  geom_line() +
  geom_point() +
  labs(title = "Poisson distributions",
       x = "X",
       y = "density") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))


# create fake data for regression
set.seed(123)
x <- rnorm(100, mean = 5, sd = 2)
y <- rpois(100, lambda = exp(0.5 * x - 2))
plot(x, y, main = "Poisson Regression Data",
     xlab = "x", ylab = "y", col = "blue", pch = 19)


library(ggplot2)
library(dplyr)

# Simulate base data
set.seed(123)
x <- rnorm(100, mean = 5, sd = 2)
y <- rpois(100, lambda = exp(0.5 * x - 2))
df_points <- data.frame(x = x, y = y)

# Reference x-values and corresponding lambdas
x_ref <- c(2, 4, 6, 8)
lambda_ref <- exp(0.5 * x_ref - 2)
width <- 1  # horizontal stretch of the density curves

# Generate only the right half of each Poisson density curve
poisson_curves <- lapply(seq_along(x_ref), function(i) {
  lam <- lambda_ref[i]
  xx <- x_ref[i]
  y_vals <- 0:20
  d_vals <- dpois(y_vals, lam)
  
  data.frame(
    x = xx + d_vals * width,
    y = y_vals,
    group = paste0("x = ", xx)
  )
}) %>% bind_rows()

# Integer y values for horizontal reference lines
y_breaks <- 0:max(df_points$y)

# Lambda curve: exp(0.5 * x - 2)
lambda_curve <- data.frame(
  x = seq(min(df_points$x), max(df_points$x), length.out = 200)
) %>%
  mutate(y = exp(0.5 * x - 2))

# Plot
ggplot(df_points, aes(x = x, y = y)) +
  # scatterplot
  geom_point(color = "blue", alpha = 0.6) +
  # vertical dashed reference lines
  geom_vline(xintercept = x_ref, linetype = "dashed", color = "red", linewidth = 0.3) +
  # horizontal dashed lines at integer y-values
  geom_hline(yintercept = y_breaks, linetype = "dashed", color = "gray50", linewidth = 0.2) +
  # right half of Poisson curves
  geom_path(data = poisson_curves, aes(x = x, y = y, group = group),
            color = "red", linewidth = 1) +
  # lambda curve
  geom_line(data = lambda_curve, aes(x = x, y = y),
            color = "darkred", linewidth = 1, linetype = "solid") +
  # x-axis only at reference points
  scale_x_continuous(breaks = x_ref) +
  labs(
    title = "Poisson Model: Densities and Lambda(x) = exp(0.5x - 2)",
    x = "x",
    y = "y"
  ) +
  theme_minimal()



# Bayes
set.seed(123)
x <- rnorm(100, mean = 5, sd = 2)
y <- rpois(100, lambda = exp(0.5 * x - 2))
df <- data.frame(x = x, y = y)

# with rethinking
library(rethinking)
# Fit the model
model <- ulam(
  alist(
    y ~ dpois(lambda),
    log(lambda) <- a + b * x,
    a ~ dnorm(0, 1),
    b ~ dnorm(0, 1)
  ),
  data = df,
  iter = 2000,
  chains = 4
)
precis(model)
# looks good!

# predictions with confidence band:
library(rethinking)
conflicts_prefer(rethinking::sim)
x_seq <- seq(from = min(df$x), to = max(df$x), length.out = 100)
pred_data <- list(x = x_seq)
lambda_pred <- link(model, data = pred_data)
y_pred <- sim(model, data = pred_data)
lambda_mean <- apply(lambda_pred, 2, mean)
lambda_ci <- apply(lambda_pred, 2, PI, prob = 0.89)
y_ci <- apply(y_pred, 2, PI, prob = 0.89)

plot(df$x, df$y, col = alpha("black", 0.4), pch = 16,
     xlab = "x", ylab = "y", main = "Posterior Predictions with 89% CI")
lines(x_seq, lambda_mean, col = "red", lwd = 2)
shade(lambda_ci, x_seq, col = col.alpha("red", 0.2))
shade(y_ci, x_seq, col = col.alpha("red", 0.1))



# Simuliere 100 Posterior predictive samples (für gesamte y-Vektoren)
y_sim <- rethinking::sim(model, data = df)  # df enthält x und y
# y_sim: Matrix [1000 Samples × 100 Beobachtungen]

# Wähle z. B. 100 Draws aus der Posterior Predictive Matrix
set.seed(42)
draws_to_plot <- sample(1:nrow(y_sim), size = 100)

# Zeichne Posterior Predictive Dichten (jede simulierte y-Zeile)
plot(NULL, xlim = range(y), ylim = c(0, 0.3), xlab = "y", ylab = "Density",
     main = "Posterior Predictive Check (Simulated y vs. Observed y)")

# Blaue Dichten: die 100 simulierten
for (i in draws_to_plot) {
  lines(density(y_sim[i, ]), col = col.alpha("blue", 0.1))
}

# Grüne Dichte: beobachtetes y
lines(density(df$y), col = "darkgreen", lwd = 2)



# Frequenist
mod_glm <- glm(y ~ x, data = df, family = poisson(link = "log"))
summary(mod_glm)
confint(mod_glm)

# deviance:

# Modell-Log-Likelihood
(logLik_model <- logLik(mod_glm))

# Saturierte Log-Likelihood = Summe von log dpois(y_i, lambda = y_i)
(logLik_saturated <- sum(dpois(df$y, lambda = df$y, log = TRUE)))

# Deviance manuell
D_manual <- 2 * (logLik_saturated - logLik_model)
D_manual

# Vergleich mit:
mod_glm$deviance




# assumptions---------
?check_model
check_model(mod_glm)
check_model(mod_glm, check = "pp_check")

# linearity
library(mgcv)
conflicts_prefer(mgcv::gam)
mod_gam <- gam(y ~ s(x), family = poisson(link = "log"), data = df)
plot(mod_gam, residuals = TRUE)

rpois(1, lambda = exp(0.5 * 10 - 2))

plot(x, predict(mod_glm, type = "link"),
     main = "Linear Predictor (log(μ)) vs. x",
     xlab = "x", ylab = "log(μ̂)")

library(car)
?crPlots
crPlots(mod_glm)

check_overdispersion(mod_glm)

check_outliers(mod_glm)

# 
