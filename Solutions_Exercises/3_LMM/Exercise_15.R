set.seed(334)
AGE <- rnorm(100)
y <- rbinom(100, size = 1, prob = inv_logit(AGE)) # the probability of a 1 increases with AGE

df <- data.frame(AGE, y)
df


# try to find parameters with least squares:
inv_logit <- function(x) {
  1 / (1 + exp(-x))
}

# Loss-Funktion (least squares loss)
loss_function <- function(par, x, y) {
  beta_0 <- par[1]
  beta_1 <- par[2]
  y_hat <- inv_logit(beta_0 + beta_1 * x)
  sum((y - y_hat)^2)  # Sum of squared errors
}

# Optimierung
set.seed(334)
AGE <- rnorm(100)
y <- rbinom(100, size = 1, prob = inv_logit(AGE))

res <- optim(par = c(0, 0), fn = loss_function, x = AGE, y = y)

res$par  # Estimated beta_0 and beta_1

modlog <- glm(y ~ AGE, family = binomial(link = "logit"))
summary(modlog)
coef(modlog)  
