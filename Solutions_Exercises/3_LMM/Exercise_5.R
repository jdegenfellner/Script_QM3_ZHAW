library(rethinking)
conflicts_prefer(rethinking::logit)
conflicts_prefer(rethinking::logistic)
x <- seq(-10, 10, length.out = 100)
round(logit(logistic(x)) - x, 2) # diff should be very small ~ 0

y <- seq(0, 1, length.out = 100)
round(logistic(logit(y)) - y, 2) # diff should be very small ~ 0


# Plot logit: (0,1) -> (-\infty, +\infty)
plot(y, logit(y), type = "l", col = "blue", lwd = 2,
     xlab = "x", ylab = "logit(x)",
     main = "logit(x) vs x",
     ylim = c(-10, 10))
abline(h = 0, col = "red", lty = 2)
abline(v = 0.5, col = "red", lty = 2)

# Plot logistic: (-\infty, +\infty) -> (0,1)
plot(x, logistic(x), type = "l", col = "blue", lwd = 2,
     xlab = "x", ylab = "logistic(x)",
     main = "logistic(x)",
     xlim = c(-10, 10))
abline(h = 0.5, col = "red", lty = 2)
abline(v = 0, col = "red", lty = 2)
