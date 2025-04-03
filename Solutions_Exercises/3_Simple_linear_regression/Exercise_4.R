library(rethinking)
data("Howell1")
d <- Howell1
d2 <- d[d$age >= 18,] # only adults

summary(d2$weight)
sd(d2$weight)
dim(d2)

# simulate data from model-----------

# lets make beta small and see if the correlation changes as we expect:
height <- numeric(352)
weight <- numeric(352)
for (i in 1:352) {
  weight[i] <- rnorm(1, 45, 6.45) # draw new weights
  height[i] <- 113.87939 + 0.00574 * weight[i] + rnorm(1, 0, 5.086 ) # draw new heights from the model
}

summary_mod <- summary(lm(height ~ weight))
summary_mod$r.squared
cor(height, weight)

plot(height, weight) # horizontal noise

# and fun fact about R^2 and correlation:
summary_mod$r.squared # is equal to:
cor(height, weight)^2
