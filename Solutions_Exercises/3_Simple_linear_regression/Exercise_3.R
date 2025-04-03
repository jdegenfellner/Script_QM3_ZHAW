# create data from the regression model with changed parameter values

# Idea: We want too better understand the connection between the OLS estimates
# for beta_0 and beta_1 by playing around with the parameters and simulating new data
# that has different standard deviations for weight, height and probably also 
# different correlation.

library(rethinking)
data("Howell1")
d <- Howell1
d2 <- d[d$age >= 18,] # only adults

summary(d2$weight)
sd(d2$weight)
dim(d2)

mod <- lm(height ~ weight, data = d2)
summary(mod)

# Simulate new heights from the model! 100 new heights
# sigma = 5.086
# beta_0 = 113.87939
# beta_1 = 0.90574

# produce a single new height in cm for a person of 67 kg:
113.87939 + 0.90574*45 + rnorm(1, 0, 5.086)

dim(d2) # 352 people in the sample

height <- numeric(352)
weight <- numeric(352)
hist(d2$weight)
mean(d2$weight) # 44.99 kg
sd(d2$weight) # 6.456708 kg
#shapiro.test(d2$weight) # don't do this!

# simulate data from model-----------
for (i in 1:352) {
  weight[i] <- rnorm(1, 45, 6.45) # draw new weights
  height[i] <- 113.87939 + 0.90574 * weight[i] + rnorm(1, 0, 5.086 ) # draw new heights from the model
}
plot(weight, height, xlab = "weight", ylab = "height", main = "weight height plot")

# manual estimates by "hand":
# beta_1 = 
( beta_1_hat <- cor(weight, height) * sd(height)/(sd(weight)) )
# beta_0 =
( beta_0_hat <- mean(height) - beta_1_hat * mean(weight) )
# -> reconstructs nicely the true coefficients!

cor(weight, height)

# change parameters----------
# _change intercept:---------
height2 <- numeric(352)
weight2 <- numeric(352)
for (i in 1:352) {
  weight2[i] <- rnorm(1, 45, 6.45) # draw new weights
  height2[i] <- 50 + 0.90574 * weight2[i] + rnorm(1, 0, 5.086 ) # draw new heights from the model
}
sd(height2)
sd(height)
# -> no large change

cor(weight2, height2) # no change
# we just shift the points downwards

# _change beta_1:---------
height3 <- numeric(352)
weight3 <- numeric(352)
for (i in 1:352) {
  weight3[i] <- rnorm(1, 45, 6.45) # draw new weights
  height3[i] <- 113.87939 + 0.5 * weight3[i] + rnorm(1, 0, 5.086 ) # draw new heights from the model
}
sd(height3) # decrease
sd(height)
cor(weight3, height3) # decrease

# now we change both the sd of heights since they do not spread so much anymore, 
# the point cloud is less steep.
# Also the fixed model error of 5.086 is now larger with respect to the
# expected change caused by beta_1, hence, correlation goes down too.



