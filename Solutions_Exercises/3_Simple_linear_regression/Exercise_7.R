# ex 7

library(rethinking)
data(Howell1)
d <- Howell1
d2 <- d[ d$age >= 18 , ]

# simulate data from model-----------
# We assume THIS is the data generating mechanism!
weight <- numeric(352)
height <- numeric(352)
for (i in 1:352) {
  weight[i] <- rnorm(1, 45, 6.45) # draw new weights
  height[i] <- 113.87939 + 0.90574 * weight[i] + rnorm(1, 0, 5.086 ) # draw new heights from the model
}

# estimate model:
m7.1 <- lm( height ~ weight , data = data.frame(height = height, 
                                                weight = weight) )

# 1) Draw y hat, ei plot:
plot( residuals(m7.1) ~ predict(m7.1) , data = data.frame(height = height, 
                                          weight = weight) )
# -> mark this line up to "simulate model" and run it over and over again
#    to get a feeling for the variability of the plot which is based 
#    on data produced by the model

# 2) Draw y hat, abs(ei) plot:
# Here, we know that we do NOT have heteroskedasticity!
data.frame(height = height, 
                  weight = weight)  %>%
  ggplot(aes(x = predict(m7.1), y = abs(residuals(m7.1))) ) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)
# seems rather flat
# -> mark this line up to "simulate model" and run it over and over again
