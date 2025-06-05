# ex 13 
# Why are the raw residuals not so useful in the case of logistic regression?

# Create data -----------
set.seed(334)
AGE <- rnorm(100)
y <- rbinom(100, size = 1, prob = inv_logit(AGE)) # the probability of a 1 increases with AGE

# Estimate logistic regression model----------
modlog <- glm(y ~ AGE, family = binomial(link = "logit"))
summary(modlog)

data.frame(y_i = y,
           AGE = AGE,
           y_hat = predict(modlog, type = "response"), # model predicted probabilities for a heart attack
           raw_residuals = residuals(modlog, type = "response")) %>%
  ggplot(aes(x = AGE, y = raw_residuals)) +
  geom_point() +
  labs(
    title = "AGE vs. Raw Residuals",
    x = "AGE",
    y = "y_i"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# This plot looks exactly the same everytime and does
# not tell much about the model fit
