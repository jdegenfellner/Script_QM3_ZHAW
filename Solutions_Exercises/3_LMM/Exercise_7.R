library(rethinking)
library(tidyverse)

data(Howell1)
d <- Howell1
d %>% ggplot(aes(x = weight, y = height)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  geom_smooth(method = "loess", se = FALSE, color = "red")

# Choose 80% of d randomly:------
set.seed(123)
dim(d)[1] # 544
train_index <- sample(1:dim(d)[1], size = 0.8 * dim(d)[1])
d_train <- d[train_index,]
dim(d_train) # 435
d_test <- d[-train_index,]
dim(d_test) # 109
dim(d)[1] == dim(d_train)[1] + dim(d_test)[1]

# make the same plot, but now color the 80% training set in blue 
# and the 20% test set in red
d %>% ggplot(aes(x = weight, y = height)) +
  geom_point(data = d_train, aes(color = "Training set"), size = 2) +
  geom_point(data = d[!d$weight %in% d_train$weight, ], 
             aes(color = "Test set"), size = 2) +
  #geom_smooth(method = "lm", se = FALSE, color = "blue") +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  scale_color_manual(values = c("Training set" = "blue", "Test set" = "red")) +
  labs(color = "") +
  theme_minimal()

# The plot suggests that the model should do very well in 
# predicting the body heights given the body weights.

# Standardize weight (makes the intercept interpretable as average height of a person with average mass)
d_train$weight_s <- (d_train$weight - mean(d_train$weight)) / sd(d_train$weight)
# Square of standardized weight
d_train$weight_s2 <- d_train$weight_s^2
m4.1 <- quap(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b1*weight_s + b2*weight_s^2,
    a ~ dnorm(178, 20),
    b1 ~ dlnorm(0, 1),
    b2 ~ dnorm(0, 10),
    sigma ~ dunif(0, 50)
  ), data = d_train) # change to d_train which is our 80% training set
precis(m4.1)

# predict height using m4.1 for the 20% test set
d_test$weight_s <- (d_test$weight - mean(d_train$weight)) / sd(d_train$weight)
d_test$weight_s2 <- d_test$weight_s^2
# predict height using m4.1 for the 20% test set
mu_test_samples <- link(m4.1, data = d_test %>% dplyr::select(weight_s, weight_s2))
dim(mu_test_samples) # 1000 x 109; -> we get 1000 samples from the posterior for every weight-value in the test-set
d_test$mu <- colMeans(mu_test_samples)

# 1) Plot the predicted height against the actual height for the test set
d_test %>% ggplot(aes(x = mu, y = height)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  labs(x = "Predicted height", y = "Actual height") +
  theme_minimal()
# a bit of structure deviation, but not too bad.

# Calculate RMSE:
rmse <- sqrt(mean((d_test$mu - d_test$height)^2))
rmse
# This number alone does not tell us much, but we could compare this to other models
# and quantify how well body heights from the same distribution but not in the training
# set are predicted.
# This is basically the same as the residuals.


# Let's do this using the Frequentist approach:
# Fit a linear model to the training set
m4.1_freq <- lm(height ~ weight_s + weight_s2, 
           data = d_train) # least squares method
summary(m4.1_freq)
# Predict height using m4.1_freq for the 20% test set
d_test$mu_freq <- predict(m4.1_freq, newdata = d_test)
# 2) Plot the predicted height against the actual height for the test set
d_test %>% ggplot(aes(x = mu_freq, y = height)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  labs(x = "Predicted height", y = "Actual height") +
  theme_minimal()
# Calculate RMSE:
rmse_freq <- sqrt(mean((d_test$mu_freq - d_test$height)^2))
rmse_freq # 5.191794
rmse # 5.190535

# Since the priors do not play a role due to the large sample size, 
# we get the same prediction quality

# RMSE on the training set using Frequentist approach:
d_train$mu_freq <- predict(m4.1_freq, newdata = d_train)
rmse_train_freq <- sqrt(mean((d_train$mu_freq - d_train$height)^2))
rmse_train_freq # 5.8848

# In this case, the RMSE on the training set is larger than on the test set.
# This is rather unusual, but not impossible. 
# We might have chosen observations into our test set which are comparatively
# closer to the quadratic mean curve (see plot above).

# Typically we would expect the RMSE on the training set to 
# be smaller than on the test set.


# Repeat multiple times:
set.seed(123)
rmse_results <- replicate(100, {
  index <- sample(1:nrow(d), size = 0.8 * nrow(d))
  train <- d[index, ]
  test <- d[-index, ]
  
  # Standardisierung (am Training-Set)
  train$weight_s <- scale(train$weight)[,1]
  train$weight_s2 <- train$weight_s^2
  test$weight_s <- (test$weight - mean(train$weight)) / sd(train$weight)
  test$weight_s2 <- test$weight_s^2
  
  # Modell
  m <- lm(height ~ weight_s + weight_s2, data = train)
  
  # Vorhersagen
  pred_train <- predict(m, newdata = train)
  pred_test  <- predict(m, newdata = test)
  
  # RMSEs berechnen
  rmse_train <- sqrt(mean((pred_train - train$height)^2))
  rmse_test  <- sqrt(mean((pred_test - test$height)^2))
  
  # Rückgabe als Vektor
  c(train = rmse_train, test = rmse_test)
})

# Ergebnis als DataFrame
rmse_df <- as.data.frame(t(rmse_results))
names(rmse_df) <- c("RMSE_Train", "RMSE_Test")

rmse_df %>%
  pivot_longer(everything(), names_to = "Datensatz", values_to = "RMSE") %>%
  ggplot(aes(x = RMSE, fill = Datensatz)) +
  geom_histogram(alpha = 0.6, bins = 30, position = "identity") +
  geom_vline(data = rmse_df %>%
               pivot_longer(everything(), names_to = "Datensatz", values_to = "RMSE") %>%
               group_by(Datensatz) %>%
               summarise(mean_rmse = mean(RMSE)),
             aes(xintercept = mean_rmse, color = Datensatz),
             linetype = "dashed", size = 1) +
  labs(title = "RMSE-Verteilungen (100 Wiederholungen)",
       x = "RMSE", y = "Häufigkeit") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
