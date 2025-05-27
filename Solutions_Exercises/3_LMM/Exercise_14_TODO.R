AGE <- rnorm(100)
y <- rbinom(100, size = 1, prob = inv_logit(AGE))
plot(AGE, y, main = "Inverse Logit Function", 
     xlab = "x", ylab = "inv_logit(AGE)", col = "blue", pch = 19)
abline(lm(y ~ AGE), col = "red", lwd = 2)

#estimate logistic regression model
model <- glm(y ~ AGE, family = binomial(link = "logit"))
summary(model)
exp(coef(model))  # Exponentiated coefficients)
exp(confint(model))
data.frame(coefs = coef(model), 
           exp_coefs = exp(coef(model)), 
           confint_lower = exp(confint(model)[, 1]), 
           confint_upper = exp(confint(model)[, 2]))

predicted_probabilities <- predict(model, type = "response")
plot(AGE, y, main = "Inverse Logit Function", 
     xlab = "x", ylab = "inv_logit(AGE)", col = "blue", pch = 19)
# add predicted probabilities
points(AGE, predicted_probabilities, col = "red", pch = 19)


library(broom)
tidy_modlog <- tidy(model, conf.int = TRUE, exponentiate = TRUE)
tidy_modlog

modlog <- model

# probability-scale:
predict(modlog, type = "response", 
        newdata = data.frame(AGE = 0))
predict(modlog, type = "response", 
        newdata = data.frame(AGE = 1))

# log-odds scale:
predict(modlog, type = "link", 
        newdata = data.frame(AGE = 0))
predict(modlog, type = "link", 
        newdata = data.frame(AGE = 1))

# odds scale:
exp(predict(modlog, type = "link", 
            newdata = data.frame(AGE = 0)))
exp(predict(modlog, type = "link",
            newdata = data.frame(AGE = 1)))
6.175935 / 1.348729 



library(rethinking)

dat <- list(
  y = y,
  AGE = AGE,
  N = length(y)
)

m_logistic <- ulam(
  alist(
    y ~ dbinom(1, p),
    logit(p) <- a + b * AGE,
    a ~ normal(0, 1.5),
    b ~ normal(0, 1.5)
  ),
  data = dat,
  chains = 4,
  cores = 4
)

precis(m_logistic)






set.seed(332)
AGE <- rnorm(100)
y <- rbinom(100, size = 1, prob = inv_logit(AGE))
sum(y)/length(y) # Proportion of successes
modlog <- glm(y ~ AGE, family = binomial(link = "logit"))





check_model(modlog)



# PPC
p_hat <- fitted(modlog)
p_sims <- numeric(1000)
for(i in 1:1000){
  y_sim <- rbinom(100, 1, prob = p_hat)
  p_sim_1 <- sum(y_sim/length(y_sim))
  p_sim_0 <- 1 - p_sim_1
  p_sims[i] <- p_sim_1
}
hist(p_sims, main = "Posterior Predictive Check", 
     xlab = "Simulated Proportion of Successes", 
     col = "lightblue", border = "black")

# binned residuals
res <- binned_residuals(modlog, residuals = "response")
as.data.frame(res)
plot(res)




# Standardisierte Pearson-Residuals
resid_std <- residuals(modlog, type = "pearson")

# Berechne Uniform-transformation der Pearson-Residuals
# Verwende pnorm, um Residuen in Uniform(0,1) zu transformieren
resid_uniform <- pnorm(resid_std)

# QQ-Plot gegen die theoretische Uniform(0,1)
qqplot_uniform <- function(resid_uniform) {
  n <- length(resid_uniform)
  theoretical <- (1:n) / (n + 1)  # Standard-Uniform Quantile
  observed <- sort(resid_uniform)
  
  ggplot(data.frame(theoretical, observed), aes(x = theoretical, y = observed)) +
    geom_point(color = "steelblue", size = 2) +
    geom_abline(intercept = 0, slope = 1, color = "darkgreen") +
    geom_ribbon(aes(ymin = pmax(0, theoretical - 0.05), ymax = pmin(1, theoretical + 0.05)),
                fill = "grey80", alpha = 0.5) +
    labs(
      title = "Uniformity of Residuals",
      subtitle = "Dots should fall along the line",
      x = "Standard Uniform Distribution Quantiles",
      y = "Sample Quantiles"
    ) +
    theme_minimal()
}

# Plot
qqplot_uniform(resid_uniform)

check_residuals(modlog)

library(arm)
binnedplot(fitted(modlog), 
           residuals(modlog, type = "response"), 
           nclass = NULL, 
           xlab = "Expected Values", 
           ylab = "Average residual", 
           main = "Binned residual plot", 
           cex.pts = 0.8, 
           col.pts = 1, 
           col.int = "gray")


plot(modlog)



# install if needed
install.packages("ResourceSelection")

library(ResourceSelection)

# modlog: dein logistisches Modell
# g=10 → Gruppiere nach Dezilen des vorhergesagten Risikos
hoslem.test(y, 
            fitted(modlog), 
            g = 10)


The Hosmer-Lemeshow test is a statistical test for goodness of fit for logistic regression models.