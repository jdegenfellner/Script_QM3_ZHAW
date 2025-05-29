library(conflicted)
library(rethinking)
library(tidyverse)
conflicts_prefer(posterior::sd)

# Create data -----------
set.seed(334)
AGE <- rnorm(100)
y <- rbinom(100, size = 1, prob = inv_logit(AGE)) # the probability of a 1 increases with AGE

plot(AGE, y, main = "Inverse Logit Function", 
     xlab = "x", ylab = "inv_logit(AGE)", col = "blue", pch = 19)
abline(lm(y ~ AGE), col = "red", lwd = 2)

# Estimate logistic regression model----------
modlog <- glm(y ~ AGE, family = binomial(link = "logit"))
summary(modlog)

exp(coef(modlog))  # Exponentiated coefficients)
exp(confint(modlog))
data.frame(coefs = coef(modlog), 
           exp_coefs = exp(coef(modlog)), 
           confint_lower = exp(confint(modlog)[, 1]), 
           confint_upper = exp(confint(modlog)[, 2]))

# Vizualize---------
predicted_probabilities <- predict(modlog, type = "response")
plot(AGE, y, main = "Inverse Logit Function", 
     xlab = "x", ylab = "inv_logit(AGE)", col = "blue", pch = 19)
# add predicted probabilities
points(AGE, predicted_probabilities, col = "red", pch = 19)

# We can explicitely give the model equation for the red line
beta_0 = coef(modlog)[1]
beta_1 = coef(modlog)[2]

# inv_logit:
inv_logit <- function(x) {
  exp(x) / (1 + exp(x))
}
plot(AGE, y, main = "Inverse Logit Function", 
     xlab = "x", ylab = "inv_logit(AGE)", col = "blue", pch = 19)
# add predicted probabilities
curve(inv_logit(beta_0 + beta_1 * x), 
      add = TRUE, col = "red", lwd = 2)
points(AGE, predicted_probabilities, col = "green", pch = 19)
# perfect match!

# One could also estimate this curve by looking 
# at the proportions of 1s in bins of AGE:
df <- data.frame(AGE = AGE, y = y)
breaks <- pretty(df$AGE, n = 10)
df$AGE_bin <- cut(df$AGE, breaks = breaks, include.lowest = TRUE)

bin_midpoints <- data.frame(
  AGE_bin = levels(df$AGE_bin),
  midpoint = (head(breaks, -1) + tail(breaks, -1)) / 2
)

df_summary <- df %>%
  dplyr::group_by(AGE_bin) %>%
  dplyr::summarise(mean_y = mean(y), count = n(), .groups = "drop") %>%
  left_join(bin_midpoints, by = "AGE_bin")

df_summary

plot(AGE, y, main = "Inverse Logit Function", 
     xlab = "x", ylab = "inv_logit(AGE)", col = "blue", pch = 19)
points(df_summary$midpoint, df_summary$mean_y, 
       col = "orange", pch = 19, cex = 1.5)

library(ggeffects)
ggeffect(modlog, 
         terms = "AGE", 
         type = "response") %>%
  plot() +
  labs(title = "Predicted Probabilities from Simple Logistic Regression",
       x = "Age",
       y = "Predicted Probability of 1") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))


# Check_model-------
?check_model
check_model(modlog)

# 1) PPC---------

set.seed(123)
sim_props <- replicate(1000, {
  # Ziehe jedes Mal 1 Sample aus der Posteriorverteilung
  s <- extract.samples(modlog, n = 1)
  predicted_probabilities <- inv_logit(s$Intercept + s$AGE * AGE)
  
  # Ziehe ein neues y_sample basierend auf den Wahrscheinlichkeiten
  predicted_y <- rbinom(n = length(predicted_probabilities), size = 1, prob = predicted_probabilities)
  
  # Speichere Anteil 0 und 1
  c(prop_0 = mean(predicted_y == 0), prop_1 = mean(predicted_y == 1))
})

# Transponiere Matrix für besseren Zugriff
sim_props <- t(sim_props)

# Zugriff auf die Anteile
simulated_prop_0_vec <- sim_props[, "prop_0"]
simulated_prop_1_vec <- sim_props[, "prop_1"]

# Berechne 95%-Quantile (analog check_model Balken)
quantile(simulated_prop_1_vec, probs = c(0.025, 0.975))
quantile(simulated_prop_0_vec, probs = c(0.025, 0.975))

# Optional: Visualisierung als Histogramm
hist(simulated_prop_1_vec, main = "Distribution of Simulated Proportion 1s",
     xlab = "Proportion of 1s", col = "lightblue", border = "white")
abline(v = mean(y), col = "red", lwd = 2, lty = 2) # beobachtete Prop. 1


# visualize:


# 2) Binned residuals----------
check_model(modlog, check = "binned_residuals",
            residual_type = "normal")

# a)
res <- binned_residuals(modlog, 
                        residuals = "response")
as.data.frame(res)
plot(res)

# b)
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
# details:
# In logistic regression, as with linear regression,
# the residuals can be defined as observed minus
# expected values. The data are discrete and so are
# the residuals. As a result, plots of raw residuals
# from logistic regression are generally not useful.
# The binned residuals plot instead, after dividing
# the data into categories (bins) based on their
# fitted values, plots the average residual versus
# the average fitted value for each bin.

# c) binred_plot
library(stevemisc)
?binred_plot
binred_plot(modlog, nbins = 10, plot = TRUE)

# 3) Influential points----------
?check_model
check_model(modlog, check = "outliers")

hatvalues(modlog)
cooks.distance(modlog)
boxplot(residuals(modlog, type = "deviance"))


# 4) QQ-Plot of residuals----------
check_model(modlog, check = "qq")



plot(modlog)



# install if needed
install.packages("ResourceSelection")

library(ResourceSelection)
# modlog: dein logistisches Modell
# g=10 → Gruppiere nach Dezilen des vorhergesagten Risikos
hoslem.test(y, 
            fitted(modlog), 
            g = 10)
# ok

