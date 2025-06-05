# TODO...
library(conflicted)
library(rethinking)
library(tidyverse)
library(performance)
library(kableExtra)
library(gtsummary)
conflicts_prefer(posterior::sd)

# Create data -----------
set.seed(334)
AGE <- rnorm(100)
y <- rbinom(100, size = 1, 
            prob = inv_logit(AGE)) # the probability of a 1 increases with AGE
# since there is no intercept (beta_0 = 0), we implicitely assume
# that for AGE = 0 (average AGE), the probability of a 1 is: 
inv_logit(0) # 0.5
# the logistic curve is point-symmetric in the plot around the point: (0,0.5)




# Estimate logistic regression model----------
modlog <- glm(y ~ AGE, family = binomial(link = "logit"))
summary(modlog)

exp(coef(modlog))  # Exponentiated coefficients)
exp(confint(modlog))
df <- data.frame(coefs = coef(modlog), 
           exp_coefs = exp(coef(modlog)), 
           confint_lower = exp(confint(modlog)[, 1]), 
           confint_upper = exp(confint(modlog)[, 2]))
flextable(df) # there are many different options to display the results
kable(df)
tbl_regression(modlog)
tbl_regression(modlog, 
               exponentiate = TRUE) %>%
  as_kable() %>%
  kable_styling(full_width = FALSE)
# -> NEVER copy paste the regression results from R, but create (as much as possible)
# publication-ready tables.


# Visualize---------
predicted_probabilities <- predict(modlog, type = "response")
plot(AGE, y, main = "inv_logit Function", 
     xlab = "x", ylab = "inv_logit(AGE)", col = "blue", pch = 19)
points(AGE, predicted_probabilities, col = "red", pch = 19)

# We can explicitely give the model equation for the red line
beta_0 = coef(modlog)[1]
beta_1 = coef(modlog)[2]

# inv_logit:
inv_logit <- function(x) {
  exp(x) / (1 + exp(x))
}
plot(AGE, y, main = "inv_logit Function", 
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

# now with ggplot and a smoothing line through the orange points
ggplot(df_summary, aes(x = midpoint, y = mean_y)) +
  geom_point(color = "orange", size = 3) +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(title = "Inverse Logit Function with Binned Proportions",
       x = "AGE (midpoint of bins)",
       y = "Proportion of 1s") +
  theme_minimal()
# -> would probabl<- look smoother with more data points


# Check_model output-------
?check_model
check_model(modlog)
# linearity plot seems to be missing

# 1) PPC---------
samples_modlog <- extract.samples(modlog, n=100) # this also works for Frequentist models
str(samples_modlog) # 
hist(samples_modlog$Intercept)
hist(samples_modlog$AGE) 
# -> both revolve around the model estimated best guess...

# we want the probabilties:
predicted_probabilities <- inv_logit(samples_modlog$Intercept + 
                                      samples_modlog$AGE * AGE)

# use all these predicted probabilities to draw one number from a bernoulli distribution
predicted_y <- rbinom(n = length(predicted_probabilities), 
                      size = 1, 
                      prob = predicted_probabilities)
# plot to confirm the check_model output:
(obs_prop_1 <- sum(y) / length(y))
(obs_prop_0 <- 1 - obs_prop_1)

(simulated_prop_1 <- sum(predicted_y) / length(predicted_y))
(simulated_prop_0 <- 1 - simulated_prop_1)

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
#install.packages("ResourceSelection")
library(ResourceSelection)
# modlog: dein logistisches Modell
# g=10 → Gruppiere nach Dezilen des vorhergesagten Risikos
hoslem.test(y, 
            fitted(modlog), 
            g = 10)
# from the authors of "Applied Logistic Regression" (Hosmer, Lemeshow, Sturdivant)
# ok

