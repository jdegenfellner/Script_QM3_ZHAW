# set wd to source file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(pacman)
p_load(
  tidyverse,
  rethinking,
  conflicted
)
data(reedfrogs)
d <- reedfrogs
str(d)

d$tank <- 1:nrow(d)

dat <- list(S = d$surv,
            N = d$density,
            tank = d$tank)

# 0) empirical survival probability-----------
dat$empirical_surv_prob <- dat$S / dat$N
#all.equal(round(d$propsurv - dat$empirical_surv_prob, 3),rep(0,48)) # all are 0, same

set.seed(122)
m13.1 <- ulam(
  alist(
    S ~ dbinom(N, p),
    logit(p) <- a[tank],
    a[tank] ~ dnorm(0, 1.5)
  ) , data = dat, 
  chains = 4, 
  log_lik = TRUE,
  cores = detectCores()-1)
# 0.3s execution time
df_13.1 <- precis(m13.1, depth = 2)
# 1) Calculate the mean survival probability for each tank in model 13.1 -----------
df_13.1$surv_prob_tanks <- rethinking::logistic(df_13.1$mean)
df_13.1

conflicts_prefer(posterior::var)
m13.2 <- ulam(
  alist(
    S ~ dbinom(N, p),
    logit(p) <- a[tank],
    a[tank] ~ dnorm(a_bar, sigma),
    a_bar ~ dnorm(0, 1.5),
    sigma ~ dexp(1)
  ) , data = dat, 
  chains = 4, 
  log_lik = TRUE,
  cores = detectCores()-1)
# 0.2s execution time
#compare(m13.1, m13.2)
df_13.2 <- precis(m13.2, depth = 2)

# 2) Calculate the mean survival probability for each tank in model 13.2 -----------
df_13.2$surv_prob_tanks <- rethinking::logistic(df_13.2$mean)
df_13.2

data.frame(surv_prob_13_1 = df_13.1$surv_prob_tanks,
           surv_prob_13_2 = df_13.2$surv_prob_tanks[1:48],
           empirical_surv_prob = dat$empirical_surv_prob)

# Compare to Bayesian models:
df_compare <- data.frame(
  surv_prob_13_1 = df_13.1$surv_prob_tanks,
  surv_prob_13_2 = df_13.2$surv_prob_tanks[1:48],
  empirical_surv_prob = dat$empirical_surv_prob
)

# 3) Plot the mean survival probability for each tank from model 13.1 against the raw survival probability-----------
ggplot(df_compare, aes(x = surv_prob_13_1, y = empirical_surv_prob)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  labs(x = "13.1 no pooling", y = "empirial surv probs") +
  theme_minimal()
# -> very similar to raw survival probabilities!
# for large probabilities, the raw values are larger.

# 4) Do the same for model 13.2-------------
ggplot(df_compare, aes(x = surv_prob_13_2, y = empirical_surv_prob)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  labs(x = "13.2 partial pooling", y = "empirial surv probs") +
  theme_minimal()
# -> differences are visible now
# small raw probabilities are drawn upwards towards the overall mean (become larger)
# large raw probabilities are drawn downwards towards the overall mean (become smaller)


# 5) change prior in 13.1.------------
df_collect <- data.frame(raw_surv_prob = dat$empirical_surv_prob)
#var_vec_a <- c(0.5, 1, 1.5, 2, 3)
# ugly manually, since ulam does not eat the for-loop:
m13.1 <- ulam(
    alist(
      S ~ dbinom(N, p),
      logit(p) <- a[tank],
      a[tank] ~ dnorm(0, 3) # change manually here...
    ) , data = dat, 
    chains = 4, 
    log_lik = TRUE,
    cores = detectCores()-1)
df <- precis(m13.1, depth = 2)
df$surv_prob_tanks <- rethinking::logistic(df$mean)

df_collect <- cbind(df_collect, df$surv_prob_tanks)
colnames(df_collect) <- c("raw_surv_prob", "var05", "var1", "var15", "var2", "var3")

#saveRDS(df_collect, file = "df_collect.RDS")
df_collect <- readRDS("df_collect.RDS")

df_long <- df_collect %>%
  pivot_longer(
    cols = starts_with("var"),
    names_to = "var",
    values_to = "surv_prob_tank"
  )

ggplot(df_long, aes(x = raw_surv_prob, y = surv_prob_tank, color = var)) +
  geom_point() +
  theme_minimal() +
  labs(
    x = "Raw survival probability",
    y = "Estimated survival probability per tank",
    color = "Prior variance"
  ) + 
  geom_abline(intercept = 0, slope = 1, linetype = "dashed")

# -> the larger the variance, the less the estimates 
# are drawn towards the overall mean (48 points are almost at the identical line).
# The model with var=1.5 from the book is not far off the diagonal either.
# If we make the variance (resp \sigma)  of the random intercepts smaller,
# we allow less variation for the intercepts, and therefore
# we get more shrinkage!

# 6) change priors in 13.2.------------
df_collect_13.2 <- data.frame(raw_surv_prob = dat$empirical_surv_prob)

# __change only the prior for sigma-----------
# param (lambda) for dexp: 0.5, 1, 2, 3, 10, 100
# Note that the expectations for the distributions are 
# 1/0.5, 1/1, 1/2, 1/3, 1/10, 1/100
# https://de.wikipedia.org/wiki/Exponentialverteilung#Eigenschaften
# repeat this manually to get the data frame.
m13.2 <- ulam(
  alist(
    S ~ dbinom(N, p),
    logit(p) <- a[tank],
    a[tank] ~ dnorm(a_bar, sigma),
    a_bar ~ dnorm(0, 1.5), # leave as is.
    sigma ~ dexp(100) # change manually here...
  ) , data = dat, 
  chains = 4, 
  log_lik = TRUE,
  cores = detectCores()-1)

# Interpretation of priors:
# a_bar
inv_logit(0 + c(-1,1)*2*1.5)
# a priori a_bar is somewhere between p = 0.04742587 and 0.95257413, which is a very wide range
# a priori the 48 intercepts come from a normal distribution with expected sigma of 1/100, so very small,
# with such a small sigma, most a[tank] should be (a priori) around a_bar.
# reducing the expected value of sigma to essentially zero, should result in the simple overall mean model.
# since a bar has a very wide range, the data can influence the overall intercept a_bar.
# we could make this influence smaller by restricting the prior of a_bar.


df <- precis(m13.2, depth = 2)
inv_logit(1.09) # a bar -> 0.7483 
df_collect_13.2 <- cbind(df_collect_13.2, 
                         rethinking::logistic(df$mean)[1:48])
head(df_collect_13.2)
df_collect_13.2 <- readRDS("df_collect_13.2_change_sigma_prior.RDS")
colnames(df_collect_13.2) <- c("raw_surv_prob", "dexp05", 
                               "dexp1", "dexp2", "dexp3", "dexp10", "dexp100")
# -> does not change much.

sapply(df_collect_13.2, function(x) var(x, na.rm = TRUE))
# variance of probabilities gets smaller with smaller sigma as expected.

# plot against raw probabilites
df_long_13.2 <- df_collect_13.2 %>%
  pivot_longer(
    cols = starts_with("dexp"),
    names_to = "prior",
    values_to = "surv_prob_tank"
  )

ggplot(df_long_13.2, aes(x = raw_surv_prob, y = surv_prob_tank, color = prior)) +
  geom_point() +
  theme_minimal() +
  labs(
    x = "Raw survival probability",
    y = "Estimated survival probability per tank",
    color = "Prior variance"
  ) + 
  geom_abline(intercept = 0, slope = 1, linetype = "dashed")

#saveRDS(df_collect_13.2, file = "df_collect_13.2_change_sigma_prior.RDS")

# as you can see in the plot, dexp100 means that we have very strong
# shrinkage, estimates are drawn to the overall mean.

# posterior mean of sigma is 0.69, i.e. the model updated its belief towards
# a much higher value compared to our prior about sigma with an expected value of 0.01

# posterior mean of a_bar is also notably different compared to the intial model 13.2
# (0.42 vs. 1.34)

# __high variance for a_bar, high variance for sigma--------
m13.2 <- ulam(
  alist(
    S ~ dbinom(N, p),
    logit(p) <- a[tank],
    a[tank] ~ dnorm(a_bar, sigma),
    a_bar ~ dnorm(0, 10),
    sigma ~ dexp(0.1)
  ) , data = dat, 
  chains = 4, 
  log_lik = TRUE,
  cores = detectCores()-1)
df <- precis(m13.2, depth = 2)
data.frame(
  raw_surv_prob = dat$empirical_surv_prob,
  surv_prob_tanks = rethinking::logistic(df$mean)[1:48]
) %>%
ggplot(aes(x = surv_prob_tanks, y = raw_surv_prob)) +
  geom_point() +
  theme_minimal() +
  labs(
    y = "Raw survival probability",
    x = "13.2 model-Estimated survival probability"
  ) + 
  geom_abline(intercept = 0, slope = 1, linetype = "dashed")
# -> high raw probabilities are drawn downwards towards inv_logit(a_bar=1.41)=0.8
# low raw probabilities are drawn upwards towards inv_logit(a_bar=1.41)=0.8
# there is no large restriction on the variability of the intercepts, hence they move a lot.
# shrinkage is small.

# __low variance for a_bar, low variance for sigma--------
m13.2 <- ulam(
  alist(
    S ~ dbinom(N, p),
    logit(p) <- a[tank],
    a[tank] ~ dnorm(a_bar, sigma),
    a_bar ~ dnorm(0, 0.1), # overall probability is not very flexible, we are rather sure about it a priori to be p=0.5
    sigma ~ dexp(100) # very small variance for the intercepts, expected variance: 1/100
  ) , data = dat, 
  chains = 4, 
  log_lik = TRUE,
  cores = detectCores()-1)
df <- precis(m13.2, depth = 2)
data.frame(
  raw_surv_prob = dat$empirical_surv_prob,
  surv_prob_tanks = rethinking::logistic(df$mean)[1:48]
) %>% 
ggplot(aes(x = surv_prob_tanks, y = raw_surv_prob)) +
  geom_point() +
  theme_minimal() +
  labs(
    y = "Raw survival probability",
    x = "13.2 model-Estimated survival probability"
  ) + 
  geom_abline(intercept = 0, slope = 1, linetype = "dashed")
# a priori we are pretty sure that the overall intercept should be around 0.5
# (data shows it should be higher) with not much room to move since sigma is very small.
# -> hence high/low raw probabilities are drawn to inv_logit(a_bar=0.42)=0.6
