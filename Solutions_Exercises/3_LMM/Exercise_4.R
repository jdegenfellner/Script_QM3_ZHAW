library(pacman)
p_load(lme4, rethinking)

# 1)--------------

d$tank <- 1:nrow(d)

dat <- list(S = d$surv,
            N = d$density,
            tank = d$tank)

# Frequentist version of 13.2---------
m_lmer <- glmer(
  cbind(S, N - S) ~ (1 | tank), # here, we have an overall intercept
  data = dat,
  family = binomial(link = "logit"),
  control = glmerControl(optimizer = "bobyqa")
)
summary(m_lmer)

fixed_intercept <- fixef(m_lmer)[1]
random_intercepts <- ranef(m_lmer)$tank[,1]

full_intercepts <- fixed_intercept + random_intercepts
probabilities <- plogis(full_intercepts)

frequentist_results <- data.frame(
  tank = rownames(ranef(m_lmer)$tank),
  surv_prob_frequentist = probabilities
)

# Bayesian version of 13.2---------
m13.2 <- ulam(
  alist(
    S ~ dbinom(N, p),
    logit(p) <- a[tank],
    a[tank] ~ dnorm(a_bar, sigma),
    a_bar ~ dnorm(0, 1.5),
    sigma ~ dexp(1)
  ) , data = dat, chains = 4, log_lik = TRUE)
df_13.2 <- precis(m13.2, depth = 2)
df_13.2$surv_prob_tanks <- rethinking::logistic(df_13.2$mean)



# Compare---------
compare_results <- data.frame(
  surv_prob_bayes = df_13.2$surv_prob_tanks[1:48],
  surv_prob_frequentist = frequentist_results$surv_prob_frequentist
)
compare_results
ggplot(compare_results, aes(x = surv_prob_bayes, 
                            y = surv_prob_frequentist)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  labs(x = "Bayesian survival probability (13.2)", 
       y = "Frequentist survival probability") +
  theme_minimal()
# -> very similar results

# 2)--------------
# Bayesian version of 13.1---------
m13.1 <- ulam(
  alist(
    S ~ dbinom(N, p),
    logit(p) <- a[tank],
    a[tank] ~ dnorm(0, 3)
  ) , data = dat, chains = 4, log_lik = TRUE)
df_13.1 <- precis(m13.1, depth = 2)
df_13.1$surv_prob_tanks <- rethinking::logistic(df_13.1$mean)
df_13.1

# Frequentist version of 13.1---------
dat$tank
glm_13.1 <- glm(
  cbind(S, N - S) ~ factor(tank) - 1, # we just need 48 intercepts, and no overall intercept
  data = dat,
  family = binomial(link = "logit")
)
summary(glm_13.1)
df_13.1_freq <- data.frame(surv_prob_freq_13_1 = inv_logit(coef(glm_13.1)))

# Compare
compare_results_13_1 <- data.frame(
  surv_prob_bayes = df_13.1$surv_prob_tanks,
  surv_prob_frequentist = df_13.1_freq$surv_prob_freq_13_1
)
compare_results_13_1
ggplot(compare_results_13_1, aes(x = surv_prob_bayes, y = surv_prob_frequentist)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  labs(x = "Bayesian survival probability (13.1)", y = "Frequentist survival probability") +
  theme_minimal()
# -> very similar results