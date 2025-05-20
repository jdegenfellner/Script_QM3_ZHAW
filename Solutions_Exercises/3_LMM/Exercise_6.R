library(rethinking)
library(lme4)
library(conflicted)
conflicts_prefer(posterior::sd)
data(reedfrogs)
d <- reedfrogs
str(d)

d$tank <- 1:nrow(d)

dat <- list(S = d$surv,
            N = d$density,
            tank = d$tank)

# approximate posterior
set.seed(122)
m_compl_pool <- ulam(
  alist(
    S ~ dbinom(N, p),
    logit(p) <- a, # one intercept
    a ~ dnorm(0, 1.5) # a priori probability of survival: 0.5
  ) , data = dat, 
  chains = 4, 
  log_lik = TRUE,
  cores = detectCores() - 1)

precis(m_compl_pool)
rethinking::logistic(0.84)
# 0.6984652

# raw survival probability
sum(dat$S) / sum(dat$N)
# 0.6991071

# nice
