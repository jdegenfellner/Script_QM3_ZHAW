library(rethinking)
library(lme4)
data(reedfrogs)
d <- reedfrogs
str(d)

library(DiagrammeR)

grViz("
digraph hierarchical_model {
  
  graph [rankdir = TB, layout = dot]  // Top to Bottom
  
  node [shape = ellipse, style = filled, fillcolor = lightgray, fontsize = 12]
  
  bar_alpha [label = 'bar_alpha\n~ Normal(0, 1.5)']
  sigma [label = 'sigma\n~ Exponential(1)']
  alpha_j [label = 'alpha[j]\n~ Normal(bar_alpha, sigma)']
  pi [label = 'logit(p[i]) = alpha[TANK[i]]']
  Si [label = 'S[i]\n~ Binomial(N[i], p[i])']
  
  bar_alpha -> alpha_j
  sigma -> alpha_j
  alpha_j -> pi
  pi -> Si
}
")

library(rethinking)
d$tank <- 1:nrow(d)

dat <- list(S = d$surv,
            N = d$density,
            tank = d$tank)

set.seed(122)
m13.1 <- ulam(
  alist(
    S ~ dbinom(N, p),
    logit(p) <- a[tank],
    a[tank] ~ dnorm(0, 3)
  ) , data = dat, chains = 4, log_lik = TRUE)
df_13.1 <- precis(m13.1, depth = 2)
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
  ) , data = dat, chains = 4, log_lik = TRUE)

compare(m13.1, m13.2)
df_13.2 <- precis(m13.2, depth = 2)
df_13.2$surv_prob_tanks <- rethinking::logistic(df_13.2$mean)
df_13.2

# empirical survival probability
dat$empirical_surv_prob <- dat$S / dat$N

data.frame(surv_prob_13_1 = df_13.1$surv_prob_tanks,
           surv_prob_13_2 = df_13.2$surv_prob_tanks[1:48],
           empirical_surv_prob = dat$empirical_surv_prob)

df_compare <- data.frame(
  surv_prob_13_1 = df_13.1$surv_prob_tanks,
  surv_prob_13_2 = df_13.2$surv_prob_tanks[1:48],
  empirical_surv_prob = dat$empirical_surv_prob
)

ggplot(df_compare, aes(x = surv_prob_13_1, y = empirical_surv_prob)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  labs(x = "13.1 no pooling", y = "empirial surv probs") +
  theme_minimal()

ggplot(df_compare, aes(x = surv_prob_13_2, y = empirical_surv_prob)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  labs(x = "13.2 partial pooling", y = "empirial surv probs") +
  theme_minimal()


# Are the intercepts in model 13.2 correlated?
m13.2_sims <- extract.samples(m13.2, n = 1000)
m13.2_sims$a_bar
m13.2_sims$sigma
m13.2_sims$a

cov_a <- cov(m13.2_sims$a)
cor_a <- cov2cor(cov_a)
max(cor_a - diag(nrow(cor_a)))

# what about the intercepts in model 13.1?
m13.1_sims <- extract.samples(m13.1, n = 1000)
m13.1_sims$a
cov_a_13.1 <- cov(m13.1_sims$a)
cor_a_13.1 <- cov2cor(cov_a_13.1)
max(cor_a_13.1 - diag(nrow(cor_a_13.1)))

# plot posterior for a1 a2 from model 13.1
m13.1_sims <- extract.samples(m13.1, n = 1000)
a1 <- m13.1_sims$a[, 1]
a2 <- m13.1_sims$a[, 2]
plot(a1, a2, xlab = "a1", ylab = "a2")
cor(a1, a2) # 0.035

# plot posterior for a1 a2 from model 13.2
m13.2_sims <- extract.samples(m13.2, n = 1000)
a1 <- m13.2_sims$a[, 1]
a2 <- m13.2_sims$a[, 2]
plot(a1, a2, xlab = "a1", ylab = "a2")
cor(a1, a2) # 0.046

# -> still not correlated. The hyperpriors just shrink the estimates,
#    but do not force them to be correlated.


# Frequenstist version

# of 13.1 ?

# of 13.2.:
library(lme4)

m_lmer <- glmer(
  cbind(S, N - S) ~ (1 | tank),
  data = dat,
  family = binomial(link = "logit"),
  control = glmerControl(optimizer = "bobyqa")
)
summary(m_lmer)

fixed_intercept <- fixef(m_lmer)[1]
random_intercepts <- ranef(m_lmer)$tank[,1]

full_intercepts <- fixed_intercept + random_intercepts
probabilities <- plogis(full_intercepts)

result <- data.frame(
  tank = rownames(ranef(m_lmer)$tank),
  logit_p = full_intercepts,
  p = probabilities
)

cbind(result, df_compare)
