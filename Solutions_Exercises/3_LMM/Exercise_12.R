# TODO.......
# ex 12 simple log reg

# how do the odds for myocardial infarction change with age?

# lets try this as prior knowledge (ballpark figures):
# https://www.sciencedirect.com/science/article/pii/S0167527322019143
# -> Table 2 "Meta regression analysis for the risk of acute myocardial
# infarction after COVID-19 infection. CI: Confidence interval."

# https://stats.stackexchange.com/questions/130237/convert-hazards-ratio-to-odds-ratio

# "If there was an extremely low proportion of subjects
# with an event in all experiments (let's say <10%)
# and the hazard and odds ratios are vey close to 1,
# then hazard, odds and relative risk ratios will be
# relatively close to each other."

HR <- seq(0.3, 5, by = 0.1)
r <- 0.05 # low prevalence in reference group

RR <- (1-exp(HR*log(1-r)))/r

data.frame(HR, RR) %>%
  ggplot(aes(x = HR, y = RR)) +
  geom_line() + 
  geom_abline(slope = 1, 
              intercept = 0, 
              linetype = "dashed", 
              color = "red")
# -> in a certain range HR and RR are very similar
# OR and RR: RR is closer to 1 typically.

# According to the paper, the coefficient for AGE is 0.008;	95% CI: 0.06 to 0.013
# Hence the RR (~OR?) changes by a factor of exp(0.008) per year of age.
exp(0.008)
exp(0.008*10) # 1.08 per 10 years
exp(c(0.006, 0.013)) # 1.006 and 1.013

# a SD in our AGE data could be something like 15 years.
# -> per SD the OR changes by a factor of approximately
exp(0.008*15)

# priors:---------

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

# e^beta_1 = exp(0.008*15)
# -> beta_1 = 0.008 * 15
c(0.006, 0.013)*15

# -> 0 as mean is not that far off for the mean
# -> one could possible choose a much narrower prior?