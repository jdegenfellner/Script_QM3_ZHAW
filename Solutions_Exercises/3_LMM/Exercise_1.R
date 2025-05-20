# Exercise 1.R
library(pacman)
p_load(tidyverse, lme4, rethinking)

data(reedfrogs)
d <- reedfrogs
str(d)
# 'data.frame':	48 obs. of  6 variables:
#   $ density : int  10 10 10 10 10 10 10 10 10 10 ...
# $ pred    : Factor w/ 2 levels "no","pred": 1 1 1 1 1 1 1 1 2 2 ...
# $ size    : Factor w/ 2 levels "big","small": 1 1 1 1 2 2 2 2 1 1 ...
# $ surv    : int  9 10 7 10 9 9 10 9 4 9 ...
# $ propsurv: num  0.9 1 0.7 1 0.9 0.9 1 0.9 0.4 0.9 ...
# $ tank    : int  1 2 3 4 5 6 7 8 9 10 ...

# Explanation of variables:
# density: initial tadpole density (number of tadpoles in a 1.2 x 0.8 x 0.4 m tank) [experiment 1]
# pred factor: predators present or absent [experiment 1]
# size factor: big or small tadpoles [experiment 1]
# surv: number surviving
# propsurv: proportion surviving (=surv/density) [experiment 1]

# Complete pooling:
sum(d$surv) / sum(d$density) # 0.699

# 1) Descriptive statistics--------
summary(d)
table(d$pred)
table(d$size)
table(d$density)
summary(d$surv)
summary(d$propsurv)

d %>%
  dplyr::group_by(pred, size) %>%
  dplyr::summarise(
    n = n(),
    mean_density = mean(density),
    mean_surv = mean(surv),
    mean_propsurv = mean(propsurv)
  )
# equal number of tanks for all combinations.

# 2) Create individual-level data: one row per tadpole-------
d_individual <- d %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    tadpole_id = list(1:density),
    survived = list(c(rep(1, surv), rep(0, density - surv)))
  ) %>%
  tidyr::unnest(cols = c(tadpole_id, survived)) %>%
  dplyr::mutate(
    tank = row_number()  # create tank ID for clarity if needed
  )

head(d_individual)
dim(d_individual) # 1120
sum(d$density) # 1120 -> every tadpole has its own row now
