# ex 13

library(rethinking)
library(conflicted)
library(car)
conflicts_prefer(stats::sd)
data(Howell1)
d <- Howell1
d2 <- d[ d$age >= 18 , ]

mod <- lm( height ~ weight , data = d2 )

# SST = total variability
sum( (d2$height - mean(d2$height))^2 ) # 21040.24

# SSR = explained by regression
sum( (predict(mod) - mean(d2$height))^2 ) # 11985.45

# SSE = unexpected variability
sum( residuals(mod)^2 ) # 9054.786

# Do they add up?
11985.45 + 9054.786 # 21040.24 -> YES

11985.45/(11985.45 + 9054.786) # = R^2

11985.45/21040.24

1 - 9054.786/21040.24

# How many degrees of freedom does each of the 3 terms have?

# SST: n - 1 = 352 - 1 = 351
# We can vary all but the last (for instance) quadratic term, because if
# we have estimated the mean and know all other 351 terms, the last
# one can be calculated!

# SSE: n - 2 = 352 - 2 = 350

# SSR: 1 degree of freedom, difference between the two.

# (n-2) + 1 = n - 1


