# ex 16 priors for simple poisson regression?

# one could invent a story: 
# y is the number of customers per 10 minutes pushing towards
# the cash register in a supermarket. 
# x could be the time of day in hours, 2 = 2PM, ... 8 = 8 PM.
# in a world where people have suiting working hours, such a curve might occur.

exp(0.5)
# -> 1 hour later (ref 2 PM) would imply that the average number of customers
# per 10 min is larger by factor of 1.65, hence grows exponentially.

# a sales person could observe 2.5-fold increase with a rough 
# CI of 1.5 to 4.5, hence a 95% CI of (1.5, 4.5) for the factor.
# assuming this insecurity is symmetric, one could take a normal prior:
# beta_1 ~ Normal(2.5, 0.5). Market observations (cameras)
# could potentially deliver precise estimate for the actual rates.