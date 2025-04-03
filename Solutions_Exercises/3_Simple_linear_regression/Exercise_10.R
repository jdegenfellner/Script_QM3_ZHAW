#ex 10
library(rethinking)
data(Howell1)
d <- Howell1
d2 <- d[d$age >= 18, ]

plot(d2$weight, d2$height, pch = 19, col = "blue", xlab = "Weight", ylab = "Height")

# 1) Regress height on weight----------
mod_hw <- lm(height ~ weight, data = d2)
summary(mod_hw)
# k_hw = 0.90503
1/0.90503 # 1.104936
# 113.87939 + 0.90503*weight = 0
-113.87939/0.90503
# -125.8294

# 2) Regress weight on height----------
mod_wh <- lm(weight ~ height, data = d2)
summary(mod_wh)
# k_wh = 0.62942

# draw both lines into one plot:
plot(d2$height, d2$weight, pch = 19, col = "blue", xlab = "Height", ylab = "Weight")
abline(mod_wh)
abline(-125.8294, 1.104936, col = "red")


# if cor=1, regression lines are identical

# depending which variable I want to predict or explain with which one,
# the analysis could be wrong.