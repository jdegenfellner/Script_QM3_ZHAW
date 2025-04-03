# mean
heights_CH_mean <-  171.1 # cm
n_CH <-  21873 # sample size

# ZGV:
# CI for the true, but unknown mean of the population is:
# \bar{x} \pm 1.96 * \frac{s}{\sqrt{n}}

# width CI:
c(-0.002*171.1,0.002*171.1) # 0.2%

# CI (Vertrauensinterval) = 
heights_CH_mean + c(-0.002*171.1,0.002*171.1)
#170.7578 171.4422

# CI was created with the following formula:
# mean(heights_CH) + c(-1.96,1.96) * sd(heights_CH) / sqrt(n_CH)
# the last part = 0.002

# Hence, sd(heights_CH):
# 0.002*171.1 = 1.96 *sd/sqrt(n_CH)

# sd = 
0.002*heights_CH_mean * sqrt(n_CH)/1.96
# 25.8213
