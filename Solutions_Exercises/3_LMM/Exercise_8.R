# ex 8 LLM

# Try to find the method for sample size calculation in 
# the referenced literature literature.

# In the text, there is no source to a formula, webapp, or package 
# which was used to get to the sample size.

# "The sample size was calculated based on the primary outcome 
# of headache frequency. According to the International 
# Headache Society guidelines (2018),3 a 50% reduction in 
# headache frequency is considered to be a clinically worthwhile effect.
# We anticipated that the participants would have a headache frequency
# of 4 days/month, so a 50% reduction would equate to a between-group
# difference of 2 days/month. A sample size calculation with power of 0.8,
# an alpha of 0.05, 2 days/month as the effect size sought, and an assumed
# standard deviation of 2.4 indicated 24 participants per group.
# To account for the multiple comparisons in this three-arm trial
# and to allow for 10% loss to follow-up, this was increased to 33/group
# for a total of 99 participants."

# It just says "A sample size calculation" without a reference???


install.packages("pwrss")
library(pwrss)

# Beispiel: mittlerer Effekt (η² = 0.06), 4 Messzeitpunkte
pwrss.f.rmanova(eta2 = 0.25, 
                n.levels = 3, # For example, for randomized controlled trials with two arms (treatment/control) it takes a value of 2
                n.rm = 4, # number of measurements. For example, for pretest/posttest designs it takes a value of 2.
                power = 0.80, 
                alpha = 0.05, 
                corr.rm = 0.10, 
                type = "within")

# playing around with the parameters could give the sample size stated in the paper:
3*24 # 72

eta2 = 0.06
# f2 = 
eta2 / (1 - eta2)

R_squared <- 0.2
# f2 = 
R_squared / (1 - R_squared)


# "2 days/month as the effect size" does this make sense?
