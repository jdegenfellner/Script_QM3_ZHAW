#install.packages("NHANES")
library(pacman)
p_load(NHANES, tidyverse, rethinking)
data(NHANES)
head(NHANES)
df <- NHANES
df$Age

# >=20 age:
df <- df %>% dplyr::filter(Age >= 20) %>%
  dplyr::select(Age, Gender, BPSysAve, PhysActive) %>%
  drop_na() # eliminate all obs with at least one missing
dim(df) # 6971    4
(Age_mean <- mean(df$Age, na.rm = TRUE)) # 47.22565 years

# calibrate parameters for priors.
hist(rlnorm(10000, meanlog = 4, sdlog = 0.5))

mean(rlnorm(10000, meanlog = 4, sdlog = 0.5)) 
exp(4 + 0.5^2 / 2) 



# fit improved model:
set.seed(122)
m_NHANES_lnorm <- quap(
  alist(
    BPSysAve ~ dlnorm(lmu, lsd), 
    lsd <- exp(beta_4 + beta_5 * lmu),
    lmu <- beta_0 + beta_1[PhysActive] + beta_2 * (Age - Age_mean) + beta_3[Gender],
    beta_0 ~ dnorm(4, 2),  # 
    beta_1[PhysActive] ~ dnorm(0, 10),  # 
    beta_2 ~ dnorm(0, 10),
    beta_3[Gender] ~ dnorm(0, 10),  # 
    beta_4 ~ dnorm(0, 10),  # 
    beta_5 ~ dnorm(0, 10)  #
  ),
  data = df,
  start = list(beta_0 = 4, beta_1 = c(0, 0),
               beta_2 = 0, beta_3 = c(0, 0),
               beta_4 = 0, beta_5 = 0)
)
precis(m_NHANES_lnorm, depth = 2)


# Posterior Samples ziehen
samples <- extract.samples(m_NHANES_lnorm)

# Mittelwerte μ berechnen
mu_active  <- samples$beta_0 + samples$beta_1[,2]
mu_inactive <- samples$beta_0 + samples$beta_1[,1]

# Berechne σ_i für beide Gruppen (kann ggf. clipped werden)
clip <- function(x, lower = -100, upper = 100) pmin(pmax(x, lower), upper)
sigma_active <- exp(clip(samples$beta_4 + samples$beta_5 * mu_active))
sigma_inactive <- exp(clip(samples$beta_4 + samples$beta_5 * mu_inactive))

# Erwartungswerte auf Originalskala
E_active <- exp(clip(mu_active + 0.5 * sigma_active^2))
E_inactive <- exp(clip(mu_inactive + 0.5 * sigma_inactive^2))

# Differenz im erwarteten Blutdruck (mmHg)
diff_expected_BP <- E_active - E_inactive

# Bereinigen (z. B. Extremwerte entfernen, falls nötig)
diff_expected_BP_clean <- diff_expected_BP[is.finite(diff_expected_BP) & abs(diff_expected_BP) < 20]

hist(diff_expected_BP_clean, breaks = 50)

# Zusammenfassen
precis(data.frame(diff_expected_BP_clean))
length(diff_expected_BP_clean) # 
