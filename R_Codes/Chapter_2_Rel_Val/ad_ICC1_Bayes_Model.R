library(pacman)
p_load(tidyverse, readxl)

# Read file
url <- "https://raw.githubusercontent.com/jdegenfellner/Script_QM2_ZHAW/main/data/chapter%205_assignment%201_2_wide.xls"
temp_file <- tempfile(fileext = ".xls")
download.file(url, temp_file, mode = "wb")  # mode="wb" is important for binary files
df <- read_excel(temp_file)

head(df)


# As in the book, let's randomly select 50 patients.
set.seed(123)
df <- df %>% sample_n(50)
dim(df)


library(tictoc)

df_long <- df %>% 
  mutate(ID = row_number()) %>%
  dplyr::select(ID,ROMas.Peter, ROMas.Mary) %>% 
  pivot_longer(cols = c(ROMas.Peter, ROMas.Mary), 
               names_to = "Rater", values_to = "ROM") %>% 
  mutate(Rater = factor(Rater))

tic()
m5.1 <- ulam(
  alist(
    # Likelihood
    ROM ~ dnorm(mu, sigma),
    
    # Patient-specific intercepts (random effects)
    mu <- a[ID],  
    a[ID] ~ dnorm(mu_a, sigma_ID),  # Hierarchical structure for patients
    
    # Priors for hyperparameters
    mu_a ~ dnorm(66, 20),  # Population-level mean
    sigma_ID ~ dunif(0,20),  # Between-patient standard deviation
    sigma ~ dunif(0,20)  # Residual standard deviation
  ), 
  data = df_long, 
  chains = 8, cores = 4
)
toc()


precis(m5.1, depth = 2)


# PPC-----------
library(rethinking)
library(scales)
library(tidyverse)

# 1. Simuliere 100 Posterior-Prediktive
set.seed(123)
D_sim <- sim(m5.1, n = 100)  # 100 Simulationen → jede Spalte ist ein Sample

# 2. Dichten über die Spalten berechnen
densities <- apply(D_sim, 2, density)

# 3. Maximalhöhe für y-Achse finden
max_density <- max(sapply(densities, function(d) max(d$y)))

# 4. Leerer Plot
plot(NULL, xlim = range(df_long$ROM), ylim = c(0, max_density),
     xlab = "ROM", ylab = "Dichte",
     main = "Posterior Predictive Check")

# 5. Zeichne die simulierten Dichtekurven
for (s in 1:100) {
  lines(densities[[s]], col = alpha("gray", 0.3), lwd = 1)
}

# 6. Beobachtete Dichtekurve
obs_density <- density(df_long$ROM)
lines(obs_density$x, obs_density$y, col = "darkgreen", lwd = 2)

# 7. Legende
legend("topright", legend = c("Posterior Predictive Dichten", "Beobachtete Dichte"),
       col = c("gray", "darkgreen"), lty = 1, lwd = c(1, 2))


# OVERALL PPC - kann das Modell ROMS auf Populationsebene erklären?-----------


# 1. 100 neue Sets mit jeweils 50 ROM-Werten simulieren
set.seed(42)
sim_sets <- replicate(100, {
  mu_sample <- sample(post$mu, size = 100, replace = TRUE)  # ziehe zufällige mu
  sigma_sample <- sample(post$sigma, size = 1)  # eine sigma pro Simulation
  rnorm(50, mean = mu_sample, sd = sigma_sample)
}, simplify = FALSE)

# 2. Dichten berechnen
densities <- lapply(sim_sets, density)

# 3. Plot vorbereiten
max_y <- max(sapply(densities, function(d) max(d$y)))
plot(NULL, xlim = range(df_long$ROM), ylim = c(0, max_y),
     xlab = "ROM", ylab = "Dichte",
     main = "PPC: 50 neue ROMs vs. beobachtete 50 ROMs")

# 4. Simulierte Dichtekurven
for (d in densities) {
  lines(d, col = alpha("gray", 0.3))
}

# 5. Beobachtete ROMs (eine Dichte)
lines(density(df_long$ROM), col = "darkgreen", lwd = 2)

# 6. Legende
legend("topright", legend = c("Simulierte 50-ROM-Sätze", "Beobachtete 50 ROMs"),
       col = c("gray", "darkgreen"), lty = 1, lwd = c(1, 2))

