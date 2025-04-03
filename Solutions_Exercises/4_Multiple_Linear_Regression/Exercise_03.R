# 4 Beobachtungen, 3 Prädiktoren
df <- data.frame(
  X1 = c(1, 2, 3, 4),
  X2 = c(2, 4, 6, 8),
  X3 = c(3, 6, 9, 12)
)

# Y als perfekte lineare Kombination der Prädiktoren (z. B. Y = 1 + X1 + 2*X2 - 0.5*X3)
df$Y <- 1 + df$X1 + 2 * df$X2 - 0.5 * df$X3

# Modell fitten
model <- lm(Y ~ X1 + X2 + X3, data = df)

# Zusammenfassung
summary(model)

# R² anzeigen
summary(model)$r.squared
# Warning message:
# In summary.lm(model) : essentially perfect fit: summary may be unreliable
