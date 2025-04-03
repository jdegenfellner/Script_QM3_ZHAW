library(ggplot2)

set.seed(42)
N <- 1000  # Large sample

# Generate independent variables W, X, Y
W <- rnorm(N, mean = 100, sd = 15)  # GPA
X <- rnorm(N, mean = 100, sd = 15)  # Math Score
Y <- rnorm(N, mean = 100, sd = 15)  # Verbal Score

# Define Z as a sum score
Z <- W + X + Y + rnorm(N, sd = 10)  # Some noise

# Select only students above a threshold Z
admitted <- Z > quantile(Z, 0.8)  # Top 20% get admitted

# Create a data frame
df <- data.frame(W, X, Y, Z, admitted)

# Compute correlation in full population
cor_full <- cor(df[, c("W", "X", "Y")])

# Compute correlation only in admitted students
cor_admitted <- cor(df[df$admitted, c("W", "X", "Y")])

# Print correlation matrices
cat("\nCorrelation in full population:\n")
print(cor_full)

cat("\nCorrelation among admitted students (Berkson's Paradox):\n")
print(cor_admitted)
# -> seems to be true for 3 variables as well

ggplot(df, aes(x = X, y = Y, color = as.factor(admitted))) +
  geom_point(alpha = 0.5) +  # Scatterplot
  geom_smooth(method = "lm", se = FALSE) +  # Separate trendlines
  labs(title = "Berkson's Paradox: Artificial Negative Correlation",
       x = "Math Score (X)", y = "Verbal Score (Y)",
       color = "Admitted") +
  scale_color_manual(values = c("red", "blue"), labels = c("Not Admitted", "Admitted")) +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))
  
