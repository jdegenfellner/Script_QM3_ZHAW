# Load necessary libraries
library(ggdag)
library(dagitty)

# Define the DAG
dag <- dagitty("dag {
  Smoking -> LungCancer
  Smoking -> Lighter
  Lighter -> LungCancer
}")

# Plot the DAG
ggdag(dag, layout = "circle") +
  theme_dag()

# Set seed for reproducibility
set.seed(123)

# Number of individuals
n <- 10000

# Simulate Smoking (1 = Smoker, 0 = Non-Smoker)
smoking <- rbinom(n, 1, 0.25)  # 25% smokers

# Simulate Carrying a Lighter (dependent on smoking)
lighter <- ifelse(smoking == 1, rbinom(n, 1, 0.95), rbinom(n, 1, 0.05))  # 95% of smokers, 5% of non-smokers

# Simulate Lung Cancer (dependent on smoking)
lung_cancer <- ifelse(smoking == 1, rbinom(n, 1, 0.20), rbinom(n, 1, 0.02))  # 20% of smokers, 2% of non-smokers

# Store in dataframe
df <- data.frame(smoking, lighter, lung_cancer)

# Check the data
table(df$smoking, df$lung_cancer)  # Cross-tab for lung cancer rates
table(df$lighter, df$lung_cancer)  # Cross-tab for lighters

# chisquare test for independence
chisq.test(table(df$smoking, df$lung_cancer))
chisq.test(table(df$lighter, df$lung_cancer))

# Logistic regression
model <- glm(lung_cancer ~ smoking + lighter, data = df, family = "binomial")
summary(model) # beta for lighter is now 0
