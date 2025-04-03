library(gridExtra)
library(ggplot2)
library(png)
library(grid)

# Load images (ensure they are in the "images/" subfolder)
img1 <- rasterGrob(readPNG("images/Cover_Stat_Reth.png"), interpolate=TRUE)
img2 <- rasterGrob(readPNG("images/Cover_Westfall.png"), interpolate=TRUE)
img3 <- rasterGrob(readPNG("images/Cover_Gelman.png"), interpolate=TRUE)
img4 <- rasterGrob(readPNG("images/Cover_Kruschke.png"), interpolate=TRUE)

# Create plots with labels
p1 <- ggplot() + annotation_custom(img1) + ggtitle("Statistical Rethinking") + theme_void()
p2 <- ggplot() + annotation_custom(img2) + ggtitle("Understanding Regression Analysis") + theme_void()
p3 <- ggplot() + annotation_custom(img3) + ggtitle("Data Analysis Using Regression") + theme_void()
p4 <- ggplot() + annotation_custom(img4) + ggtitle("Doing Bayesian Data Analysis") + theme_void()

# Arrange in 2x2 grid
grid.arrange(p1, p2, p3, p4, ncol=2)