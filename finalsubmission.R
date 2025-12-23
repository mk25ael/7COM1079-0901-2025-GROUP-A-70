############################################################
# Project: Earthquakes in Japan (2001–2018)
# Dataset ID: DS182
# Research Question:
#   Is there a correlation between earthquake depth (km)
#   and earthquake magnitude (mag) in Japan (2001–2018)?
#
# This script:
#   1. Loads the earthquake dataset
#   2. Prepares variables (depth, mag)
#   3. Runs a Spearman correlation test
#   4. Creates two plots:
#        - Histogram of magnitude with normal curve
#        - Scatterplot of depth vs magnitude with smooth curve
############################################################

# --- 1. Load packages -------------------------------------

library(readr)     # for read_csv()
library(dplyr)     # for data wrangling
library(ggplot2)   # for plots
library(tidyr)
# --- 2. Load data -----------------------------------------
# Make sure the CSV file is in the same folder as this script,
# or change the path below to where the file is stored.

quakes <- read.csv("C:\\Users\\jeeta\\Desktop\\HERTFORDSHIRE UNIVERSITY\\team research and development\\archive (3)\\Japan earthquakes 2001 - 2018.csv")

# --- 3. Select variables and handle missing values --------

quakes_sub <- quakes %>%
  select(depth, mag) %>%
  drop_na()

# Optional basic checks
summary(quakes_sub)
nrow(quakes_sub)

# --- 4. Spearman correlation test -------------------------
# H0: There is no correlation between depth and magnitude.
# H1: There is a correlation between depth and magnitude.

cor_result <- cor.test(
  quakes_sub$depth,
  quakes_sub$mag,
  method = "spearman",
  exact  = FALSE
)

print(cor_result)

# You can also save key results for reporting:
rho_value  <- unname(cor_result$estimate)
p_value    <- cor_result$p.value
sample_n   <- cor_result$parameter + 2  # n used in test

cat("\nSpearman rho:", round(rho_value, 4), "\n")
cat("p-value:", format(p_value, digits = 4), "\n")
cat("Sample size (approx.):", sample_n, "\n")

# --- 5. Visualise magnitude distribution ------------------

mag_mean <- mean(quakes_sub$mag)
mag_sd   <- sd(quakes_sub$mag)

p_mag_hist <- ggplot(quakes_sub, aes(x = mag)) +
  geom_histogram(aes(y = ..density..),
                 bins  = 30,
                 colour = "black",
                 fill   = "lightblue") +
  stat_function(fun = dnorm,
                args = list(mean = mag_mean, sd = mag_sd),
                colour   = "red",
                linewidth = 1) +
  labs(
    title = "Histogram of earthquake magnitude (Japan, 2001–2018)",
    x     = "Magnitude",
    y     = "Density"
  ) +
  theme_minimal()

print(p_mag_hist)

# --- 6. Visualise depth vs magnitude ----------------------

p_depth_mag <- ggplot(quakes_sub, aes(x = depth, y = mag)) +
  geom_point(alpha = 0.3, size = 1, colour = "blue") +
  geom_smooth(method = "loess", se = FALSE, colour = "red") +
  labs(
    title = "Earthquake depth vs magnitude (Japan, 2001–2018)",
    x     = "Depth (km)",
    y     = "Magnitude"
  ) +
  theme_minimal()

print(p_depth_mag)

# --- 7. Save outputs (optional) ---------------------------
# Uncomment if you want to save plots to files for your GitHub repo.

# ggsave("fig_magnitude_histogram.png", p_mag_hist, width = 7, height = 5, dpi = 300)
# ggsave("fig_depth_vs_magnitude.png", p_depth_mag, width = 7, height = 5, dpi = 300)

# End of script
############################################################
