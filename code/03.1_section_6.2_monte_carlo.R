# Script: 03.1_section_6.2_monte_carlo.R
# Purpose: Reproduce Section 6.2 Monte Carlo simulation
# Author: Jonas Klingwort & Sarah Redlich
# Date: 09.07.2025
# ------------------------------------------------------------------------------

# Environment cleanup
rm(list = ls())
gc()

# Load required libraries
library(data.table)
library(ggplot2)

# ------------------------------------------------------------------------------
# Bootstrap Summary Statistics
# ------------------------------------------------------------------------------

# Load precomputed data for Germany
# Change the file path to load("../data/03_section_6.2_data_NL.RDATA") to switch to Netherlands data
load("../data/03_section_6.2_data_GER.RDATA") 

# Compute bootstrapped summary statistics for Haversine distances by number of points
boot = result[,
              .(bsmean = mean(as.numeric(d_haversine)),
                se = sd(as.numeric(d_haversine)),
                median = median(as.numeric(d_haversine)),
                q0.025 = quantile(as.numeric(d_haversine), probs = 0.025),
                q0.975 = quantile(as.numeric(d_haversine), probs = 0.975)),
              by = .(n_points)]

# Create a new variable 'x' by repeating vector P, R times for plotting or alignment
result[, x := rep(P,each=R)]

# ------------------------------------------------------------------------------
# Table 3
# ------------------------------------------------------------------------------

# Define the set of 'A_' variables (representing different numbers of random points)
npoints <- c("A_1", "A_3", "A_5", "A_7", "A_10", "A_50", "A_100", "A_200", "A_300") # for NL remove A_300

# Filter the bootstrapped summary table to include only selected 'A_' values
boot <- boot[n_points %in% npoints]

# Round summary statistics (columns 2 to 6) to 5 decimal places for readability
boot <- round(boot[,2:6],2)

# Add a column to indicate the corresponding 'A_' level for each row
boot[, npintd := npoints]

# Print the processed table
print(boot) # reported: nprintd = A_300 for DE; nprintd = A_1 for NL

# ------------------------------------------------------------------------------
# Figure 6
# ------------------------------------------------------------------------------

# Select number of random points
p2 <- "A_300"  # Use "A_300" for Germany
# p2 <- "A_1"  # Uncomment to use "A_1" for Netherlands

# Extract Haversine distances (as numeric) for the selected level of random points
x = result[n_points == p2, as.numeric(d_haversine)] # CW for shipment weight

# Initialize vectors to store bootstrap statistics
bsmean = ciupper = cilower = se = numeric()

# Loop through the data incrementally to compute statistics at each sample size
for (i in 1:length(x)) {
  bsmean[i] = mean(x[1:i])
  q = quantile(x[1:i], c(0.025, 0.975))
  cilower[i] = q[1]
  ciupper[i] = q[2]
  se[i] = sqrt(var(x[1:i]) / i)
}

# Plot bootstrapped mean with confidence intervals
ggplot() + 
  geom_line(aes(x = 1:length(x), y = cilower), col = "grey", linewidth = 1.5) + 
  geom_line(aes(x = 1:length(x), y = ciupper), col = "grey", linewidth = 1.5) +
  geom_line(aes(x = 1:length(x), y = bsmean), col = "darkgreen", linewidth = 1.5) + #  use for GER
  # geom_line(aes(x = 1:length(x), y = bsmean), col = "orange", size = 1.5) + #  use for NL
  theme_bw() + 
  theme(panel.background = element_blank(),
        panel.border = element_rect(fill = NA, colour = "black", linewidth = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(colour = "black"),
        legend.background = element_rect(fill = NA)) +
  labs(y = "MC mean", x = "Number of samples")

# ------------------------------------------------------------------------------
# End of Script
# ------------------------------------------------------------------------------
