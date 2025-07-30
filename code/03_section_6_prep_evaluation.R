# ------------------------------------------------------------------------------
# Script: 03_section_6_prep_evaluation.R
# Purpose: Application of Masking Method for Utility and Risk Evaluation
# Author: Jonas Klingwort & Sarah Redlich
# Date: 26.06.2025
# ------------------------------------------------------------------------------

# Environment cleanup
rm(list = ls())
gc()

# Load required libraries
library(data.table)
library(rlist)

# ------------------------------------------------------------------------------
# Configuration
# ------------------------------------------------------------------------------

# Define the dataset to use (choose "DE" or "NL")
country <- "DE"  # Change to "NL" as needed

# ------------------------------------------------------------------------------
# Load data
# ------------------------------------------------------------------------------

# Load data (DE and NL)
load("../data/03_section_6_data.RDATA")  # Loads multiple objects into workspace

# Select appropriate data based on country
if (country == "DE") {
  poi   <- de_hospital
  shape <- de_shape # go to: https://gadm.org/download_country.html and download shapefile
  res   <- de_res
  outfile <- "../data/01_results_DE.RDATA"
} else if (country == "NL") {
  poi   <- nl_school
  shape <- nl_shape # go to: https://gadm.org/download_country.html and download shapefile
  res   <- nl_res
  outfile <- "../data/01_results_NL.RDATA"
} else {
  stop("Invalid country selection. Choose either 'DE' or 'NL'.")
}

# Remove unused variables
rm(list = setdiff(ls(), c("poi", "shape", "res"))) 
gc()

# ------------------------------------------------------------------------------
# Prepare coordinate pairs
# ------------------------------------------------------------------------------

# Create data set with each combination of POI and random points
# Repeat each POI for every raster point
poiA <- poi[rep(seq_len(length(poi)), each = length(res)), ]
# Repeat each raster point for every POI
poiB <- data.table(res@coords)
poiB <- poiB[rep(poiB[, .I], length(poi))]
# Combine coordinate sets
dt <- cbind(data.table(poiA@coords), poiB)
dt
colnames(dt) <-  c("x1", "y1", "x2", "y2")
rm(poiA, poiB, poi)
gc()

# Define base coordinate vectors (needed to combine results of true coordinates and proxy)
x1 <- dt[, x1]
y1 <- dt[, y1]
x2 <- dt[, x2]
y2 <- dt[, y2]

# ------------------------------------------------------------------------------
# Loop: Compute triangle surface areas for various sample sizes
# ------------------------------------------------------------------------------

# Number of random points to simulate (for comparison of different numbers of random points)
P <- c(seq(1, 9, 1), seq(10, 90, 10), seq(100, 300, 100))

for (p in P) { # Example: p = 1
  print(p)
  
  # Generate p random X-coordinates in bounding box
  xr <- replicate(p, runif(n = nrow(dt), min = shape@bbox[1], max = shape@bbox[3]))
  xr <- as.data.table(list.cbind(xr))
  
  # Generate p random Y-coordinates in bounding box
  yr <- replicate(p, runif(n = nrow(dt), min = shape@bbox[2], max = shape@bbox[4]))
  yr <- as.data.table(list.cbind(yr))
  
  # Base side distance (Euclidean)
  d <- sqrt((x2 - x1)^2 + (y2 - y1)^2)
  
  # Height from each random point to base
  h <- abs((x2 - x1) * (y1 - yr) - (x1 - xr) * (y2 - y1)) / sqrt((x2 - x1)^2 + (y2 - y1)^2)
  
  # Triangle area = 0.5 * base * height
  A <- 0.5 * d * h
  
  # Add average area to data.table
  dt[[paste0("A_", p)]] <- rowMeans(A)
  
  # Memory cleanup
  rm(A, h, xr, yr, d)
  gc()
}

# ------------------------------------------------------------------------------
# Save results
# ------------------------------------------------------------------------------

save(dt, file = outfile)

# ------------------------------------------------------------------------------
# End of Script
# ------------------------------------------------------------------------------
