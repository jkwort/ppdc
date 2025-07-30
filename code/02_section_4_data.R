# ------------------------------------------------------------------------------
# Script: 02_section_4_data.R
# Purpose: Reproduce Section 4, Table 2
# Author: Jonas Klingwort & Sarah Redlich
# Date: 26.06.2025
# ------------------------------------------------------------------------------

# Clear workspace
rm(list = ls())
gc()

# Load required libraries
library(data.table)
library(geosphere)       # For geodesic distance calculations

# ------------------------------------------------------------------------------ 
# Load data and compute Haversine distances
# ------------------------------------------------------------------------------

# Define country codes to iterate over
nam <- c("DE", "NL")

# Loop through each country code
for(nm in nam){ # Example: nm = "DE"

  # Load the data object from file
  load(paste0("../data/01_results_", nm, ".RDATA"))  # 'dt' is loaded from RDATA file (run 03_section_6_prep_evaluation.r to obtain this file)
  
  # Calculate Haversine distance between two sets of coordinates (columns 1:2 and 3:4)
  dt[, d_haversine := distHaversine(dt[, 1:2], dt[, 3:4])]
  
  # Print summary statistics of distance in kilometers (shown in Table 2)
  print(nm)
  print(round(summary(dt$d_haversine) / 1000, 2))
}

# ------------------------------------------------------------------------------
# End of Script
# ------------------------------------------------------------------------------
