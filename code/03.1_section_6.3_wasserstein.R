# ------------------------------------------------------------------------------
# Script: 03.1_section_6.3_wasserstein.R
# Purpose: Reproduce Section 6.3 Wasserstein Distance
# Author: Jonas Klingwort & Sarah Redlich
# Date: 09.07.2025
# ------------------------------------------------------------------------------

# Environment cleanup
rm(list = ls())
gc()

# Load required libraries
library(data.table)
library(ggplot2)
library(ggridges)
library(transport)
library(geosphere)

# ------------------------------------------------------------------------------
# Configuration
# ------------------------------------------------------------------------------

nam <- c("DE", "NL")
res <- NULL
W <- NULL

# Function to apply min-max normalization to a numeric vector
norm_minmax <- function(x){
  (x- min(x, na.rm = T)) /(max(x, na.rm = T)-min(x, na.rm = T))
}

# ------------------------------------------------------------------------------
# Compute Wasserstein Distance for each country
# ------------------------------------------------------------------------------

for(nm in nam){ # e.g., nm = "DE"
  
  # Load result file for the current country
  load(paste0("../data/01_results_",nm,".RDATA")) # (run 03_section_6_prep_evaluation.r to obtain this file)
  
  # Compute Haversine distance between point pairs
  dt[, d_haversine := distHaversine(dt[,1:2], dt[,3:4])]

  # Identify column names for 'A_' and 'd_' prefixed variables
  cols_A <- colnames(dt)[c(dt[,grep("A_", colnames(dt))])]
  cols_d <- colnames(dt)[c(dt[,grep("d_", colnames(dt))])]
  
  # Normalize distance and accuracy columns using min-max scaling
  dt[ , (cols_d) := lapply(.SD, norm_minmax), .SDcols = cols_d]
  dt[ , (cols_A) := lapply(.SD, norm_minmax), .SDcols = cols_A]
  
  # Subset normalized data for plotting and analysis
  dtm <- dt[, c(cols_A, cols_d), with = F]
  dtm <- melt(dtm) # Convert data to long format for ggplot
  
  # Select relevant variables and rename them for clarity in plots
  dtm_sel <- dtm[variable %in% c("A_1", "A_10", "A_50", "A_100", "A_300", "d_haversine")]
  dtm_sel[variable == "d_haversine", variable := "Haversine"]
  dtm_sel[variable == "A_1", variable := "r=1"]
  dtm_sel[variable == "A_10", variable := "r=10"]
  dtm_sel[variable == "A_50", variable := "r=50"]
  dtm_sel[variable == "A_100", variable := "r=100"]
  dtm_sel[variable == "A_300", variable := "r=300"]
  
  # Set factor levels for consistent plotting order
  dtm_sel[, variable := factor(variable, levels = c("r=1", "r=10", "r=50", "r=100", "r=300", "Haversine"))]
  
  # Create and save density ridge plot of normalized distributions  
  p <- ggplot(dtm_sel, aes(x = value, y = variable, fill = variable)) +
    geom_density_ridges() +
    theme_ridges() + 
    theme(legend.position = "none") +
    labs(x = "Min-max normalized density", y = "")
  
  # Display plot
  print(p)
  
  # Compute Wasserstein distances (L1) between each r=x distribution and Haversine
  sel <- unique(dtm_sel[, variable])[1:max(length(unique(dtm_sel[, variable]))-1)]
  
  for(i in sel){ # e.g., i = "r=1"
    w <- round(wasserstein1d(dtm_sel[variable == i, value], dtm_sel[variable == "Haversine", value], p=1),3)
    w <- cbind(w,i,nm)
    W <- rbind(W, w)
  }
}

# ------------------------------------------------------------------------------
# Table 4
# ------------------------------------------------------------------------------

print(W)

# ------------------------------------------------------------------------------
# End of Script
# ------------------------------------------------------------------------------
