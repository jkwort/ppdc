# ------------------------------------------------------------------------------
# Script: 03.1_section_6.1_correlation_performance.R
# Purpose: Reproduce Section 6.1 Correlations and performance
# Author: Jonas Klingwort & Sarah Redlich
# Date: 26.06.2025
# ------------------------------------------------------------------------------

# Environment cleanup
rm(list = ls())
gc()

# Load required libraries
library(data.table)
library(ggplot2)
library(geosphere)

# ------------------------------------------------------------------------------
# Configuration
# ------------------------------------------------------------------------------

# Choose correlation method: "pearson" or "spearman"
cor_method <- "pearson"  # Change to "spearman" as needed

# Countries to include
countries <- c("DE", "NL")

# Initialize result container
res <- NULL

# ------------------------------------------------------------------------------
# Loop through countries
# ------------------------------------------------------------------------------

for (country in countries) { # Example: country = "DE"
  message("Processing country: ", country)
  
  # Load data
  load(paste0("../data/01_results_", country, ".RDATA")) # Loads `dt` (run 03_section_6_prep_evaluation.r to obtain this file)
  
  # Compute true distances (using Haversine and Vincenty)
  dt[, d_haversine := distHaversine(dt[, .(x1, y1)], dt[, .(x2, y2)])]
  dt[, d_vincentyE := distVincentyEllipsoid(dt[, .(x1, y1)], dt[, .(x2, y2)])]
  
  # Identify triangle area and distance columns
  cols_A <- grep("^A_", colnames(dt), value = TRUE)
  cols_d <- grep("^d_", colnames(dt), value = TRUE)
  
  # Initialize correlation container
  cors <- NULL
  
  for (area_col in cols_A) { # Example: # area_col = cols_A[1]
    # Compute correlations between true distance and proxy (using Haversine
    # and Vincenty)
    cor_values <- as.numeric(round(
      cor(as.matrix(dt[[area_col]]),
          as.matrix(dt[, ..cols_d]),
          use = "complete.obs",
          method = cor_method), 3))
    
    # Combine into single row
    cor_row <- c(area_col, cor_method, cor_values)
    cors <- rbind(cors, cor_row)
  }
  
  # Format as data.table
  cors <- as.data.table(cors)
  colnames(cors) <- c("n_points", "cor_method", cols_d)
  
  # Reshape for plotting
  corsm <- melt(cors, id.vars = c("n_points", "cor_method"), measure.vars = cols_d)
  
  # Clean up and annotate
  corsm[, n_points := as.numeric(gsub("A_", "", n_points))]
  corsm[, value := as.numeric(value)]
  corsm[, country := country]
  
  res <- rbind(res, corsm)
}

# ------------------------------------------------------------------------------
# Save results
# ------------------------------------------------------------------------------

# Set output filename dynamically based on method
outfile <- paste0("../data/03.1_section_6.1_results_", cor_method, ".RDATA")
save(res, file = outfile)

# ------------------------------------------------------------------------------
# Label cleaning for figures
# ------------------------------------------------------------------------------

# Rename country codes to full names
res[country == "NL", country := "Netherlands"]
res[country == "DE", country := "Germany"]

# Rename distance variables for clarity
res[variable == "d_vincentyE", variable := "Vincenty (ellipsoid) distance"]
res[variable == "d_haversine", variable := "Haversine distance"]

# Filter for range of random points
res <- res[n_points <= 300]

# ------------------------------------------------------------------------------
# Figure 3
# ------------------------------------------------------------------------------

ggplot(res, aes(x = n_points, y = value, group = country, color = country)) +
  geom_point(size = 4) +
  geom_line(linetype = "dashed") +
  facet_wrap(facets = "variable", nrow = 2) +
  scale_y_continuous(breaks = seq(0, 1, 0.25), limits = c(0, 1)) +
  scale_x_continuous(breaks = seq(0, 300, 50)) +
  scale_color_manual(values = c("darkgreen", "orange")) +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    strip.text = element_text(face = "bold")
  ) +
  labs(
    x = "Number of random points",
    y = "Correlation coefficient"
  )

# ------------------------------------------------------------------------------
# Figure 4
# ------------------------------------------------------------------------------

# Environment cleanup
rm(list = ls())
gc()

# Load preprocessed data for Section 6, Figure 4
load("../data/03_section_6.1_fig4_data.RDATA")

# Standardize country codes for clarity
res[cntry == "NL", cntry := "NL"]
res[cntry == "DE", cntry := "GER"]

# Rename variable for readability
res[variable == "d_haversine", variable := "Haversine distance"]
# Remove unnecessary columns
res$cors <- res$variable <- NULL

# Filter data to keep observations with number of points between 10 and 300
res <- res[n_points >= 10 & n_points <= 300,]

# Combine distance group and country into a single grouping variable
res[, dgroup := paste(dgroup, cntry)]

# Relabel groups for plot readability (with line breaks for legend formatting)
res[dgroup == "Smaller Distances GER", dgroup := "Smaller Distances\nGermany"]
res[dgroup == "Larger Distances GER", dgroup := "Larger Distances\nGermany"]
res[dgroup == "Smaller Distances NL", dgroup := "Smaller Distances\nNetherlands"]
res[dgroup == "Larger Distances NL", dgroup := "Larger Distances\nNetherlands"]

# Set factor levels to control legend and plot order
res[, dgroup := factor(dgroup, levels = c("Smaller Distances\nGermany", "Larger Distances\nGermany", 
                                          "Smaller Distances\nNetherlands", "Larger Distances\nNetherlands"))]

# Create plot: correlation vs. number of random points by distance group
ggplot(res,                    
            aes(x = n_points,
                y = value,
                group = dgroup)) +
  geom_line(linetype = "dashed", aes(col = dgroup)) +
  geom_point(size = 4, aes(col = dgroup, shape = dgroup)) +
  scale_color_manual(values = c("darkgreen", "darkgreen", "orange", "orange")) +
  theme(legend.position="top",
        legend.title = element_blank()) +
  scale_y_continuous(breaks = seq(0.5,1,0.1), limits = c(0.5,1)) +
  labs(x = "Number of random points", y = "Correlation coefficient") 

# ------------------------------------------------------------------------------
# Figure 5
# ------------------------------------------------------------------------------

# Environment cleanup
rm(list = ls())
gc()

# Load preprocessed data for Section 6, Figure 5
load("../data/03_section_6.1_fig5_data.RDATA")

ggplot(res, aes(x = npoints, y = value, group = cntry, color = cntry)) +
  geom_point(size = 4) +
  geom_line(linetype = "dashed") +
  scale_y_continuous(breaks = seq(0,100,12.5), limits = c(0,100)) +
  scale_color_manual(values = c("darkgreen", "orange")) +
  labs(x = "Number of random points", y = "RRMSE (%)", color = "") +
  theme(legend.position="top")  

# ------------------------------------------------------------------------------
# End of Script
# ------------------------------------------------------------------------------