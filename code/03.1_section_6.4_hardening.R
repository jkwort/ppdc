# ------------------------------------------------------------------------------
# Script: 03.1_section_6.4_hardening.R
# Purpose: Reproduce Section 6.4 Hardening against attacking approaches
# Author: Jonas Klingwort & Sarah Redlich
# Date: 21.07.2025
# ------------------------------------------------------------------------------

# Environment cleanup
rm(list = ls())
gc()

# Load required libraries
library(data.table)
library(ggplot2)

# ------------------------------------------------------------------------------
# Load data
# ------------------------------------------------------------------------------

load("../data/03.1_section_6.4_fig8_data.RDATA")

# ------------------------------------------------------------------------------
# Figure 8
# ------------------------------------------------------------------------------

ggplot(res, aes(x = n_points, y = value, shape = variant, color = variant)) +
  geom_point(size = 4) +
  geom_line(linetype = "dashed") +
  facet_wrap(facets = c("variable"), nrow = 1) +
  scale_y_continuous(breaks = seq(0,1,0.25), limits = c(0,1)) +
   scale_colour_manual(name = "variant",
                        labels = c(bquote(bar(A)), bquote(bar(A)[1]), bquote(bar(A)[2]), bquote(bar(A)[3])),
                        values = c("red", "darkgreen", "darkgreen", "darkgreen")) +   
   scale_shape_manual(name = "variant",
                       labels = c(bquote(bar(A)), bquote(bar(A)[1]), bquote(bar(A)[2]), bquote(bar(A)[3])),
                       values = c(19, 18, 17, 16, 15, 14)) +
   theme(legend.position="top", legend.title = element_blank()) +
   scale_x_continuous(breaks = seq(0,300,50)) +
   labs(x = "Number of random points", y = "Correlation coefficient")

# ------------------------------------------------------------------------------
# Author note
# ------------------------------------------------------------------------------

# Please note that only the resulting data are provided in this release. 
# The underlying population totals and density figures required for the analyses to reproduce these results are not included due to data sharing restrictions. 
# Additionally, the associated spatial data (shapefiles) used in the analysis cannot be shared (as in other scripts). 
# These limitations are due to licensing agreements and confidentiality obligations that prohibit the distribution of the original demographic and geospatial datasets.

# ------------------------------------------------------------------------------
# End of Script
# ------------------------------------------------------------------------------
