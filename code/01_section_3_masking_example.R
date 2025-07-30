# ------------------------------------------------------------------------------
# Script: 01_section_3_masking_example.R
# Purpose: Reproduce example in Section 3
# Author: Jonas Klingwort & Sarah Redlich
# Date: 26.06.2025
# ------------------------------------------------------------------------------

# Clear workspace
rm(list = ls())
gc()

# Load required libraries
library(ggplot2)
library(sf)

# ------------------------------------------------------------------------------
# Load point-of-interest (POI) data
# ------------------------------------------------------------------------------

# Load POI
load("../data/01_section_3_data.RDATA")  # 'p_hos' object is loaded

# Convert POI to simple features (sf) object
p_hos <- st_as_sf(p_hos)

# Sample two random points for further spatial analysis
set.seed(1)
points_sf <- p_hos[sample(nrow(p_hos), 2), ]

# Create a LINESTRING between the two sampled points
line_sf <- st_union(points_sf) |> st_cast("LINESTRING") |> st_sf()

# ------------------------------------------------------------------------------
# Load shapefile (Germany outline) and prepare bounding box
# ------------------------------------------------------------------------------

# Load shapefile of Germany
# go to: https://gadm.org/download_country.html and download shapefile,
# then run following line and read shapefile
DE <- st_read("../data/shape_DE/gadm36_DEU_0.shp") # WGS84 (longitude/latitude)

# Create a bounding box from Germany's shapefile (optional)
bbox <- st_bbox(DE)

# Convert bounding box to sf polygon (needed for figures)
my_box <- st_as_sfc(st_bbox(c(
  xmin = as.numeric(bbox["xmin"]),
  xmax = as.numeric(bbox["xmax"]),
  ymin = as.numeric(bbox["ymin"]),
  ymax = as.numeric(bbox["ymax"])
), crs = st_crs(DE)))

# ------------------------------------------------------------------------------
# Figure step (a): Base map with Germany and bounding box
# ------------------------------------------------------------------------------

ggplot() +
  geom_sf(data = my_box, fill = NA, color = "black", linetype = "solid", size = 1) +
  geom_sf(data = DE, fill = "white", color = "black") +
  theme_minimal()

# ------------------------------------------------------------------------------
# Figure step (b): Add sampled points and connection line
# ------------------------------------------------------------------------------

ggplot() +
  geom_sf(data = my_box, fill = NA, color = "black", linetype = "solid") +
  geom_sf(data = DE, fill = "white", color = "black") +
  geom_sf(data = line_sf, color = "red", size = 3) +
  geom_sf(data = points_sf, color = "black", size = 3) +
  theme_minimal()

# ------------------------------------------------------------------------------
# Figure step (c): Add 5 sampled points and draw triangle lines
# ------------------------------------------------------------------------------

# Sample 5 random points within the bounding box
set.seed(17)
sample_points <- st_sample(my_box, size = 5, type = "random")

# Generate LINESTRING geometries from each sampled point to the two POI points (needed for figure)
lines_list <- vector("list", length = length(sample_points) * nrow(points_sf))
idx <- 1
for (i in seq_along(sample_points)) {
  for (j in seq_len(nrow(points_sf))) {
    coords <- rbind(
      st_coordinates(sample_points[i]),
      st_coordinates(points_sf[j, ])
    )
    lines_list[[idx]] <- st_linestring(coords)
    idx <- idx + 1
  }
}
lines_sfc <- st_sfc(lines_list, crs = st_crs(DE))

# Final map with all geometry
ggplot() +
  geom_sf(data = my_box, fill = NA, color = "black", linetype = "solid") +
  geom_sf(data = DE, fill = "white", color = "black") +
  geom_sf(data = line_sf, color = "red", size = 3) +
  geom_sf(data = points_sf, color = "black", size = 3) +
  geom_sf(data = sample_points, color = "green", size = 3) +
  geom_sf(data = lines_sfc, color = "black", linetype = "dotted", alpha = 0.7) +
  theme_minimal()

# ------------------------------------------------------------------------------
# Calculate triangle areas of two POI points and sampled points
# ------------------------------------------------------------------------------

# Get coordinates
coords_fixed <- st_coordinates(points_sf)
coords_sample <- st_coordinates(sample_points)

# Base points X and Y
X <- coords_fixed[1, ]
Y <- coords_fixed[2, ]

# 1) Calculate base side distance d_XY
d_XY <- sqrt(sum((Y - X)^2))

# 2) Function to calculate perpendicular height from a point to the base line
perp_height <- function(X, Y, R) {
  XY <- Y - X
  XR <- R - X
  proj_len <- sum(XR * XY) / sum(XY * XY)
  closest <- if (proj_len < 0) X else if (proj_len > 1) Y else X + proj_len * XY
  sqrt(sum((R - closest)^2))
}

# 3) Calculate heights for each sampled point
heights <- apply(coords_sample, 1, function(R) perp_height(X, Y, R))

# 4) Compute surface area of triangles
areas <- 0.5 * d_XY * heights

# ------------------------------------------------------------------------------
# Table 1 
# ------------------------------------------------------------------------------

results <- data.frame(
  base_side_of_triangle = d_XY,
  height_of_the_base_side = heights,
  surface_area = areas
)

# Round for readability (e.g., table in paper)
round(results, 3)

# Mean surface area for reporting
round(mean(results$surface_area), 3)

# ------------------------------------------------------------------------------
# End of Script
# ------------------------------------------------------------------------------
