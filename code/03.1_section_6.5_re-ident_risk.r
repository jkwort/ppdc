# ------------------------------------------------------------------------------
# Script: 03.1_section_6.5_re-ident_risk.R
# Purpose: Reproduce Section 6.5 Re-identification risk
# Author: Jonas Klingwort & Sarah Redlich
# Date: 21.07.2025
# ------------------------------------------------------------------------------

# Clear workspace
rm(list = ls())
gc()

# Load required libraries
library(data.table)
library(ggplot2)
library(geosphere)       
library(randomForest)
library(gridExtra)

# ------------------------------------------------------------------------------
# Load data
# ------------------------------------------------------------------------------

# run 03_section_6_prep_evaluation.r to obtain the data for this script
# analysis requires data to be in long format.

# ------------------------------------------------------------------------------
# Configuration
# ------------------------------------------------------------------------------

# scenarios
hardened <- c(T, F)
splits <- c(0.01, 0.1)
points <- c("A_1", "A_300")

# to save plots
plot_list <- list()
i <- 0

# ------------------------------------------------------------------------------
# Train random forest and evaluation prediction
# ------------------------------------------------------------------------------

for(h in hardened){ # h = T
  
  print(h)
  
  # select data
  if(h == F){
    data <- Am
  } else{
    data <- Am1
  }
  
  for(p in points){ # p = "A_1"
    
    print(p)
    
    # select points
    data2 <- data[variable == p]
    data2 <- data.frame(proxy = data2[, value], distance = data2[,d_haversine])
    data2 <- data2[complete.cases(data2), ]
    
    for(s in splits){ # s = 0.01
      
      print(s)
      
      # train/test split
      split <- s
      n <- nrow(data2)
      train_idx <- sample(1:n, size = split * n)
      train_data <- data2[train_idx, ]
      test_data <- data2[-train_idx, ]
      
      # Train model
      rf_model <- randomForest(distance ~ proxy, data = train_data)
      
      # Predict on test set
      test_data$predicted <- predict(rf_model, test_data)
      
      # Evaluate
      mae <- mean(abs(test_data$predicted - test_data$distance))
      rmse <- sqrt(mean((test_data$predicted - test_data$distance)^2))
      
      # save plot
      p1 <- ggplot(test_data, aes(x = distance/1000, y = predicted/1000)) +
        # geom_point(color = rgb(0, 0, 1, 0.3)) +
        geom_bin2d(bins = 30) +
        scale_fill_gradient(low = "khaki", high = "deepskyblue") +
        labs(x = "Observed Distance", 
             y = "Predicted Distance", 
             subtitle = paste0(" Hardened: ", h, "\n Random points: ", gsub("[^0-9]", "", p),"\n", " Training data: ", s*100, "%  \n MAE: ",
                               round(mae/1000,0))) + # , "\n RMSE: ", round(rmse/1000,0))) +
        theme(legend.position="none")
      i <- i+1
      plot_list[[i]] <- p1
    }
  }
}

# ------------------------------------------------------------------------------
# Figure 9
# ------------------------------------------------------------------------------

P <- do.call(grid.arrange, c(plot_list, ncol = 4))
print(P)

# ------------------------------------------------------------------------------
# End of Script
# ------------------------------------------------------------------------------

