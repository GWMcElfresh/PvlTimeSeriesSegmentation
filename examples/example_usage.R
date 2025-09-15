# Example Usage of PvlTimeSeriesSegmentation Package
# 
# This script demonstrates the complete workflow for analyzing viral load
# time series data using the PvlTimeSeriesSegmentation package with improved
# regime-based sample data generation.

# Load the package source files (for development)
source('../R/sample_data.R')
source('../R/interpolate_viral_load.R')
source('../R/segment_time_series.R')
source('../R/classify_states.R')
source('../R/plot_segmentation.R')

# Example 1: Two-State System (High Viremia -> Suppression)
cat("=== Example 1: Two-State Viral Load System ===\n")

# Generate sample data with realistic two-state regime pattern
sample_data1 <- generate_sample_data(12, start_date = "2020-01-01", pattern = "two_state",
                                   regime_duration_months = 4, noise_level = 0.03)
cat("Generated two-state pattern with high viremia -> viral suppression\n")
print(sample_data1)

# Interpolate to feature space
interpolated1 <- interpolate_viral_load(sample_data1, n_points = 30)
cat("Interpolated to", nrow(interpolated1), "points\n")

# Analyze slope characteristics to verify regime structure
slope_abs <- abs(interpolated1$slope)
stable_points <- sum(slope_abs < 50, na.rm = TRUE)
cat("Stable points (|slope| < 50):", stable_points, "out of", nrow(interpolated1), 
    paste0("(", round(100 * stable_points / nrow(interpolated1)), "%)\n"))

# Perform segmentation with parameters optimized for regime detection
segmentation1 <- segment_time_series(interpolated1, 
                                   density_threshold = 0.05,  # Lower threshold for regime detection
                                   min_basin_size = 2,         # Allow smaller basins
                                   variance_threshold = 0.3)   # Optimized for regime transitions

# Classify states
classified1 <- classify_states(segmentation1)

# Print results
cat("Segmentation Results:\n")
cat("- Total points:", classified1$summary$total_points, "\n")
cat("- Regimes (basins) found:", classified1$summary$num_basins, "\n")
cat("- Transitions (bridges) found:", classified1$summary$num_bridges, "\n")
cat("- Mean basin density:", round(classified1$summary$mean_basin_density, 3), "\n")
cat("- Mean bridge density:", round(classified1$summary$mean_bridge_density, 3), "\n")

cat("\nRegime/State distribution:\n")
print(table(classified1$data$state_label))

# Create plots showing regime segmentation
png("example1_trajectory.png", width = 800, height = 600)
plot_segmentation(classified1, plot_type = "trajectory", use_ggplot = FALSE)
title(main = "Example 1: Two-State System in (Value, Slope) Space")
dev.off()

png("example1_time_series.png", width = 800, height = 600)
plot_segmentation(classified1, plot_type = "time_series", use_ggplot = FALSE)
title(main = "Example 1: Two-State Viral Load Time Series")
dev.off()

cat("Plots saved: example1_trajectory.png, example1_time_series.png\n\n")

# Example 2: Three-State System with Treatment Response
cat("=== Example 2: Three-State Treatment Response Pattern ===\n")

sample_data2 <- generate_sample_data(18, start_date = "2019-01-01", pattern = "three_state",
                                   regime_duration_months = 5, transition_duration_months = 1)
cat("Generated three-state pattern: high -> intermediate -> suppressed\n")

interpolated2 <- interpolate_viral_load(sample_data2, n_points = 40)
segmentation2 <- segment_time_series(interpolated2,
                                   density_threshold = 0.05,
                                   min_basin_size = 2)
classified2 <- classify_states(segmentation2)

cat("Results:\n")
cat("- Regimes (basins) found:", classified2$summary$num_basins, "\n")
cat("- Transitions (bridges) found:", classified2$summary$num_bridges, "\n")

cat("\nRegime distribution:\n")
print(table(classified2$data$state_label))

png("example2_trajectory.png", width = 800, height = 600)
plot_segmentation(classified2, plot_type = "trajectory", use_ggplot = FALSE)
title(main = "Example 2: Three-State Treatment Response Trajectory")
dev.off()

png("example2_time_series.png", width = 800, height = 600)
plot_segmentation(classified2, plot_type = "time_series", use_ggplot = FALSE)
title(main = "Example 2: Three-State Treatment Response Time Series")
dev.off()

cat("Plots saved: example2_trajectory.png, example2_time_series.png\n\n")

# Example 3: Treatment Failure and Recovery Pattern
cat("=== Example 3: Treatment Failure and Recovery ===\n")

sample_data3 <- generate_sample_data(20, start_date = "2018-01-01", pattern = "treatment_failure")
cat("Generated treatment failure pattern: high -> suppressed -> failure -> recovery\n")

interpolated3 <- interpolate_viral_load(sample_data3, method = "spline", n_points = 35)
segmentation3 <- segment_time_series(interpolated3,
                                   density_threshold = 0.04,
                                   min_basin_size = 2,
                                   variance_threshold = 0.25)
classified3 <- classify_states(segmentation3)

cat("Treatment failure analysis results:\n")
cat("- Regimes identified:", classified3$summary$num_basins, "\n")
cat("- Transition periods:", classified3$summary$num_bridges, "\n")

# Show detailed regime progression
cat("\nDetailed regime progression (first 10 timepoints):\n")
regime_summary <- classified3$data[, c("date", "value", "state_label", "density")]
print(head(regime_summary, 10))

# Example 4: Suppression-Rebound Pattern (Adherence Issues)
cat("\n=== Example 4: Suppression-Rebound Pattern ===\n")

sample_data4 <- generate_sample_data(15, pattern = "suppression_rebound")
cat("Generated suppression-rebound pattern (common with adherence issues)\n")

interpolated4 <- interpolate_viral_load(sample_data4, n_points = 30)
segmentation4 <- segment_time_series(interpolated4, density_threshold = 0.05, min_basin_size = 2)
classified4 <- classify_states(segmentation4)

cat("Results:\n")
cat("- Regimes found:", classified4$summary$num_basins, "\n")
cat("- Transitions found:", classified4$summary$num_bridges, "\n")
print(table(classified4$data$state_label))

# Parameter Sensitivity Analysis for Regime Detection
cat("\n=== Parameter Sensitivity Analysis for Regime Detection ===\n")

cat("Testing different density thresholds on two-state system:\n")
density_thresholds <- c(0.02, 0.05, 0.1, 0.15, 0.2)
for (thresh in density_thresholds) {
  seg_test <- segment_time_series(interpolated1, density_threshold = thresh, min_basin_size = 2)
  cls_test <- classify_states(seg_test)
  cat("Density threshold", thresh, ": ", cls_test$summary$num_basins, "regimes,", 
      cls_test$summary$num_bridges, "transitions\n")
}

cat("\nTesting different minimum basin sizes:\n")
basin_sizes <- c(1, 2, 3, 5)
for (size in basin_sizes) {
  seg_test <- segment_time_series(interpolated1, density_threshold = 0.05, min_basin_size = size)
  cls_test <- classify_states(seg_test)
  cat("Min basin size", size, ": ", cls_test$summary$num_basins, "regimes,", 
      cls_test$summary$num_bridges, "transitions\n")
}

cat("\nAdvanced regime analysis complete!\n")
cat("Key improvements in new sample data:\n")
cat("- Realistic quasi-stable viral load regimes with near-zero slopes\n")
cat("- Biologically plausible transitions between regimes\n") 
cat("- Better basin/bridge detection by segmentation algorithm\n")
cat("- Support for clinically relevant patterns (treatment response, failure, adherence issues)\n")
cat("\nGenerated files:\n")
cat("- example1_trajectory.png (two-state system)\n")
cat("- example1_time_series.png\n")
cat("- example2_trajectory.png (three-state system)\n")
cat("- example2_time_series.png\n")