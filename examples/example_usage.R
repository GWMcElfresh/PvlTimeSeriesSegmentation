# Example Usage of PvlTimeSeriesSegmentation Package
# 
# This script demonstrates the complete workflow for analyzing viral load
# time series data using the PvlTimeSeriesSegmentation package.

# Load the package source files (for development)
source('../R/sample_data.R')
source('../R/interpolate_viral_load.R')
source('../R/segment_time_series.R')
source('../R/classify_states.R')
source('../R/plot_segmentation.R')

# Example 1: Basic Usage with Decline-Rebound Pattern
cat("=== Example 1: Decline-Rebound Pattern ===\n")

# Generate sample data representing typical HIV viral load pattern
sample_data1 <- generate_sample_data(12, start_date = "2020-01-01", pattern = "decline_rebound")
print(sample_data1)

# Interpolate to feature space
interpolated1 <- interpolate_viral_load(sample_data1, n_points = 30)
cat("Interpolated to", nrow(interpolated1), "points\n")

# Perform segmentation
segmentation1 <- segment_time_series(interpolated1, 
                                   density_threshold = 0.1,
                                   min_basin_size = 3,
                                   variance_threshold = 0.5)

# Classify states
classified1 <- classify_states(segmentation1)

# Print results
cat("Results:\n")
cat("- Total points:", classified1$summary$total_points, "\n")
cat("- Basins found:", classified1$summary$num_basins, "\n")
cat("- Bridges found:", classified1$summary$num_bridges, "\n")
cat("- Mean basin density:", round(classified1$summary$mean_basin_density, 3), "\n")
cat("- Mean bridge density:", round(classified1$summary$mean_bridge_density, 3), "\n")

cat("\nState labels distribution:\n")
print(table(classified1$data$state_label))

# Create plots
png("example1_trajectory.png", width = 800, height = 600)
plot_segmentation(classified1, plot_type = "trajectory", use_ggplot = FALSE)
title(main = "Example 1: Trajectory in (Value, Slope) Space")
dev.off()

png("example1_time_series.png", width = 800, height = 600)
plot_segmentation(classified1, plot_type = "time_series", use_ggplot = FALSE)
title(main = "Example 1: Time Series with Segmentation")
dev.off()

cat("Plots saved: example1_trajectory.png, example1_time_series.png\n\n")

# Example 2: Oscillating Pattern
cat("=== Example 2: Oscillating Pattern ===\n")

sample_data2 <- generate_sample_data(18, start_date = "2019-01-01", pattern = "oscillating")
interpolated2 <- interpolate_viral_load(sample_data2, n_points = 40)
segmentation2 <- segment_time_series(interpolated2,
                                   density_threshold = 0.15,
                                   min_basin_size = 4)
classified2 <- classify_states(segmentation2)

cat("Results:\n")
cat("- Basins found:", classified2$summary$num_basins, "\n")
cat("- Bridges found:", classified2$summary$num_bridges, "\n")

cat("\nState labels distribution:\n")
print(table(classified2$data$state_label))

png("example2_trajectory.png", width = 800, height = 600)
plot_segmentation(classified2, plot_type = "trajectory", use_ggplot = FALSE)
title(main = "Example 2: Oscillating Pattern Trajectory")
dev.off()

png("example2_time_series.png", width = 800, height = 600)
plot_segmentation(classified2, plot_type = "time_series", use_ggplot = FALSE)
title(main = "Example 2: Oscillating Pattern Time Series")
dev.off()

cat("Plots saved: example2_trajectory.png, example2_time_series.png\n\n")

# Example 3: Custom Data Analysis
cat("=== Example 3: Custom Data Analysis ===\n")

# Create custom viral load data
custom_dates <- seq(as.Date("2021-01-01"), by = "2 weeks", length.out = 15)
custom_values <- c(50000, 25000, 10000, 5000, 1000, 500, 200, 100, 50, 
                   100, 500, 2000, 8000, 15000, 10000)
custom_data <- data.frame(date = custom_dates, result = custom_values)

# Analyze with different parameters
interpolated3 <- interpolate_viral_load(custom_data, method = "spline", n_points = 25)
segmentation3 <- segment_time_series(interpolated3,
                                   density_threshold = 0.2,
                                   min_basin_size = 2,
                                   variance_threshold = 0.3)
classified3 <- classify_states(segmentation3)

cat("Custom data results:\n")
cat("- Basins found:", classified3$summary$num_basins, "\n")
cat("- Bridges found:", classified3$summary$num_bridges, "\n")

# Show detailed state information
cat("\nDetailed state progression:\n")
state_summary <- classified3$data[, c("date", "value", "state_label", "density")]
print(head(state_summary, 10))

# Parameter sensitivity analysis
cat("\n=== Parameter Sensitivity Analysis ===\n")

density_thresholds <- c(0.05, 0.1, 0.2, 0.3)
for (thresh in density_thresholds) {
  seg_test <- segment_time_series(interpolated1, density_threshold = thresh)
  cat("Density threshold", thresh, ": ", seg_test$summary$num_basins, "basins,", 
      seg_test$summary$num_bridges, "bridges\n")
}

cat("\nExample analysis complete!\n")
cat("Generated files:\n")
cat("- example1_trajectory.png\n")
cat("- example1_time_series.png\n")
cat("- example2_trajectory.png\n")
cat("- example2_time_series.png\n")