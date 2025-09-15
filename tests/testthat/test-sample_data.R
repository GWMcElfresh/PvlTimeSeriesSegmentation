# Tests for generate_sample_data function

library(testthat)

# Source the function
source("../../R/sample_data.R")

test_that("generate_sample_data creates correct data structure", {
  data <- generate_sample_data(12, pattern = "two_state")
  
  expect_true(is.data.frame(data))
  expect_equal(ncol(data), 2)
  expect_equal(names(data), c("date", "result"))
  expect_equal(nrow(data), 12)
  expect_true(all(data$result >= 25))  # Minimum detectable level
})

test_that("new regime patterns generate appropriate data ranges", {
  # Two-state pattern should have high and low regimes
  data_2s <- generate_sample_data(12, pattern = "two_state")
  expect_true(max(data_2s$result) > 1000)   # Should have high regime
  expect_true(min(data_2s$result) < 100)    # Should have low regime
  
  # Three-state should have wider range
  data_3s <- generate_sample_data(15, pattern = "three_state") 
  expect_true(max(data_3s$result) > 10000)  # Higher values
  expect_true(min(data_3s$result) < 100)    # Low suppressed values
  
  # Treatment failure should show rebound pattern
  data_tf <- generate_sample_data(15, pattern = "treatment_failure")
  expect_true(max(data_tf$result) > 5000)   # Should have high values
})

test_that("regime patterns create stable periods with low slopes", {
  # Test that regime patterns create periods suitable for basin detection
  source("../../R/interpolate_viral_load.R")
  
  data <- generate_sample_data(15, pattern = "two_state", noise_level = 0.02)
  interpolated <- interpolate_viral_load(data, n_points = 30)
  
  # Should have significant number of stable points (low slope)
  stable_points <- sum(abs(interpolated$slope) < 50, na.rm = TRUE)
  expect_true(stable_points / nrow(interpolated) > 0.5)  # At least 50% stable
})

test_that("parameter control works correctly", {
  # Test noise level control
  data_low_noise <- generate_sample_data(10, pattern = "two_state", noise_level = 0.01)
  data_high_noise <- generate_sample_data(10, pattern = "two_state", noise_level = 0.1)
  
  # Low noise should be more stable (this is probabilistic, so use reasonable threshold)
  expect_true(length(unique(round(data_low_noise$result, -2))) <= 
              length(unique(round(data_high_noise$result, -2))))
  
  # Test duration control
  data_long <- generate_sample_data(20, pattern = "two_state", regime_duration_months = 8)
  expect_equal(nrow(data_long), 20)
})

test_that("legacy patterns still work", {
  # Ensure backwards compatibility
  data_dr <- generate_sample_data(12, pattern = "decline_rebound")
  data_sd <- generate_sample_data(12, pattern = "stable_decline") 
  data_osc <- generate_sample_data(12, pattern = "oscillating")
  
  expect_equal(nrow(data_dr), 12)
  expect_equal(nrow(data_sd), 12)  
  expect_equal(nrow(data_osc), 12)
  
  # Should have decreasing trend for stable_decline
  expect_true(data_sd$result[1] > data_sd$result[12])
})

test_that("error handling works", {
  expect_error(generate_sample_data(12, pattern = "invalid_pattern"))
  expect_error(generate_sample_data(-1))  # Negative months should fail
})

test_that("regime patterns work with segmentation algorithm", {
  # Integration test: new patterns should work well with segmentation
  source("../../R/interpolate_viral_load.R")
  source("../../R/segment_time_series.R")
  source("../../R/classify_states.R")
  
  # Test two-state pattern
  data <- generate_sample_data(12, pattern = "two_state")
  interpolated <- interpolate_viral_load(data, n_points = 25)
  segmentation <- segment_time_series(interpolated, density_threshold = 0.05, min_basin_size = 2)
  classified <- classify_states(segmentation)
  
  # Should detect at least 2 basins for a two-state system
  expect_true(classified$summary$num_basins >= 2)
  expect_true(classified$summary$num_bridges >= 1)
  
  # Test three-state pattern  
  data3 <- generate_sample_data(18, pattern = "three_state")
  interpolated3 <- interpolate_viral_load(data3, n_points = 35)
  segmentation3 <- segment_time_series(interpolated3, density_threshold = 0.05, min_basin_size = 2)
  classified3 <- classify_states(segmentation3)
  
  # Should detect multiple regimes
  expect_true(classified3$summary$num_basins >= 2)
})