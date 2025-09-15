test_that("segment_time_series works correctly", {
  # Create test data
  dates <- seq(as.Date("2020-01-01"), as.Date("2020-12-01"), by = "month")
  values <- c(1000, 500, 200, 50, 25, 50, 100, 500, 1000, 1500, 800, 300)
  test_data <- data.frame(date = dates, result = values)
  
  # Interpolate
  interpolated <- interpolate_viral_load(test_data, n_points = 20)
  
  # Test segmentation
  result <- segment_time_series(interpolated)
  
  # Check structure
  expect_true(is.list(result))
  expect_true(all(c("data", "kde_result", "summary", "parameters") %in% names(result)))
  
  # Check data structure
  expect_true(is.data.frame(result$data))
  expect_true(all(c("value", "slope", "density", "state", "basin_index", "bridge_index") %in% names(result$data)))
  
  # Check that states are assigned
  expect_true(all(result$data$state %in% c("Basin", "Bridge")))
  
  # Check that density values are normalized
  expect_true(max(result$data$density, na.rm = TRUE) <= 1)
  expect_true(min(result$data$density, na.rm = TRUE) >= 0)
  
  # Check summary
  expect_true(is.list(result$summary))
  expect_true(result$summary$total_points > 0)
})

test_that("segment_time_series handles edge cases", {
  # Test with insufficient data
  small_data <- data.frame(
    time_index = 1:2,
    value = c(100, 200),
    slope = c(NA, 100),
    date = as.Date(c("2020-01-01", "2020-01-02"))
  )
  
  expect_error(segment_time_series(small_data), "Insufficient data points")
  
  # Test with missing columns
  bad_data <- data.frame(x = 1:10, y = 1:10)
  expect_error(segment_time_series(bad_data), "Data must contain 'value' and 'slope' columns")
})