test_that("interpolate_viral_load works correctly", {
  # Create test data
  dates <- seq(as.Date("2020-01-01"), as.Date("2020-06-01"), by = "month")
  values <- c(1000, 500, 200, 100, 50, 25)
  test_data <- data.frame(date = dates, result = values)
  
  # Test interpolation
  result <- interpolate_viral_load(test_data, n_points = 10)
  
  # Check structure
  expect_true(is.data.frame(result))
  expect_true(all(c("time_index", "value", "slope", "date") %in% names(result)))
  expect_equal(nrow(result), 9)  # n_points - 1 due to slope calculation
  
  # Check that slopes are calculated
  expect_true(all(!is.na(result$slope)))
  
  # Test error handling
  expect_error(interpolate_viral_load(data.frame(x = 1, y = 2)), 
               "Data must contain 'date' and 'result' columns")
})

test_that("interpolate_viral_load handles different methods", {
  dates <- seq(as.Date("2020-01-01"), as.Date("2020-04-01"), by = "month")
  values <- c(1000, 500, 200, 100)
  test_data <- data.frame(date = dates, result = values)
  
  # Test linear method
  result_linear <- interpolate_viral_load(test_data, method = "linear", n_points = 5)
  expect_equal(nrow(result_linear), 4)
  
  # Test spline method
  result_spline <- interpolate_viral_load(test_data, method = "spline", n_points = 5)
  expect_equal(nrow(result_spline), 4)
  
  # Results should be different
  expect_false(identical(result_linear$value, result_spline$value))
})