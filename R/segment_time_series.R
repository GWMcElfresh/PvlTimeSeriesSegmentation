#' Segment Time Series Using Kernel Density Estimation
#'
#' Identifies basins (high-density regions) and bridges (low-density transitions)
#' in 2D (value, slope) feature space using kernel density estimation.
#'
#' @param interpolated_data Data frame from interpolate_viral_load with value and slope columns
#' @param density_threshold Threshold for distinguishing basins from bridges (default: 0.05, optimized for regime detection)
#' @param min_basin_size Minimum number of consecutive points to form a basin (default: 2, optimized for regime detection)
#' @param variance_threshold Threshold for bridge variance heuristic (default: 0.3, optimized for regime transitions)
#' @return A list containing density estimates, basin/bridge classifications, and summary statistics
#' @export
#' @examples
#' # Using interpolated data
#' dates <- seq(as.Date("2020-01-01"), as.Date("2020-12-31"), by = "month")
#' values <- c(1000, 500, 200, 50, 25, 100, 500, 1000, 2000, 1500, 800, 300)
#' sample_data <- data.frame(date = dates, result = values)
#' interpolated <- interpolate_viral_load(sample_data)
#' segmentation <- segment_time_series(interpolated)
segment_time_series <- function(interpolated_data, density_threshold = 0.05, 
                               min_basin_size = 2, variance_threshold = 0.3) {
  
  if (!all(c("value", "slope") %in% names(interpolated_data))) {
    stop("Data must contain 'value' and 'slope' columns")
  }
  
  # Remove any rows with NA values
  data_clean <- interpolated_data[complete.cases(interpolated_data[c("value", "slope")]), ]
  
  if (nrow(data_clean) < 3) {
    stop("Insufficient data points for segmentation")
  }
  
  # Perform 2D kernel density estimation
  kde_result <- MASS::kde2d(data_clean$value, data_clean$slope, n = 50)
  
  # Interpolate density values for each data point
  density_values <- numeric(nrow(data_clean))
  
  for (i in 1:nrow(data_clean)) {
    # Find closest grid points for interpolation
    x_idx <- which.min(abs(kde_result$x - data_clean$value[i]))
    y_idx <- which.min(abs(kde_result$y - data_clean$slope[i]))
    
    density_values[i] <- kde_result$z[x_idx, y_idx]
  }
  
  # Normalize density values
  density_values <- density_values / max(density_values, na.rm = TRUE)
  
  # Initial classification based on density threshold
  initial_states <- ifelse(density_values > density_threshold, "Basin", "Bridge")
  
  # Apply minimum basin size constraint
  states <- initial_states
  rle_result <- rle(initial_states)
  
  # Convert short basins to bridges
  start_idx <- 1
  for (i in 1:length(rle_result$lengths)) {
    end_idx <- start_idx + rle_result$lengths[i] - 1
    if (rle_result$values[i] == "Basin" && rle_result$lengths[i] < min_basin_size) {
      states[start_idx:end_idx] <- "Bridge"
    }
    start_idx <- end_idx + 1
  }
  
  # Apply variance heuristic for bridges
  # Bridges should show higher variance in trajectory
  states <- apply_variance_heuristic(data_clean, states, variance_threshold)
  
  # Assign basin and bridge indices
  basin_bridge_indices <- assign_indices(states)
  
  # Create result data frame
  result_data <- data.frame(
    time_index = data_clean$time_index,
    value = data_clean$value,
    slope = data_clean$slope,
    date = data_clean$date,
    density = density_values,
    state = states,
    basin_index = basin_bridge_indices$basin_index,
    bridge_index = basin_bridge_indices$bridge_index
  )
  
  # Return comprehensive results
  list(
    data = result_data,
    kde_result = kde_result,
    summary = summarize_segmentation(result_data),
    parameters = list(
      density_threshold = density_threshold,
      min_basin_size = min_basin_size,
      variance_threshold = variance_threshold
    )
  )
}

#' Apply Variance Heuristic for Bridge Classification
#' @param data Data frame with value and slope columns
#' @param states Current state classifications
#' @param variance_threshold Threshold for variance-based classification
#' @return Updated state classifications
apply_variance_heuristic <- function(data, states, variance_threshold) {
  if (length(states) < 3) return(states)
  
  # Calculate local variance in a sliding window
  window_size <- min(5, floor(length(states) / 3))
  
  for (i in (window_size + 1):(length(states) - window_size)) {
    window_indices <- (i - window_size):(i + window_size)
    
    # Calculate variance in value and slope
    value_var <- var(data$value[window_indices], na.rm = TRUE)
    slope_var <- var(data$slope[window_indices], na.rm = TRUE)
    
    # Normalize variances
    total_value_var <- var(data$value, na.rm = TRUE)
    total_slope_var <- var(data$slope, na.rm = TRUE)
    
    norm_value_var <- value_var / total_value_var
    norm_slope_var <- slope_var / total_slope_var
    
    # High variance suggests bridge behavior
    if (norm_value_var > variance_threshold || norm_slope_var > variance_threshold) {
      states[i] <- "Bridge"
    }
  }
  
  return(states)
}

#' Assign Basin and Bridge Indices
#' @param states State classifications
#' @return List with basin_index and bridge_index vectors
assign_indices <- function(states) {
  basin_index <- rep(NA, length(states))
  bridge_index <- rep(NA, length(states))
  
  current_basin <- 0
  current_bridge <- 0
  
  rle_result <- rle(states)
  start_idx <- 1
  
  for (i in 1:length(rle_result$lengths)) {
    end_idx <- start_idx + rle_result$lengths[i] - 1
    
    if (rle_result$values[i] == "Basin") {
      current_basin <- current_basin + 1
      basin_index[start_idx:end_idx] <- current_basin
    } else {
      current_bridge <- current_bridge + 1
      bridge_index[start_idx:end_idx] <- current_bridge
    }
    
    start_idx <- end_idx + 1
  }
  
  list(basin_index = basin_index, bridge_index = bridge_index)
}

#' Summarize Segmentation Results
#' @param result_data Segmentation result data frame
#' @return Summary statistics
summarize_segmentation <- function(result_data) {
  basin_data <- result_data[result_data$state == "Basin", ]
  bridge_data <- result_data[result_data$state == "Bridge", ]
  
  list(
    total_points = nrow(result_data),
    basin_points = nrow(basin_data),
    bridge_points = nrow(bridge_data),
    num_basins = max(result_data$basin_index, na.rm = TRUE),
    num_bridges = max(result_data$bridge_index, na.rm = TRUE),
    mean_basin_density = mean(basin_data$density, na.rm = TRUE),
    mean_bridge_density = mean(bridge_data$density, na.rm = TRUE)
  )
}