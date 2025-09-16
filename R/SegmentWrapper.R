#' @title SegmentWrapper
#' @description A wrapper function that combines interpolation, segmentation, and classification of viral load time series data.
#' @param data A data frame containing 'date' and 'result' columns representing the time series data.
#' @param n_points Number of points for interpolation (default is 100).
#' @param method Interpolation method, either "linear" or "spline" (default is "spline").
#' @param density_threshold Density threshold for segmentation (default is 0.05).
#' @param min_basin_size Minimum number of consecutive points to qualify as a basin (default is 2).
#' @param variance_threshold Variance threshold for bridge classification (default is 0.3).
#' @return A list containing the classified segmentation results and summary statistics.
#' @export

SegmentWrapper <- function(data,
                           n_points = 100,
                           method = "spline",
                           density_threshold = 0.05,
                           min_basin_size = 2,
                           variance_threshold = 0.3,
                           prepended_aviremic_phase = FALSE) {
  #interpolate the viral load data
  interpolated_data <- interpolate_viral_load(data,
                                              n_points = n_points,
                                              method = method)

  #optionally add a prepended aviremic phase to the interpolated data
  if (prepended_aviremic_phase) {
    if (min(data$result) == 0){

    aviremic_phase <- data.frame(
      time_index = seq(-n_points/10, -1, length.out = n_points/10),
      value = rep(0, n_points/10),
      slope = rep(0, n_points/10),
      date = seq(min(data$date) - (n_points/10), by = "day", length.out = n_points/10)
    )
    interpolated_data <- rbind(aviremic_phase, interpolated_data)
    } else {
    min_value <- min(data$result)
    aviremic_phase <- data.frame(
      time_index = seq(-n_points/10, -1, length.out = n_points/10),
      value = rep(min_value, n_points/10),
      slope = rep(0, n_points/10),
      date = seq(min(data$date) - (n_points/10), by = "day", length.out = n_points/10)
    )
  }
    interpolated_data <- rbind(aviremic_phase, interpolated_data)
  }
  #segment the interpolated data
  segmentation_result <- segment_time_series(interpolated_data,
                                             density_threshold = density_threshold,
                                             min_basin_size = min_basin_size,
                                             variance_threshold = variance_threshold)

  #classify the segmented states
  classified_result <- classify_states(segmentation_result)

  return(classified_result)
}
