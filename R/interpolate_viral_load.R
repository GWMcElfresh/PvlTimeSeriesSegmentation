#' Interpolate Viral Load Data
#'
#' Takes viral load data with date and result columns and interpolates to create
#' a feature space with value and slope components.
#'
#' @param data A data frame with columns 'date' and 'result' containing viral load measurements
#' @param method Interpolation method: "linear" or "spline"
#' @param n_points Number of points to interpolate (default: 100)
#' @return A data frame with columns: time_index, value, slope, date
#' @export
#' @examples
#' # Create sample data
#' dates <- seq(as.Date("2020-01-01"), as.Date("2020-12-31"), by = "month")
#' values <- c(1000, 500, 200, 50, 25, 100, 500, 1000, 2000, 1500, 800, 300)
#' sample_data <- data.frame(date = dates, result = values)
#' 
#' # Interpolate the data
#' interpolated <- interpolate_viral_load(sample_data)
#' head(interpolated)
interpolate_viral_load <- function(data, method = "linear", n_points = 100) {
  if (!all(c("date", "result") %in% names(data))) {
    stop("Data must contain 'date' and 'result' columns")
  }
  
  # Sort by date
  data <- data[order(data$date), ]
  
  # Convert dates to numeric for interpolation
  numeric_dates <- as.numeric(data$date)
  
  # Create interpolation points
  time_seq <- seq(min(numeric_dates), max(numeric_dates), length.out = n_points)
  
  # Interpolate viral load values
  if (method == "spline") {
    interpolated_values <- spline(numeric_dates, data$result, xout = time_seq)$y
  } else {
    interpolated_values <- approx(numeric_dates, data$result, xout = time_seq)$y
  }
  
  # Calculate slopes (derivatives)
  slopes <- c(NA, diff(interpolated_values) / diff(time_seq))
  
  # Create result data frame
  result <- data.frame(
    time_index = 1:n_points,
    value = interpolated_values,
    slope = slopes,
    date = as.Date(time_seq, origin = "1970-01-01")
  )
  
  # Remove NA slopes (first point)
  result <- result[!is.na(result$slope), ]
  
  return(result)
}