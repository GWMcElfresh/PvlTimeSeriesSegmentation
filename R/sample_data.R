#' Generate Sample Viral Load Data
#'
#' Creates synthetic viral load data for testing and demonstration purposes.
#'
#' @param n_months Number of months of data to generate (default: 12)
#' @param start_date Starting date for the time series (default: "2020-01-01")
#' @param pattern Pattern type: "decline_rebound", "stable_decline", "oscillating"
#' @return Data frame with date and result columns
#' @export
#' @examples
#' # Generate sample data with decline and rebound pattern
#' sample_data <- generate_sample_data(12, pattern = "decline_rebound")
#' head(sample_data)
generate_sample_data <- function(n_months = 12, start_date = "2020-01-01", pattern = "decline_rebound") {
  dates <- seq(as.Date(start_date), by = "month", length.out = n_months)
  
  if (pattern == "decline_rebound") {
    # Classic HIV viral load pattern: high -> decline -> low -> potential rebound
    values <- c(10000, 5000, 2000, 800, 200, 50, 25, 50, 100, 300, 800, 1500)
    if (n_months != 12) {
      # Interpolate or repeat pattern as needed
      values <- approx(1:12, values, n = n_months)$y
      values[values < 0] <- 25  # Minimum detectable level
    }
  } else if (pattern == "stable_decline") {
    # Steady decline pattern
    values <- exp(seq(log(10000), log(25), length.out = n_months))
  } else if (pattern == "oscillating") {
    # Oscillating pattern with general decline
    t <- 1:n_months
    trend <- exp(seq(log(5000), log(100), length.out = n_months))
    oscillation <- 1 + 0.5 * sin(2 * pi * t / 6)  # 6-month cycle
    values <- trend * oscillation
  } else {
    stop("Unknown pattern. Choose from: 'decline_rebound', 'stable_decline', 'oscillating'")
  }
  
  # Add some random noise
  values <- values * (1 + rnorm(n_months, 0, 0.1))
  values[values < 25] <- 25  # Minimum detectable level
  
  data.frame(
    date = dates,
    result = round(values)
  )
}