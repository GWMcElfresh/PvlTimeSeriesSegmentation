#' Generate Sample Viral Load Data with Realistic Regimes
#'
#' Creates synthetic viral load data that represents distinct regimes (stable states)
#' and transitions between them. The data is designed to create clear basins 
#' (periods of quasi-stable viral loads with near-zero slopes) and bridges 
#' (transition periods between stable states).
#'
#' @param n_months Number of months of data to generate (default: 12)
#' @param start_date Starting date for the time series (default: "2020-01-01")
#' @param pattern Pattern type: "two_state", "three_state", "treatment_failure", "suppression_rebound"
#' @param regime_duration_months Average duration of stable regimes in months (default: 4)
#' @param transition_duration_months Duration of transitions between regimes in months (default: 1)
#' @param noise_level Amount of random noise to add (default: 0.05)
#' @return Data frame with date and result columns representing realistic viral load regimes
#' @export
#' @examples
#' # Generate two-state system: high viremia -> suppressed
#' two_state <- generate_sample_data(12, pattern = "two_state")
#' 
#' # Generate three-state system: high -> intermediate -> suppressed
#' three_state <- generate_sample_data(18, pattern = "three_state")
#' 
#' # Generate treatment failure pattern
#' failure <- generate_sample_data(15, pattern = "treatment_failure")
generate_sample_data <- function(n_months = 12, start_date = "2020-01-01", 
                                pattern = "two_state", regime_duration_months = 4,
                                transition_duration_months = 1, noise_level = 0.05) {
  
  dates <- seq(as.Date(start_date), by = "month", length.out = n_months)
  
  if (pattern == "two_state") {
    # Two-state system: High viremia -> Viral suppression
    values <- generate_regime_pattern(
      n_months = n_months,
      regimes = list(
        list(level = 50000, duration = regime_duration_months, stability = 0.95),  # High viremia
        list(level = 40, duration = n_months - regime_duration_months - transition_duration_months, stability = 0.98)   # Suppressed
      ),
      transition_duration = transition_duration_months
    )
    
  } else if (pattern == "three_state") {
    # Three-state system: High -> Intermediate -> Suppressed
    regime1_dur <- floor(n_months * 0.3)
    regime2_dur <- floor(n_months * 0.4) 
    regime3_dur <- n_months - regime1_dur - regime2_dur - 2 * transition_duration_months
    
    values <- generate_regime_pattern(
      n_months = n_months,
      regimes = list(
        list(level = 80000, duration = regime1_dur, stability = 0.94),      # High viremia
        list(level = 5000, duration = regime2_dur, stability = 0.96),       # Intermediate suppression
        list(level = 25, duration = regime3_dur, stability = 0.98)          # Deep suppression
      ),
      transition_duration = transition_duration_months
    )
    
  } else if (pattern == "treatment_failure") {
    # Treatment response followed by failure and recovery
    regime1_dur <- floor(n_months * 0.25)  # Initial high
    regime2_dur <- floor(n_months * 0.4)   # Suppression
    regime3_dur <- floor(n_months * 0.2)   # Rebound/failure
    regime4_dur <- n_months - regime1_dur - regime2_dur - regime3_dur - 3 * transition_duration_months
    
    values <- generate_regime_pattern(
      n_months = n_months,
      regimes = list(
        list(level = 100000, duration = regime1_dur, stability = 0.92),     # Pre-treatment high
        list(level = 50, duration = regime2_dur, stability = 0.97),         # Treatment response
        list(level = 15000, duration = regime3_dur, stability = 0.93),      # Treatment failure
        list(level = 200, duration = regime4_dur, stability = 0.96)         # Rescue therapy
      ),
      transition_duration = transition_duration_months
    )
    
  } else if (pattern == "suppression_rebound") {
    # Suppressed -> Viral rebound -> Re-suppression (common with adherence issues)
    regime1_dur <- floor(n_months * 0.4)   # Suppression
    regime2_dur <- floor(n_months * 0.3)   # Rebound
    regime3_dur <- n_months - regime1_dur - regime2_dur - 2 * transition_duration_months
    
    values <- generate_regime_pattern(
      n_months = n_months,
      regimes = list(
        list(level = 40, duration = regime1_dur, stability = 0.98),         # Suppressed
        list(level = 25000, duration = regime2_dur, stability = 0.94),      # Viral rebound
        list(level = 60, duration = regime3_dur, stability = 0.97)          # Re-suppression
      ),
      transition_duration = transition_duration_months
    )
    
  } else {
    # Legacy patterns for backwards compatibility
    if (pattern == "decline_rebound") {
      values <- c(50000, 25000, 10000, 5000, 1000, 500, 200, 100, 50, 100, 500, 2000)
      if (n_months != 12) {
        values <- approx(1:12, values, n = n_months)$y
      }
    } else if (pattern == "stable_decline") {
      values <- exp(seq(log(50000), log(25), length.out = n_months))
    } else if (pattern == "oscillating") {
      t <- 1:n_months
      trend <- exp(seq(log(10000), log(100), length.out = n_months))
      oscillation <- 1 + 0.3 * sin(2 * pi * t / 6)
      values <- trend * oscillation
    } else {
      stop("Unknown pattern. Choose from: 'two_state', 'three_state', 'treatment_failure', 'suppression_rebound', 'decline_rebound', 'stable_decline', 'oscillating'")
    }
  }
  
  # Add controlled noise while preserving regime structure
  values <- values * (1 + rnorm(n_months, 0, noise_level))
  values[values < 25] <- 25  # Minimum detectable level (clinical cutoff)
  
  data.frame(
    date = dates,
    result = round(values)
  )
}

#' Generate Regime-Based Pattern with Stable States and Transitions
#' 
#' Internal function to create viral load patterns with distinct regimes
#' connected by transition periods. This creates data suitable for identifying
#' basins (stable states) and bridges (transitions) in the segmentation algorithm.
#'
#' @param n_months Total number of months
#' @param regimes List of regime specifications, each with level, duration, and stability
#' @param transition_duration Duration of transitions in months
#' @return Vector of viral load values
generate_regime_pattern <- function(n_months, regimes, transition_duration = 1) {
  values <- numeric(n_months)
  current_month <- 1
  
  for (i in seq_along(regimes)) {
    regime <- regimes[[i]]
    
    # Create stable regime period
    regime_months <- min(regime$duration, n_months - current_month + 1)
    if (regime_months > 0) {
      # Generate stable values with high temporal correlation (quasi-stable)
      stable_values <- generate_stable_regime(regime$level, regime_months, regime$stability)
      end_month <- min(current_month + regime_months - 1, n_months)
      values[current_month:end_month] <- stable_values[1:(end_month - current_month + 1)]
      current_month <- end_month + 1
    }
    
    # Add transition to next regime (if not the last regime)
    if (i < length(regimes) && current_month <= n_months) {
      transition_months <- min(transition_duration, n_months - current_month + 1)
      if (transition_months > 0) {
        next_regime <- regimes[[i + 1]]
        transition_values <- generate_transition(
          from_level = regime$level,
          to_level = next_regime$level,
          duration = transition_months
        )
        end_month <- min(current_month + transition_months - 1, n_months)
        values[current_month:end_month] <- transition_values[1:(end_month - current_month + 1)]
        current_month <- end_month + 1
      }
    }
    
    if (current_month > n_months) break
  }
  
  # Fill any remaining months with the last regime level
  if (current_month <= n_months) {
    last_regime <- regimes[[length(regimes)]]
    remaining_values <- generate_stable_regime(last_regime$level, 
                                             n_months - current_month + 1, 
                                             last_regime$stability)
    values[current_month:n_months] <- remaining_values
  }
  
  return(values)
}

#' Generate Stable Regime Values
#' 
#' Creates viral load values for a stable regime with high temporal correlation
#' to produce near-zero slopes in the interpolated data.
#'
#' @param level Target viral load level
#' @param duration Duration in months  
#' @param stability Stability parameter (0-1), higher means more stable
#' @return Vector of viral load values for the stable regime
generate_stable_regime <- function(level, duration, stability = 0.95) {
  if (duration <= 0) return(numeric(0))
  
  # Generate highly correlated values around the target level
  values <- numeric(duration)
  values[1] <- level * (1 + rnorm(1, 0, 0.05))
  
  # Use AR(1) process with high correlation for stability
  for (i in 2:duration) {
    # High autocorrelation creates near-zero slopes
    values[i] <- stability * values[i-1] + (1 - stability) * level + 
                 level * rnorm(1, 0, 0.02)  # Small innovations
  }
  
  return(pmax(values, 25))  # Ensure minimum detectable level
}

#' Generate Transition Between Regimes
#' 
#' Creates transition values between two regime levels with controlled dynamics
#' to represent biological transitions (treatment response, viral escape, etc.)
#'
#' @param from_level Starting viral load level
#' @param to_level Target viral load level  
#' @param duration Duration of transition in months
#' @return Vector of viral load values for the transition period
generate_transition <- function(from_level, to_level, duration = 1) {
  if (duration <= 0) return(numeric(0))
  if (duration == 1) return((from_level + to_level) / 2)
  
  # Create smooth transition with some variability
  # Use sigmoid-like transition for biological realism
  t <- seq(0, 1, length.out = duration)
  
  if (to_level < from_level) {
    # Declining transition (treatment response)
    sigmoid_t <- 1 / (1 + exp(-8 * (t - 0.5)))  # Steeper decline
  } else {
    # Rising transition (viral rebound) 
    sigmoid_t <- 1 - 1 / (1 + exp(-6 * (t - 0.3)))  # Earlier rise
  }
  
  base_values <- from_level + (to_level - from_level) * sigmoid_t
  
  # Add transition-specific noise (higher during transitions)
  noise_factor <- 0.1 * sin(pi * t) + 0.05  # Higher noise in middle of transition
  values <- base_values * (1 + rnorm(duration, 0, noise_factor))
  
  return(pmax(values, 25))  # Ensure minimum detectable level
}